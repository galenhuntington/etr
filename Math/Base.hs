-- | Basic utility and mathematics functions, types, and classes.
--
--   This module re-exports the @Prelude@ with many functions hidden.
--   Thus, it must be used with @import Prelude ()@ or @NoImplicitPrelude@.

module Math.Base (
  -- * Extension class
  Extension(..),
  -- * Divisibility classes
  Divisible(..),
  divides, divM,
  Reducer,
  nonReducer,
  Reducible(..),
  reducePair,
  rlcm,
  -- * Miscellaneous utility functions
  zipWith',
  __,
  choose, combs, combsTo,
  fromJustM, mreturn,
  tweak,
  (^), (?),
  module Prelude,
  module Data.Foldable,
  module Control.Monad
  ) where

-- Hide Foldable instances, less-efficient @^@.
-- There ought to be a better way to do this!  Shadowing?
import qualified Prelude
import Prelude hiding (
  (^), foldl, foldr, foldl1, foldr1, sum, product, maximum, minimum, concat,
  concatMap, and, or, all, any, sequence_, mapM, mapM_, elem, notElem)

-- This (blech!) is needed because ghc 6.10.1 disables the automatic
-- use of this faster GCD, and the performance difference is enormous.
import GHC.Integer (gcdInteger)

import Data.Maybe
import Data.Foldable
import Data.Ratio
import Control.Monad (MonadPlus, mzero, guard, join)

-- | Numeric version of a pointed functor (or premonad), with the ability to
--   project down to boot.
class Foldable x => Extension x where
  unit :: Num a => a -> x a
  project :: Num a => x a -> a
  nmap :: (Num a, Num b) => (a -> b) -> x a -> x b


-- | A general notion of a type where one can sometimes do exact division.
--   A monadic (really `Maybe`) version returns `Nothing` if
--   the first argument does not divide the second or @return@s the divisor.
--   An \"unsafe\" version assumes it's an exact divisor and returns it, with
--   behavior undefined if not an exact factor.  (This is an instance of a
--   general notion of
--   a partial function where testing for failure might be expensive.)
--
--   The monadic version originally used the NotJustMaybe idea, but
--   following recent trends I respecialized it.  However, with the
--   utility functions below, we can easily simulate the general behavior.
--   For instance, the following lists all pairs of positive integers whose product
--   is 120:
--
-- @
--    [ (x, y) | x <- [1..120], y <- mreturn $ 120 \`divM\` x ]
-- @
--
--   Minimal complete definition: @divoutM@.
class Num z => Divisible z where
  divoutM :: z -> z -> Maybe z
  divout_ :: z -> z -> z
  divout_ x = fromJust . divoutM x

instance Divisible Integer where
  divoutM x y = case divMod y x of
    (q, 0) -> return q
    _      -> Nothing
  divout_ x y = y `div` x

-- I never use these.
instance (Integral x) => Divisible (Ratio x) where divoutM = (return .) . (/)
instance Divisible Float where divoutM = (return .) . (/)
instance Divisible Double where divoutM = (return .) . (/)

-- | Test for divisibility.
divides :: Divisible z => z -> z -> Bool
divides x = isJust . divoutM x

-- | Simple utility function providing a more intuitive order for division.
divM :: Divisible z => z -> z -> Maybe z
divM = flip divoutM


-- | A reducer takes a @b@ and an @a@ and returns @a@ reduced towards
--   1 \"as much as possible\" where @b@ is reduced equally.  This is used to
--   handle pseudodivision of polynomial coefficients in a general way.
--   Where sign is meaningful, @a@ is always to be returned positive.
type Reducer a = a -> a -> (a, a)

-- | The most basic reducer, which doesn't reduce.
nonReducer :: Reducer a
nonReducer = (,)

-- | This class defines types with a standard reducer, as well as a
--   corresponding recursive GCD, which should give the same answers
--   as dividing via the reducer, up to a sign.
--
--   Examples:
-- 
-- @
--    (6 :: Integer) \`reduce2\` 8 == (3, 4)
--    (6 :: Rational) \`reduce2\` 8 == (1, 4%3)
-- @
class Divisible a => Reducible a where
  reduce2 :: Reducer a
  reduce2 a b = let g = a`rgcd`b in (g`divout_`a, g`divout_`b)
  rgcd :: a -> a -> a
  rgcd a b = (fst $ a `reduce2` b) `divout_` a

-- All this crap is needed for ghc 6.10.1.
gcdInteger' :: Integer -> Integer -> Integer
gcdInteger' 0 0 = error "gcd 0 0"
gcdInteger' a b = gcdInteger a b
{-# INLINE [1] gcd' #-}
{-# RULES "gcd'/Integer->Integer->Integer" gcd' = gcdInteger' #-}
gcd' :: Integral a => a -> a -> a
gcd' = gcd

instance Reducible Integer where
  rgcd = gcd'
  reduce2 a b = (g`divout_`a, g`divout_`b)
    where g = (if a<0 then negate else id) $ a `gcd'` b
instance Integral a => Reducible (Ratio a) where reduce2 a b = (1, b/a); rgcd a _ = a
instance Reducible Float where reduce2 a b = (1, b/a); rgcd a _ = a
instance Reducible Double where reduce2 a b = (1, b/a); rgcd a _ = a

-- | Convenience function, since I vacillated whether to make the
--   reducer curried or not.
reducePair :: Reducible a => (a, a) -> (a, a)
reducePair = uncurry reduce2

-- | Counterpart to rgcd.  Since this is not a class method, it cannot
--   be overloaded to customize sign issues, a possible weakness.
rlcm :: Reducible a => a -> a -> a
rlcm a b = divout_ (rgcd a b) (a*b)

-- | Zip with a function until one list runs out, then append
--   the remaining list unchanged.
--   E.g., @zipWith' (+) [1,2] [10,20,30] == [11,22,30]@.
zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' f = z where
    z (a:as) (b:bs) = f a b : z as bs
    z a [] = a; z [] b = b

-- A way to ameliorate bulky @otherwise@s; empty guards would be preferable.
__ :: Bool
__ = True
-- o'w = True -- another idea

-- | Binomial coefficient.
choose :: Integral a => a -> a -> a
choose n k =
  case min k (n-k) of
    -1 -> 0
    i  -> product [n-i+1..n] `div` product [1..i]

-- | All k-size combinations from a list.
combs :: Int -> [a] -> [[a]]
combs 0 _  = [[]]
combs _ [] = []
combs k (h:t) = map (h:) (combs (k-1) t) ++ combs k t

-- | All <=k-size combinations from list.
combsTo :: Int -> [a] -> [[a]]
combsTo (-1) _ = []; combsTo _ [] = [[]]
combsTo k (h:t) = combsTo k t ++ map (h:) (combsTo (k-1) t)

-- | More efficient, less general version of @^@.
(^) :: Num a => a -> Int -> a
x ^ 2 = x*x
x ^ 3 = x*x*x
x ^ y = x Prelude.^ y

-- | Also called @if'@.
(?) :: Bool -> a -> a -> a
(?) x y z = if x then y else z

-- | Returns a list with the nth adjusted by the second argument.
--   Extends the list with zeroes if needed.
tweak :: Num t => Int -> t -> [t] -> [t]
tweak _ 0 l = l
tweak n d l = tw n l where
  tw 0 (h:t) = h+d : t
  tw m (h:t) = h : tw (m-1) t
  tw m []    = replicate m 0 ++ [d]

-- | This converts a @Maybe@ to a general monad.  The trend
--   seems to be away from \"NotJustMaybe\", thus making this
--   a useful utility function.
fromJustM :: Monad m => Maybe a -> m a
fromJustM = maybe (error "Nothing converted to non-complying monad.") return

-- | If we're going to have both @Monad@ and @MonadPlus@, we
--   might as well have a specialized version.
mreturn :: MonadPlus m => Maybe a -> m a
mreturn = maybe mzero return
