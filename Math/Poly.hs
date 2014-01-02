-- |An API for a simple polynomial module, and implementation thereof.
--
--  This implementation is optimized for cases where often only the
--  constant term is ultimately needed.  In particular,
--
--  @
--    'constantTerm' ('fromCoeffs' [a, 'undefined'] \`op\` 'fromCoeffs' [b, 'undefined'])
--  @
--
--  is defined for @op@ any of @+@, @-@, or @*@ and any defined
--  values @a@, @b@, including zero.

module Math.Poly (  
  Poly,
  -- * Basic operations
  idPoly,
  degree,
  evalAt, deriv,
  -- * Conversion
  coeffs, coeffsRaw, fromCoeffs, fromCoeffsRaw, fromCoeffsRare,
  -- * Term extraction
  -- | These are self-explanatory.  @leadingTerm p@ is never @0@ unless @p==0@.
  constantTerm, leadingTerm
  ) where

import Prelude ()
import Math.Base

-- P is not exported, use utility functions instead.
data Poly a = P !a [a] deriving (Eq)

-- Eliminate trailing zeroes, examining as few entries as possible.
canon :: Num a => [a] -> [a]
canon l = case span (==0) l of (_, [])  -> []; (a, c:b) -> a ++ c : canon b

-- This assumes the base type has no zero divisors (but could be changed).
instance Num a => Num (Poly a) where
    P c x + P d y = P (c+d) $ canon (zipWith' (+) x y)
    P c x * P d y = P (if c==0 then 0 else c*d) rest where
      rest = case (null x, null y) of
              (True, _) -> if c==0 then [] else map (c*) y
              (_, True) -> if d==0 then [] else map (d*) x
              _         -> map (c*) y `add` raw x (d:y)
      add = zipWith' (+)
      raw []    _ = []
      raw (a:r) p = (map (a*) p) `add` (0:raw r p)
    negate (P c x) = P (-c) $ map negate x
    abs = id
    signum _ = 1
    fromInteger x = P (fromInteger x) []

-- This functor instance assumes the leading coefficient will not be mapped
-- to zero.  If this cannot be assumed, use `nmap` instead.
instance Functor Poly where
  fmap f = fromCoeffsRaw . map f . coeffsRaw
instance Foldable Poly where
  foldr f v = foldr f v . coeffsRaw

-- A basic show which renders the coefficents as a list.
instance (Num a, Show a) => Show (Poly a) where
  show x = show (coeffs x)

instance Extension Poly where
  unit x = P x []
  project (P x _) = x
  nmap f = fromCoeffs . map f . coeffs

-- |The identity polynomial.  This is useful for creating a \"dummy
--  variable\", e.g.,
-- 
-- @
--   x :: Poly Integer
--   x = idPoly
--   poly1 = (1 - x + x^2) * (3 - 2*x)
-- @
idPoly :: Num a => Poly a
idPoly = fromCoeffs [0,1]

-- |Gives the degree of the polynomial.  @degree 0@ is conventionally @-1@.
degree :: Num a => Poly a -> Int
degree (P 0 []) = -1
degree (P _ l)  = length l

-- |Returns the coefficients of the polynomial as a list, starting with that
--  of the constant term, with no trailing zeroes.  @coeffs 0 == []@.
coeffs :: Num a => Poly a -> [a]
coeffs (P 0 []) = []
coeffs (P c l)  = c : l

-- |Returns the coefficients of a polynomial, as stored internally.  This may
--  differ from @coeffs@.  In this implementation, @coeffsRaw 0 == [0]@.
coeffsRaw :: Poly a -> [a]
coeffsRaw (P c l) = c : l

-- |Builds a polynomial from a list of coefficients.
fromCoeffs :: Num a => [a] -> Poly a
fromCoeffs []    = 0
fromCoeffs (c:l) = P c (canon l)

-- |Builds a polynomial from a list of coefficients.  The list is assumed to
--  be in the same form as given by `coeffs`.  No comparisons are done.
--  In this case, only @fromCoeffsRaw [0] == 0@.
fromCoeffsRaw :: [a] -> Poly a
fromCoeffsRaw []    = error "Invalid representation."
fromCoeffsRaw (c:l) = P c l

-- |Builds a polynomial from a list of coefficients.  The list is assumed to
--  be in \"standard form\", in this case meaning no trailing zeroes.  The
--  case of an empty list will give 0, which provides a small convenience.
fromCoeffsRare :: Num a => [a] -> Poly a
fromCoeffsRare []    = 0
fromCoeffsRare (c:l) = P c l

constantTerm :: Num a => Poly a -> a
constantTerm = project

leadingTerm :: Num a => Poly a -> a
leadingTerm (P x [])    = x
leadingTerm (P _ x)     = last x

-- |Evaluates a polynomial at the given argument.  E.g., @idPoly \`evalAt\` r == r@.
evalAt :: Num a => Poly a -> a -> a
evalAt p x = sum $ zipWith (*) (coeffs p) $ iterate (*x) 1

-- |Compute the derivative of a polynomial.
deriv :: Num a => Poly a -> Poly a
deriv (P _ [])    = 0
deriv (P _ (c:l)) = P c $ zipWith (*) (map fromInteger [2..]) l
