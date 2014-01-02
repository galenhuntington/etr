-- | Simple API for multivariate polynomials.

module Math.Poly.Multi (
    MPoly, terms, x_, totalDegree, monomial, pderiv, lderiv,
    evalAts, linMonoMap, numVars
  ) where

import Prelude ()
import Math.Base
import Data.Monoid
import Control.Arrow (first)


type Mono = [Int]
-- Named instances perhaps?
newtype Mono' = M Mono deriving (Eq)
type RawPoly a = [(a, Mono)]
newtype MPoly a = P { terms :: RawPoly a } deriving (Eq)

-- This is wrapped in M instead of being a type just so this ordering
-- can be customized, which I can't think of any reason to do.
instance Ord Mono' where
  compare (M a) (M b) = (sum a `compare` sum b) `mappend` (a `compare` b)

instance (Eq a, Num a) => Num (MPoly a) where
  P x + P y = P $ gen_add 1 x y
  P x * P y = P $ foldl (gen_add 1) []
    [ [ (cx*cy, mx`mmul`my) | (cy, my) <- y ] | (cx, mx) <- x ]
  negate (P x) = P $ gen_add (-1) [] x
  P x - P y = P $ gen_add (-1) x y
  abs (P x) = P x; signum _ = 1
  fromInteger = unit . fromInteger

instance (Show a, Num a) => Show (MPoly a) where
  show (P x) = show x

instance Extension MPoly where
  unit 0 = P[]; unit x = P[(x, [])]
  project (P x) = maybe 0 fst $ find (null . snd) x
  nmap f (P x) = P . filternz $ map (first f) x

instance Foldable MPoly where
  foldr f v = foldr f v . map fst . terms

filternz :: (Eq a, Num a) => RawPoly a -> RawPoly a
filternz = filter ((/=0).fst)

mmul :: Mono -> Mono -> Mono
mmul = zipWith' (+)

-- There may be a more efficient way to do this, but efficiency
-- is not hugely important here.
gen_add :: (Eq a, Num a) => a -> RawPoly a -> RawPoly a -> RawPoly a
gen_add f = ga where
  ga x [] = x
  ga [] x = map (first (f*)) x
  ga z@(b@(c1,t1):x1) w@((c2,t2):x2) =
    case M t1 `compare` M t2 of
      GT -> b : ga x1 w
      LT -> (f*c2, t2) : ga z x2 
      EQ -> case c1+f*c2 of 0 -> ga x1 x2; n -> (n, t1) : ga x1 x2

-- | Build a raw monomial from coefficient and exponent sequence.
-- 	 It could be defined as
--
-- 	 @
-- 	    monomial c es = c * product [ x_ i ^ e | (i, e) <- zip [0..] es ]
-- 	 @
--
-- 	 but is more efficient.
-- 	 It is something like an opposite of `terms`.  Trailing zeros are
-- 	 eliminated.
monomial :: Num a => a -> [Int] -> MPoly a
monomial c es = P [(c, dz es)] where
  dz l = case span (==0) l of (_, []) -> []; (x1, x:xs) -> x1++x:dz xs

-- | Simple utility function to give the @n@th variable as a polynomial.  This
--   type is overloaded, so it is useful to have utility functions such as
--
-- @
--    x0 :: MPoly Integer
--    x0 = x_ 0
-- @
x_ :: (Num a) => Int -> MPoly a
x_ i = 1 `monomial` (replicate i 0 ++ [1])

-- | The total degree of the polynomial is the maximum total degree of its
--    constituent monomial;
--    the total degree of a monomial is the sum of the degrees of its component
--    variables.
totalDegree :: (Num a) => MPoly a ->  Int
totalDegree (P x) = maximum $ 0 : map (sum.snd) x

-- | A general map which is linear in monomials.  It takes a coercion
-- 	 function and a function
--   from exponent sequences to a numeric type and returns a function
--   from polynomials to whatever.
linMonoMap :: (Num a, Num b) => (a -> b) -> ([Int] -> b) -> MPoly a -> b
linMonoMap u f (P p) = sum [ u c * f e | (c, e) <- p ]

-- | Partial derivative.
pderiv :: (Eq a, Num a) => Int -> MPoly a -> MPoly a
pderiv n = linMonoMap unit $ \es ->
  case splitAt n es of
    (_, [])    -> 0; (_, 0:_)   -> 0
    (e1, x:e2) -> fromIntegral x `monomial` (e1++(x-1):e2)

-- | Linear derivative, in a given direction.
lderiv :: (Eq a, Num a) => (Int, MPoly a) -> [a] -> MPoly a
lderiv (n, p) = let ds = map (flip pderiv p) [0..n-1] in
  \ v -> sum [ unit e * d | (e, d) <- zip v ds {-, e/=0 -} ]

-- This could be changed to use linMonoMap.
-- | Substitute a list of values into the variables.  The second argument may be
--   infinite, but a failure will result if it has insufficient variables.
evalAts :: Num a => MPoly a -> [a] -> a
evalAts (P ms) ps =
    sum [ c * product [ w!!p | (w, p) <- zip powers vec ] | (c, vec) <- ms ]
  where powers = map (\p -> iterate (p*) 1) ps


-- | Simple utility function gives one more than the index of the largest
--   variable in a polynomial.
numVars :: MPoly a -> Int
numVars = maximum . map (length . snd) . terms

