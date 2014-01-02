-- |This module provides an efficient algorithm for determining all possible
--   signs of a sequence of polynomials at the roots of a key polynomial.
--   It accepts any ordered numeric type with a `Reducible` instance for the
--   coefficients.  The underlying Sturm query function is made available.

module Math.SignDet (sturmQuery, signDet) where

import Prelude ()
import Math.Base
import Math.Poly (Poly, degree, leadingTerm, deriv)
import Math.Poly.Extra (rmod)
import Math.Linear (solve, mkMatrix)
import List (transpose, group, sort)

-- | Type of a ternary sequence, by default encoded as an `Integer`.
--   An `Int` is sufficient for any reasonable number of polynomials,
--   so a potential optimization is to change this.
type Ternary = Integer

-- I don't really under understand SPECIALIZE, but maybe this helps.
-- |Compute the Sturm query of two polynomials.
{-# SPECIALIZE sturmQuery :: Poly Integer -> Poly Integer -> Int #-}
sturmQuery :: (Ord a, Reducible a) => Poly a -> Poly a -> Int
sturmQuery x y = loop 0 (sig x) x y where
  sig z = let t = leadingTerm z > 0 in (t, t == even (degree z))
  loop c (s, t) a b =
    let sg@(s', t') = sig a
        c' = c - fromEnum (s /= s') + fromEnum (t /= t')
    in  if b==0 then c' else loop c' sg b (negate (a `rmod` b))

-- Convert to reverse ternary.
ternary :: Ternary -> [Int]
ternary 0 = []
ternary n = let (q, r) = n`divMod`3 in fromInteger r : ternary q

getCols :: Int -> [Ternary] -> [Ternary]
getCols n l = let pn = 3^(n-1) in do x <- l; [x, x+pn, x+2*pn]

getRows :: Int -> [Ternary] -> [Ternary]
getRows _ []  = []
getRows _ [_] = [0]
getRows n l   =
  concatMap pproc . zip [0..] . transpose . group . sort . map (`mod`pw) $ l
    where pw = 3^(n-1); pproc (i, l') = map (+ i*pw) (getRows (n-1) l')

-- Random access to Hadamard matrix.
hadam :: Ternary -> Ternary -> Int
hadam r c = product $ map u $ zip (ternary r) (ternary c)
  where u (1,1) = -1; u (1,2) = 0; u (2,2) = 0; u _ = 1

-- Construct the vector to solve, using polynomials.
getVect :: (Ord a, Reducible a) => (Poly a, [Poly a]) -> Ternary -> Int
getVect (ky, pl) r =
  sturmQuery ky (deriv ky * product [ p^e | (p, e) <- zip pl (ternary r) ])

-- Convert ternary integer to sign string, given length.
ttos :: Int -> Ternary -> String
ttos n = map ("+-0"!!) . take n . (++ repeat 0) . ternary

-- |Compute all possible sign sequences of a set of a polynomials at the roots
--   of the key polynomial.  Return as a list of strings consisting
--   of \'+\', \'-\', and \'0\'.
signDet :: (Ord a, Reducible a) =>
              Poly a {- ^ The key polynomial. -} -> [Poly a] -> [String]
{-# SPECIALIZE signDet :: Poly Integer -> [Poly Integer] -> [String] #-}
signDet key pl = map (ttos (length pl)) $ fst $ foldl iloop ([0], []) pl where
  iloop (hl, ps) po = (res, nps) where
    nps = ps ++ [po]; np = length nps
    cols = getCols np hl; rows = getRows np cols
    mx = [ hadam r c | r <- rows, c <- cols ]
    vec = map (getVect (key, nps)) rows
    ans = solve (mkMatrix (length rows) mx) vec
    res = [ ss | (ss, cnt) <- zip cols ans, cnt>0 ]
