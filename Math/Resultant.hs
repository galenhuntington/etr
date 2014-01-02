-- | This provides a generalization of the usual resultant to
--   several polynomials in several variables.  An instance of
--   this is the very useful u-resultant.

{-# LANGUAGE ParallelListComp #-}

module Math.Resultant (
  mResultant, resMatrices, prepareGCP, uSuppl, uSuppl', homog, homogSys,
  MSystem, mkMSystem) where

import Prelude ()
import Math.Base

import Control.Arrow ((***))
import Data.Array

import Math.Linear
import Math.Poly
import Math.Poly.Multi

-- Simple utility functions.

-- This presumes in highest-sum-first order.
hmp_deg :: MPoly a -> Int
hmp_deg p = head $ [ sum h | (_, h) <- terms p ] ++ [-1]

tot_deg :: [Int] -> Int
tot_deg dsig = sum dsig - length dsig + 1

ptnum_ :: Int -> Int -> [Int] -> Int
ptnum_ _   _   []    = 0
ptnum_ tot len (x:l) = (tn+len-2) `choose` (len-1) + ptnum_ tn (len-1) l
  where tn = tot - x

ptnum :: [Int] -> Int
ptnum l = ptnum_ (sum l) (length l) l

pttot_ :: Integral a => a -> a -> a
pttot_ l t = (t+l-1) `choose` t

hlist :: [Int] -> [[[Int]]]
hlist ds = [ reverse (hl (td-d) ds k) | (k, d) <- zip [0..] ds ] where
  td = tot_deg ds
  hl :: Int -> [Int] -> Int -> [[Int]]
  hl 0 []  _ = [[]]
  hl _ []  _ = []
  hl t [_] _ = [[t]] -- redundant but efficient
  hl t (lh:lt) k =
    [ i:x | i <- [0..if k>0 then lh-1 else t], x <- hl (t-i) lt (k-1) ]

-- | Direct access to the utility function that computes the matrices
--   before the resultant is found.
resMatrices :: Num a => [MPoly a] -> (Matrix a, Matrix a)
resMatrices ps = (bigm, subm) where
  dsig = map hmp_deg ps; n = length dsig; td = tot_deg dsig; hl = hlist dsig;
  entries = concat [ [ ((r, ptnum $ zipWith' (+) v ls), x) | (x, v) <- terms p ]
                      | (p, h) <- zip ps hl, ls <- h | r <- [0..] ]
  hqual ls i = or [ e<=pd | (e, pd) <- drop (i+1) (zip dsig ls) ]
  bigm = mkMatrix (pttot_ n td) (repeat 0) // entries
  cols = [ ptnum (tweak i pd ls) | (i, h, pd) <- zip3 [0..] hl dsig, ls <- h, hqual ls i ]
  rows = [ r | (r, True) <- [ (r, hqual ls i) | (h, i) <- zip hl [0..], ls <- h | r <- [0..] ] ]
  subm = mkMatrix (length cols) [ bigm ! (r, c) | r <- rows, c <- cols ]

-- | Compute the multivariate resultant of a set of polynomials, using
--   the basic algorithm.  Uses a `Maybe` instance for the case @0/0@.
mResultant :: (Eq a, Num a, Divisible a) => [MPoly a] -> Maybe a
mResultant ps =
  let (a, b) = det *** det $ resMatrices ps in
    case b of 0 -> Nothing; _ -> Just $ b `divout_` a

-- This is less general than the real u-resultant, so maybe it
-- should be named thus.
-- | Supplement a set with a u-variable, to prepare for u-resultant.
uSuppl :: (Eq a, Num a) => [a] -> MSystem a -> [MPoly (Poly a)]
uSuppl cv (nv, pl) = map (nmap unit) pl ++ [ueq] where
  ueq = unit idPoly * x_ 0 - sum [ unit (unit g) * x_ i | (i, g) <- zip [1..nv-1] cv ]

-- | This supplements indirectly based on Minimair (2006).  Note that it
--   does produce spurious factors of `idPoly`, which will have to be removed.
--   So far simply using `divoutIds` has not caused any problems.
uSuppl' :: (Eq a, Num a) => [a] -> MSystem a -> [MPoly (Poly a)]
uSuppl' cv (_, pl) = map (flip evalAts vs) (map (nmap unit . nmap unit) pl) where
  vs = (negate.sum) [ unit (unit c) * x_ i | (i, c) <- zip [0..] cv ]
          : [ unit idPoly * x_ i | i <- [0..] ]

-- | Used to convert to \"generalized characteristic polynomial\" (Canny 1990).
prepareGCP :: (Eq a, Num a) => [MPoly a] -> [MPoly (Poly a)]
prepareGCP ps =
  [ nmap unit p + unit idPoly * x_ i^(hmp_deg p) | (p, i) <- zip ps [0..] ]

-- | This allows easy passing around of the number of variables.
type MSystem a = (Int, [MPoly a])

-- | Turn a mere list of multivariate polynomials into a system(TM).
mkMSystem :: [MPoly a] -> MSystem a
mkMSystem l = (maximum $ 0 : map numVars l, l)

-- | Homogenize a polynomial by making all terms have equal total degree.
--   This is done by adding a new variable \"at the beginning\", which
--   (possibly counterintuitively) shifts all the variables over by one.
homog :: (Eq a, Num a) => MPoly a -> MPoly a
homog p = linMonoMap unit (\x -> 1 `monomial` (tdeg - sum x : x)) p
  where tdeg = totalDegree p

homogSys :: (Eq a, Num a) => MSystem a -> MSystem a
homogSys (nv, mps) = (nv+1, map homog mps)

