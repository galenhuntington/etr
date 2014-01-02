-- | A few simple linear algebra functions, basically just algorithms
--   needed for this project, and so not terribly general.  The emphasis is
--   on taming division.

{-# LANGUAGE ParallelListComp #-}

module Math.Linear (
    Matrix, mkMatrix, det, polyDet, subres, charPoly, orthoBasis, solve
  ) where

import Prelude ()
import Math.Base
import Data.Array
import Data.Ratio
import Math.Poly
-- Need instances.
import Math.Poly.Extra ()

-- | This library uses the Haskell '98  @Array@ type.  Another could
--   be substituted, but for the algorithms here the immutable array
--   operations are not noticeably slower or more memory-inefficient,
--   as the arithmetical operations dwarf any differences.
type Matrix a = Array (Int, Int) a

-- | Division-free algorithm for computing the characteristic polynomial
--   in O(n^4) time, based on Rote (2001).  It can be used to find the
--   determinant in the absence of a @Divisible@ instance.
charPoly :: Num a => Matrix a -> Poly a
charPoly mx = fromCoeffs $ fst $ layer n where
  n = (fst $ snd $ bounds mx) + 1 :: Int
  shel pn ar = (ar!(n,n) : pn, ar)
  layer 0 = shel [] $ mkMatrix (n+1) [ if r==c then 1 else 0 | r <- [0..n], c <- [0..n] ]
  layer l = let (pn, pl) = layer (l-1) in shel pn $ array ((0,0), (n,n)) $
    [ ((r, c), sum [ mx!(x, r)*pl!(x, c) | x <- [c..n-1] ])
      | r <- [1..n-1], c <- [0..r-1] ] ++
    [ ((c1, c1), -tot) | c1 <- [0..n]
      | tot <- scanl (+) 0 [ sum [ mx!(r,c)*pl!(r,c) | r <- [c..n-1] ] | c <- [0..n-1] ] ]

-- | Compute determinant using only exact divisions.
det :: Divisible a => Matrix a -> a
det mx0 = if sz<0 then 1 else loop sz 1 mx0 where
  sz = fst $ snd $ bounds mx0
  loop n lpv mx = case [ r |  r <- [0..n], mx!(r, n) /= 0 ] of
    [] -> 0
    k:_ -> let pv = mx!(k, n) in
      if n==0 then pv
      else loop (n-1) pv $ mkMatrix n
          [ lpv `divout_` (mx!(r, c)*pv-mx!(k, c)*v) |
            r <- [0..n], r/= k, let v = mx!(r, n), c <- [0..n-1] ]

-- | Compute the polynomial determinant, using only exact divisions.
--   This uses the interpretation that the matrix is a list of
--   polynomials.
polyDet :: (Divisible a) => [Poly a] -> Poly a
polyDet ps = det $ mkMatrix (m+1) $ concat rows where
    m = length ps - 1; n = maximum (map degree ps)
    rows = [ replicate (m - length t) 0 ++ map unit t ++ [p]
              | p <- ps, let t = reverse (drop (n-m+1) (coeffs p)) ]

-- | Compute the kth subresultant of two polynomials using the
--   polynomial determinant.
subres :: (Divisible a) => Int -> Poly a -> Poly a -> Poly a
subres k p q = polyDet $ up p q ++ up q p where
    up a b = let d = degree b in [ nud i a | i <- [0..d-k-1] ]
    nud i r = fromCoeffs (replicate i 0 ++ coeffs r)

-- | Simple function to solve a linear equation with `Integral` elements,
--   by casting to a Ratio type.
solve :: (Integral a) => Matrix a -> [a] -> [a]
solve mx0 vec = loop sz vmx where
  sz = fst $ snd $ bounds mx0
  vmx = listArray ((0,-1), (sz,sz))
    [ fromIntegral $ if -1==c then vec!!r else mx0!(r, c) | r <- [0..sz], c <- [-1..sz] ]
  loop n mx = if n==0 then backsolve nmx else loop (n-1) nmx where
    k = head [ r | r <- [0..n], mx!(r,n) /= 0 ]; pv = mx!(k, n)
    nmx = dorows n $ mx // do c <- [-1..n]; [ ((k, c), mx!(n, c)), ((n, c), mx!(k, c)/pv) ]
  dorows n mx =
    mx // [ ((r, c), mx!(r, c)-mx!(n, c)*v) | r <- [0..n-1], let v = mx!(r, n), c <- [-1..n-1] ]
  backsolve nmx = map numerator x where
    x = [ nmx!(r, -1) - sum [ nmx!(r, i)*x!!i | i <- [0..r-1] ] | r <- [0..sz] ]

-- | Compute an orthogonal basis for a vector.  The first element is assumed to
--   be nonzero; it would be simple to remove this restriction, but I do not need to.
orthoBasis :: Num a => [a] -> [[a]]
orthoBasis (vh:vt) = zipWith vone [1..] vt where
  vone k e = -e : [ if k==i then vh else 0 | i <- [1..length vt] ]
  -- n = length vt

-- | Build a square matrix of the given size @n@ from a list of length @n^2@.
--   It will be @(0,0)@-based.
mkMatrix :: Int -> [a] -> Matrix a
mkMatrix n = listArray ((0, 0), (n-1, n-1))
