-- | Less basic functions involving univariate division, especially
--   those needed for the algorithms under consideration.

module Math.Poly.Extra (
  -- * Division
  -- | Division is a tricky business.  Two main division functions are provided,
  --   which offer different strategies.  Both are still quite general.
  genDivisionBy, genDivision, exactDivideBy, exactDivide,
  -- * Remainder sequences
  remSeqWith, remSeq, remSeqCollins,
  content, squareFree, primPart, divout, rmod,
  pTranspose,
  divoutIds,
  xmod
  ) where

import Prelude ()
import Math.Base
import Math.Poly

import Data.Maybe (fromJust)

-- |General division with remainder and all factors introduced
--  during pseudofactoring.  It accepts a pseudofactoring function
--  for processing the coefficients.
--  A substantial number of special cases of this function could be
--  quite reasonably defined, but here there are but two.
genDivisionBy :: (Eq a, Num a) => Reducer a -> Poly a -> Poly a -> ((Poly a, Poly a), a)
genDivisionBy pd a' b' = psx [] 1 (degree a' - degree b') a where
  a = reverse (coeffs a'); bh:bt = reverse (coeffs b'); pdl = pd bh
  psx q xs d r@(~(h:t))
    | d<0  = ((fromCoeffsRare q, fromCoeffs (reverse r)), xs)
    | h==0 = psx (0:q) xs (d-1) t
    | __   = psx q' (xs*r0) (d-1) rest where
               (r0, q0) = pdl h
               rest = zipWith (\x y -> r0*x - y) t $ map (q0*) bt ++ repeat 0
               q' = q0 : if r0==1 then q else map (r0*) q -- a helpful optimization

-- | The above using a `Reducible` instance.
genDivision :: (Eq a, Reducible a) => Poly a -> Poly a -> ((Poly a, Poly a), a)
genDivision = genDivisionBy reduce2

-- | General exact division.  Results are undefined if it is not in fact an
--   exact division.  The first argument is a coefficient factoring function,
--   which should satisfy @f a b * a == b@.  This function examines
--   coefficients starting with the constant term and going up.
exactDivideBy :: (Eq a, Num a) => (a -> a -> a) -> Poly a -> Poly a -> Poly a
exactDivideBy fct a b = fromCoeffs $ trim (coeffs a) (coeffs b) where
  trim (0:x) (0:y) = trim x y
  trim x (c:qs) = qu x where
    qu []     = []
    qu (t:ts) = cdt : qu rest
      where cdt = c `fct` t
            rest = zipWith (\r q -> r - cdt*q) ts $ qs ++ repeat 0

-- | The above using a 'Divisible' instance.
exactDivide :: (Eq a, Divisible a) => Poly a -> Poly a -> Poly a
exactDivide = exactDivideBy divout_

--  These are orphan instances, but I don't see a way around that
--  without breaking the module structure.
instance (Eq a, Divisible a) => Divisible (Poly a) where
  divout_ = flip exactDivide
  -- This is a hack, as genDivision offers no way to thread failure
  -- through, and generalizing it in this way seems excessive...
  -- so I just blot with zeroes.
  -- This calls into question this entire strategy.
  -- I never need this function, however.
  divoutM a b =
    case genDivisionBy zig b a of
      ((x, 0), 1) -> Just x
      _           -> Nothing
    where zig x y = maybe (0, 0) ((,) 1) $ divoutM y x

instance (Eq a, Reducible a) => Reducible (Poly a) where
  rgcd x y = nmap ((cx`rgcd`cy)*) . last $ (reduceBy cx x) `remSeq` (reduceBy cy y)
    where cx = content x; cy = content y
          reduceBy v p = nmap (divout_ v) p

-- | Compute the content of a structure, that is, the rgcd of its
--   component terms.  The signature is quite general and covers
--   polynomials and any `Extension`.  However, ask for @content 0@
--   at your own risk (it should give zero).
content :: (Eq a, Reducible a, Foldable x) => x a -> a
content l = foldl rgcd' 0 l where
  rgcd' 0 x = x; rgcd' x y = rgcd x y

-- | Remove the content.
primPart :: (Eq a, Reducible a, Extension x) => x a -> x a
primPart x = nmap (divout_ (content x)) x

-- | Guaranteed to throw an error if division fails.
divout :: Reducible a => a -> a -> a
divout x y = fromJust $ divoutM x y

-- | Recursive modulus.
rmod :: (Eq a, Reducible a) => Poly a -> Poly a -> Poly a
rmod x y = snd . fst $ genDivision x y

-- | Compute the polynomial remainder sequence with given reducer,
--   using a post-processing function at each step.
remSeqWith ::
  (Eq a, Num a) => Reducer a -> (Poly a -> Poly a) -> Poly a -> Poly a -> [Poly a]
remSeqWith rd f x y = s where
  s = x : takeWhile (/=0) (y : [ f $ a `pmod` b | (a, b) <- zip s (tail s) ])
  pmod a b = snd . fst $ genDivisionBy rd a b

-- | Remainder sequence where elements are kept content-free.
--   Uses the `Reducible` instance, used in `Reducible` instance.
remSeq :: (Eq a, Reducible a) => Poly a -> Poly a -> [Poly a]
remSeq = remSeqWith reduce2 primPart

-- | Remainder sequence using Collins' reduction.
remSeqCollins :: (Eq a, Divisible a) => Poly a -> Poly a -> [Poly a]
remSeqCollins x y = let l@(_:t) = x : y : loop l 1 in x : takeWhile (/=0) t where
  loop (_:0:_)     _  = []
  loop (a:r@(b:_)) me = nmap (divout_ me) p : loop r me'
    where ((_, p), me') = genDivisionBy nonReducer a b

-- | Eliminate redundant factors.  (It also reduces as a side effect.)
squareFree :: (Eq a, Reducible a) => Poly a -> Poly a
squareFree 0 = 0
squareFree x = divout_ (x `rgcd` deriv x) x

-- | Transpose two variables of a bivariate polynomial represented
--   as a @Poly . Poly@.
pTranspose :: (Eq a, Num a) => Poly (Poly a) -> Poly (Poly a)
pTranspose bp =
  fromCoeffs $ map fromCoeffs $ peel (dg+1) $ map coeffs lst where
    lst = coeffs bp
    dg = maximum [ degree p | p <- lst ]
    peel 0 _ = []
    peel n z = a : peel (n-1) b where
      (a, b) = unzip (map foo z)
      foo [] = (0, []); foo (c:m) = (c, m)

-- | Divide out all factors of the identity, shifting all coefficients
--   down till the constant term is nonzero.  Useful.
divoutIds :: (Eq a, Num a) => Poly a -> Poly a
divoutIds = fromCoeffs . dropWhile (==0) . coeffs

-- If I knew haskell better, i might be able to generalize with Traversable.
-- In my defense, it uses circular programming....
-- | Pseudo-modulo each coefficient by some polynomial, as though it
--   were zero, collecting factors to not affect the polynomial's value
--   in the quotient ring.  Used for Canny's proposed degree reduction.
xmod :: (Eq a, Reducible a) => Poly a -> Poly (Poly a) -> Poly (Poly a)
xmod u pp = fromCoeffs pr where
  (pr, pf) = unzip $ do
    t <- coeffs pp
    let ((_, r), f) = t `genDivision` u
    return (nmap ((f`divout_`ff)*) r, f)
  ff = foldl1 rlcm pf
