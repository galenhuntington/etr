-- | These are functions for solving the existential theory of the reals.
--   This is mostly based on the strategies of Canny.

{-# LANGUAGE NamedFieldPuns, NoMonomorphismRestriction #-}

module Math.ETR (
    perturb, mkTube, uRes, uResGCP, varRep, varReps, qPlug,
    -- * Walk-through
    -- | The following functions provide an easy interface to the
    -- algorithm, with some cost in flexibility.
    UPack(..), prepSimpETR, etrSigns, etrSignsAll
  ) where

import Prelude ()
import Math.Base
import Control.Arrow
import Data.Maybe

import Math.Linear
import Math.Poly
import Math.Poly.Multi
import Math.Poly.Extra
import Math.Resultant
import Math.SignDet

-- | Perturb using your favorite infinity and generic a-vector.
perturb :: (Eq a, Num a) => a -> [a] -> (Int, [MPoly a]) -> (Int, [MPoly a])
perturb bigm av = second $ \ps -> [ unit bigm*p - unit a | (p, a) <- zip ps av ]

-- | To test if a subset has a solution, we pick a generic
--   g-vector and a set of indices representing that subset, and create
--   a \"tube\".
mkTube :: (Eq a, Reducible a) => [a] -> [Int] -> MSystem a -> MSystem (Poly a)
mkTube gv is (n, ps) =
    (n, nmap unit tot - unit idPoly : map (nmap unit . primPart . myd . (bas!!)) [0..n-2])
  where tot = primPart $ sum [ (ps!!i)^2 | i <- is ]
        myd = lderiv (n, tot); bas = orthoBasis gv

-- I feel this could be better.
reduceBy _ (0, 0) = (0, 0)
reduceBy u (a, b) =
  let rd = divout_ (a `rgcd` b); (ra, rb) = (rd a, rd b)
      (((_, a1), a2), ((_, b1), b2)) = (ra `genDivision` u, rb `genDivision` u)
      gt = nmap $ divout $ (b2 * content a1) `rgcd` (a2 * content b1)
  in (gt (a1 * unit b2), gt (b1 * unit a2))

-- Abstract common root functions as drop-in replacements.
-- The monomorphism restriction is disabled to save me typing
-- ungodly type signatures.  Hence, room for improvement.
croot f p1 p2 =
  case coeffs `fmap` f p1 p2 of
    Just (n:d:_) | d/=0 -> Just (n `reduce2` (-d))
    _                   -> Nothing
linFinder f x y = find ((==1).degree) $ f x y
crootCollins = croot (linFinder remSeqCollins)
crootPolyDet = croot ((Just .) . subres 1)
crootCanny res = croot (linFinder (remSeqWith reduce2 (primPart . xmod res)))
crootPrimPart = croot (linFinder (remSeqWith (,) primPart))

-- Ad hoc abstraction.
uResWith us f1 f2 eqs cv =
  fmap (squareFree . primPart . divoutIds . project . divoutIds . f1)
    . mResultant . f2 . map (nmap pTranspose) . us (map unit cv) $ eqs

-- | Get the u-resultant of the system.
uRes :: (Eq a, Reducible a) => MSystem (Poly a) -> [a] -> Maybe (Poly a)
uRes = uResWith uSuppl' id id

-- | Via Canny's generalized characteristic polynomial.
uResGCP :: (Eq a, Reducible a) => MSystem (Poly a) -> [a] -> Maybe (Poly a)
uResGCP = uResWith uSuppl (project . divoutIds) prepareGCP

-- | Compute variable representations, threaded through the @Maybe@
--   monad to catch degenerate cases.
varRep :: (Eq a, Reducible a) => MSystem (Poly a) -> Poly a -> [a] -> Int
                                  -> Maybe (Poly a, Poly a)
varRep eqs res cv i = do
  [ps, ns] <- sequence [ uRes eqs $ tweak i x cv | x <- [1, -1] ]
  guard $ ps/=0 && ns/=0
  let rz = nmap unit
  let ps' = (rz.rz) ps `evalAt` (2*unit idPoly - idPoly)
      ns' = rz ns
  (n', d) <- crootCollins ns' ps'
  let n = (idPoly * d) - n'
  return $ reduceBy res (n, d)

-- | Compute all variable representations at once into a list,
--   again via the @Maybe@ monad.
varReps :: (Eq a, Reducible a) => MSystem (Poly a) -> Poly a -> [a] -> Maybe [(Poly a, Poly a)]
varReps eqs@(nv, _) res cv = sequence [ varRep eqs res cv i | i <- [0..nv-2] ]

-- | Plug a sequence of variable representations into a multivariate polynomial,
--   multiplying through by an even power of the denominator to
--   eliminate denominators without changing the sign.
qPlug :: (Eq a, Num a) => [(Poly a, Poly a)] -> MPoly a -> Poly a
qPlug ups mp =
  let pows   = [ let l x = iterate (x*) 1 in (l n, l d) | (n, d) <- ups ]
      maxexp = map bump $ foldl1 (zipWith' max) [ e | (_, e) <- terms mp ]
      bump x = if odd x then x+1 else x
  in  sum [ unit c *
              product [ (ns!!p) * (ds!!(me-p))
                | ((ns, ds), me, p) <- zip3 pows maxexp $ e ++ repeat 0 ]
            | (c, e) <- terms mp ]

-- | A @UPack@ contains all information needed to iterate through
--   possible subsets.  @isets@ gives a list of lists of indices
--   into the subset.
data UPack a = UPack {
  orig :: [MPoly a],    -- ^ Original equation set.
  msys :: MSystem a,    -- ^ Perturbed system.
  gvec :: [a],          -- ^ Generic vector (line).
  isets :: [[Int]]      -- ^ Index subsets to iterate through.
  } deriving (Eq, Show)

-- | Prepare the set of equations for further processing.  This
--   creates a `UPack` structure which contains all information needed
--   to build variables representations.
prepSimpETR ::
  (Eq a, Num a) =>
  [MPoly a] -- ^ Original set of equations.
  -> a      -- ^ The number to use as infinity.
  -> [a]    -- ^ The a-vector, used for perturbation
  -> [a]    -- ^ The g-vector, or generic line.
  -> UPack a
prepSimpETR orig inf av gvec =
  UPack { orig, msys, gvec, isets = tail $ combsTo nv [0..length orig-1] }
    where msys@(nv, _) = perturb inf av $ mkMSystem orig

-- | For each @iset@, get a list of achievable sign sequences.
--   Needs a constant vector to make it happen.  Threads through `Maybe`
--   monad in case of failure.
etrSigns :: (Ord a, Reducible a) => UPack a -> [Int] -> [a] -> Maybe [String]
etrSigns upk is cv = do
  let feed = homogSys $ mkTube (gvec upk) is (msys upk)
  ur <- uRes feed cv
  guard $ ur /= 0
  vr <- varReps feed ur cv
  let qp = map ((`rmod`ur) . qPlug vr) (orig upk)
  return $ signDet ur qp

-- | Runs through all possible subsets with a list of cvecs to try.
--   Returns a pair consisting of all sequences identified and
--   whether all subsets were successful.
etrSignsAll :: (Ord a, Reducible a) => [[a]] -> UPack a -> ([String], Bool)
etrSignsAll cvs upk = (concat $ catMaybes res, all (/=Nothing) res) where
  res = [ msum [ etrSigns upk is cv | cv <- cvs ] | is <- isets upk ]
