import Prelude()
import Math.Base
import Math.Poly.Multi
import Math.ETR

a, b :: MPoly Integer
a = x_ 0; b = x_ 1

main = forM_ [346, 347] $ \z -> do
  let eqs = [ z - a^2 - 3*b^2, a*b - 100 ]
      upk = prepSimpETR eqs 256 [3,1] [7,11]
  print $ etrSignsAll [[2,3]] upk
