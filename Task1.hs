module Task1 (main) where

import LessTools
import Tools
import MoreTools
import VM


get :: State -> (Double, RV, M)
get s =
  case getOutput s of
    [_, f, x, y, r] -> (f, (x, y), r)
                       

getF :: State -> Double
getF = firstOf3 . get

getP :: State -> RV
getP = secondOf3 . get

getR :: State -> M
getR = thirdOf3 . get


wasteFuel :: State -> Scen
wasteFuel s0 = scen
  where
    f = getF s0 - 0.000001
    v = curSatVelRV getP s0
    vd = extDeltaRV v (f / 4)
    vd' = extDeltaRV v (f / (-2))
    scen = [(1, vd), (1, vd'), (1, vd)]


main :: Int -> Scen
main conf = stdScen (hohScen ++ wasteScen)
  where
    s0 = runStepZ (new "bin1.obf" conf)
    dstR = getR s0
    (s1, hohScen) = hohTrans getP dstR s0
    wasteScen = wasteFuel s1
