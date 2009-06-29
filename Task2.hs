module Task2 (main) where

import Debug.Trace (trace)

import LessTools
import Tools
import MoreTools
import VM


type Result = (Rad, M, State, Scen)


get :: State -> (RV, RV, RV)
get s =
  case getOutput s of
    [_, _, x1, y1, x2, y2] -> ((x1, y1), (x1 - x2, y1 - y2), (x2, y2))


getP :: State -> RV
getP = firstOf3 . get

getQ :: State -> RV
getQ = secondOf3 . get

getD :: State -> RV
getD = thirdOf3 . get


tryHohTrans :: Double -> State -> Result
tryHohTrans dstR s0 = (a, d, s1, hohScen)
  where
    (s1, hohScen) = hohTrans getP dstR s0
    a = (getP s1) `angSubRV'` (getQ s1)
    d = lenRV (getD s1)


didJump :: Rad -> Rad -> Bool
didJump a1 a2 | pi / 2 > a1 && a1 > 0 && 0 > a2 && a2 > -pi / 2 = True
didJump a1 a2 | pi / 2 > a2 && a2 > 0 && 0 > a1 && a1 > -pi / 2 = True
didJump _ _ = False


findBest :: Double -> Int -> State -> (Int, Result)
findBest dstR step s0 =
  let
    x0 = tryHohTrans dstR s0
  in
    loop 0 step x0 s0
  where
    loop steps step x1@(a1, _, _, _) s1 =
      let
        s2 = runNStepsZ step s1
        x2@(a2, _, _, _) = tryHohTrans dstR s2
      in
        if didJump a1 a2
          then
            if step == 1
              then
                if abs a1 < abs a2
                  then (steps, x1)
                  else (steps + 1, x2)
              else loop steps (step `div` 10) x1 s1
          else loop (steps + step) step x2 s2        


main :: Int -> Scen
main conf = stdScen ([(steps, (0, 0))] ++ bestScen ++ matchScen)
  where
    s0 = runStepZ (new "bin2.obf" conf)
    dstR = lenRV (getQ s0)
    (steps, (_, _, s1, bestScen)) = findBest dstR 1000 s0
    (_, matchScen) = matchVel getP getQ s1
