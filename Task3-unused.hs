module Task3 (main) where

import Debug.Trace (trace)

import LessTools
import Tools
import MoreTools
import VM


-- type Result = (Int, RV, M)
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


-- oneStep s n = (n, dstR, r) : (oneStep s1 (n+1))
--  where 
--  s1 = runStepZ s 
--  dstR = getQ s1
--  r = lenRV dstR
-- 
-- findMinMax n p p2 (x1:x2:xs) = 
--  if p (thirdOf3 x1) (thirdOf3 x2)
--    then x1 :(if n < 1 then [] else findMinMax (n-1) p2 p (x1:x2:xs))
--    else findMinMax n p p2 (x2:xs)
-- 
-- -- main :: Int -> Scen
-- main conf = ekstrema
--   where
--     firstState = new "bin3.obf" conf
--     list = oneStep firstState 1
--     ekstrema = findMinMax 3 (>) (<) list


-- findEllipse :: State -> (Double, Double)
-- findEllipse s0 =
--   let
--     p0 = getP s0
--     r0 = lenRV p0
--     a0 = angRV p0
--   in
--     loop 0 (0, p0, r0) (0, p0, r0) s0
--   where
--     loop step minX maxX s1 =
--       let
--         s2 = runStepZ s1
--         p1 = getP s1
--         r1 = lenRV p1
--         a1 = angRV a1
--       in
--         if a1 `angSubRV` a2 == 0
--           then (minX, maxX)


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

-- This one gave us 3001.txt, after adding 1 1 to it by hand
main :: Int -> Scen
main conf = [(2 * 10803 + 6890, (0, 0))] ++ hohScen ++ [(7606 - 6890 - 23, (0, 0))] ++ matchScen ++ [(901, (0, 0))]
  where
    s0 = runNStepsZ (2 * 10803 + 6890) (new "bin3.obf" conf)
    (s1, hohScen) = hohTrans getP 8357000 s0
    s2 = runNStepsZ (7606 - 6890 - 23) s1
    (_, matchScen) = matchVel getP getQ s2

-- main :: Int -> Scen
-- main conf = trace (show a1) $ x
--   where
--     s0 = runNStepsZ (2 * 10803) (new "bin3.obf" conf)
--     a1 = radToDeg (angRV (getQ s0))
--     x = [(i, (test i, 0)) | i <- [6800 .. 7605]]
--     test i = a
--       where
--         s1 = runNStepsZ i s0
--         (s2, _) = hohTrans getP 8357000 s1
--         s3 = runNStepsZ (7606 - i) s2
--         a = radToDeg (angRV (getP s3))

-- main :: Int -> Scen
-- main conf = stdScen (bestScen ++ halfHohScen)
--   where
--     s0 = runStepZ (new "bin3.obf" conf)
--     p0 = getP s0
--     srcR = lenRV p0
--     minDstR = 8357000
--     maxDstR = 1.2799965491022138e7
--     (steps, (_, _, s1, bestScen)) = findBest minDstR 1000 s0
--     p1 = getP s1
--     v1 = curSatVelRV getP s0
--     vd1 = hohVDeltaRV v1 srcR minDstR
--     t = hohTime srcR minDstR
--     halfHohScen = [(1, vd1), (t, (0, 0))]
