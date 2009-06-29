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


-- This one gave us 3001.txt with some hand-tweaking
-- main :: Int -> Scen
-- main conf = [(2 * 10803 + 6890, (0, 0))] ++ hohScen ++ [(7606 - 6890 - 23, (0, 0))] ++ matchScen ++ [(901, (0, 0))]
--   where
--     s0 = runNStepsZ (2 * 10803 + 6890) (new "bin3.obf" conf)
--     (s1, hohScen) = hohTrans getP 8357000 s0
--     s2 = runNStepsZ (7606 - 6890 - 23) s1
--     (_, matchScen) = matchVel getP getQ s2

-- This one gave us 3002.txt
-- main :: Int -> Scen
-- main conf = [(7467, (0, 0))] ++ hohScen ++ [(123510 - 34531 - 7467 + 167, (0, 0))] ++ matchScen ++ [(901, (0, 0))]
--   where
--     s0 = runNStepsZ 7467 (new "bin3.obf" conf)
--     (s1, hohScen) = hohTrans getP 6.3887059314144686e7 s0
--     s2 = runNStepsZ (123510 - 34531 - 7467 + 167) s1 
--     (_, matchScen) = matchVel getP getQ s2

-- This one gave us 3003.txt
main :: Int -> Scen
main conf = [(34194, (0, 0))] ++ hohScen ++ [(783190 - 3457 - 34194 - 8, (0, 0))] ++ matchScen ++ [(901, (0, 0))]
  where
    s0 = runNStepsZ 34194 (new "bin3.obf" conf)
    (s1, hohScen) = hohTrans getP 7357000 s0
    s2 = runNStepsZ (783190 - 3457 - 34194 - 8) s1 
    (_, matchScen) = matchVel getP getQ s2

-- This one gave us 3004.txt with some hand-tweaking
-- main :: Int -> Scen
-- main conf = [(10803 + 6356, (0, 0))] ++ hohScen ++ [(7606 - 6356 + 16, (0, 0))] ++ matchScen ++ [(901, (0, 0))]
--   where
--     s0 = runNStepsZ (10803 + 6356) (new "bin3.obf" conf)
--     (s1, hohScen) = hohTrans getP 8357000 s0
--     s2 = runNStepsZ (7606 - 6356 + 16) s1
--     (_, matchScen) = matchVel getP getQ s2

-- main :: Int -> Scen
-- main conf = x
--   where
--     s0 = runNStepsZ 1 (new "bin3.obf" conf)
--     x = [(i, (test i, 0)) | i <- [34190 .. 783190 - 3457]]
--     test i = a
--       where
--         dstR = lenRV (getQ s0)
--         s1 = runNStepsZ i s0
--         (s2, _) = hohTrans getP dstR s1
--         s3 = runNStepsZ (783190 - 3457 - i) s2
--         a = radToDeg (angRV (getP s3))

-- main :: Int -> Scen
-- main conf = trace z $ []
--   where
--     s0 = runStepZ (new "bin3.obf" conf)
--     p0 = getP s0
--     srcR = lenRV p0
--     q0 = getQ s0
--     dstR = lenRV q0
--     t = hohTime (lenRV p0) (lenRV q0)
--     b = radToDeg (angRV q0)
--     z = show srcR ++ " " ++ show dstR ++ " " ++ show t ++ " " ++ show b
