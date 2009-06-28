module Task2 (main) where

import Tools
import MoreTools
import qualified VM as VM


runVM :: Int -> String -> (Double, RV, RV, RV)
runVM k i = (f, (x1, y1), (x1 - x2, y1 - y2), (x2, y2))
  where
    [_, f, x1, y1, x2, y2] = VM.run 2 k i


try :: Int -> Int -> (String, Double)
try k t1 = (i3 ++ frame 1000 (0, 0), lenRV d3)
  where
    run = runVM k
    i1 = frame t1 (0, 0)
    (_, p1a, q1a, _) = run i1
    (_, p1b, _, _) = run (i1 ++ frame 1 (0, 0))
    r1 = lenRV p1a
    r2 = lenRV q1a
    v1 = satVelRV p1a p1b
    vd1 = hohVDeltaRV v1 r1 r2
    t2 = round (hohTime r1 r2)
    i2 = i1 ++ frame 1 vd1 ++ frame t2 (0, 0)
    (_, p2a, _, _) = run i2
    (_, p2b, _, _) = run (i2 ++ frame 1 (0, 0))
    v2 = satVelRV p2a p2b
    vd2 = hohVDeltaRV' v2 r1 r2
    i3 = i2 ++ frame 1 vd2
    (_, _, _, d3) = run i3


-- 2001: 15926
-- 2002: 5416.5?
-- 2003: 34140
-- 2004: 14727


main :: Int -> String
main k = show d
  where
    (_, d) = try k 15926
