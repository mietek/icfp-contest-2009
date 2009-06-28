module Task2 (main) where

import Debug.Trace (trace)
import Tools
import MoreTools
import qualified VM as VM


runVM :: Int -> String -> (Double, RV, RV, RV)
runVM k i = (f, (x1, y1), (x1 - x2, y1 - y2), (x2, y2))
  where
    [_, f, x1, y1, x2, y2] = VM.run 2 k i


test :: Int -> String -> (String, Double)
test k i0 = (i2, lenRV d)
  where
    run = runVM k
    runP = (\(_, p, _, _) -> p) . run
    (_, p, q, _) = run i0
    r1 = lenRV p
    r2 = lenRV q
    i1 = hohTrans runP r1 r2 i0
    (_, _, _, d) = run i1
    i2 = i1 ++ frame 1000 (0, 0)

-- TODO: intelligent starting step

-- largestPow10 :: Double -> Int
-- largestPow10 x = 10^(fromIntegral (floor (logBase 10 x)))

findBest :: Int -> String -> (Int, String, Double)
findBest k i0 = (t5 + 1, i5, d5)
  where
    (_, d1) = test k i0
    (_, d1') = test k (i0 ++ frame 1 (0, 0))
    t2 = if d1 < d1'
           then goAway k i0 1000 1 d1
           else goToward k i0 1000 1 d1
    (_, d2) = test k (i0 ++ frame t2 (0, 0))
    t3 = goToward k i0 100 t2 d2
    (_, d3) = test k (i0 ++ frame t3 (0, 0))
    t4 = goToward k i0 10 t3 d3
    (_, d4) = test k (i0 ++ frame t4 (0, 0))
    t5 = goToward k i0 1 t4 d4
    (i5, d5) = test k (i0 ++ frame (t5 + 1) (0, 0))

goAway :: Int -> String -> Int -> Int -> Double -> Int
goAway k i0 s t d =
  trace ("a " ++ show t ++ " " ++ show d) $ if d < d'
    then goAway k i0 s t' d'
    else goToward k i0 s t' d'
  where
    t' = t + s
    (_, d') = test k (i0 ++ frame t' (0, 0))

goToward :: Int -> String -> Int -> Int -> Double -> Int
goToward k i0 s t d =
  trace ("t " ++ show t ++ " " ++ show d) $ if d < d'
    then t - s
    else goToward k i0 s t' d'
  where
    t' = t + s
    (_, d') = test k (i0 ++ frame t' (0, 0))


-- TODO: 2002 needs more precision
-- we should check if we find a good enough solution (d < 1000)
-- if not, maybe we should enter slower orbit (max r1 r2 * 1.25)?

-- main :: Int -> String
-- main k = show d
--   where
--     run = runVM k
--     runP = (\(_, p, _, _) -> p) . run
--     (_, p, q, _) = run (frame 1 (0, 0))
--     r1 = lenRV p
--     r2 = lenRV q
--     i1 = if min r1 r2 / max r1 r2 < 0.5
--       then hohTrans runP r1 (max r1 r2 * 0.7) (frame 1 (0, 0))
--       else frame 1 (0, 0)
--     (t, _, d) = findBest k i1


main :: Int -> String
main k = i
  where
    (_, i, _) = findBest k (frame 1 (0, 0))



-- 2001: 15926
-- 2002: 5416.5?
-- 2003: 34140
-- 2004: 14727