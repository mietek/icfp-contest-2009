module Task2 (main) where

import Debug.Trace (trace)
import Tools
import MoreTools
import qualified VM as VM


runScen :: Int -> Scen -> (Double, RV, RV, RV)
runScen conf scen = (f, (x1, y1), (x1 - x2, y1 - y2), (x2, y2))
  where
    s1 = VM.new "bin2.obf" conf
    s2 = VM.runScen s1 scen
    [_, f, x1, y1, x2, y2] = VM.getOutput s2


test :: Int -> Scen -> (Scen, Double)
test conf scen0 = (scen2, lenRV d)
  where
    run = runScen conf
    runP = (\(_, p, _, _) -> p) . run
    (_, p, q, _) = run scen0
    r1 = lenRV p
    r2 = lenRV q
    scen1 = hohTrans runP r1 r2 scen0
    (_, _, _, d) = run scen1
    scen2 = scen1 ++ [(901, (0, 0))]

-- TODO: intelligent starting step

-- largestPow10 :: Double -> Int
-- largestPow10 x = 10^(fromIntegral (floor (logBase 10 x)))

findBest :: Int -> Scen -> (Int, Scen, Double)
findBest conf scen0 = (t5 + 1, scen5, d5)
  where
    t = test conf
    away = goAway conf scen0
    toward = goToward conf scen0
    (_, d1) = t scen0
    (_, d1') = t (scen0 ++ [(1, (0, 0))])
    t2 = if d1 < d1'
           then away 1000 1 d1
           else toward 1000 1 d1
    (_, d2) = t (scen0 ++ [(t2, (0, 0))])
    t3 = toward 100 t2 d2
    (_, d3) = t (scen0 ++ [(t3, (0, 0))])
    t4 = toward 10 t3 d3
    (_, d4) = t (scen0 ++ [(t4, (0, 0))])
    t5 = toward 1 t4 d4
    (scen5, d5) = t (scen0 ++ [(t5 + 1, (0, 0))])

goAway :: Int -> Scen -> Int -> Int -> Double -> Int
goAway k scen0 s t d =
  trace ("a " ++ show t ++ " " ++ show d) $ if d < d'
    then goAway k scen0 s t' d'
    else goToward k scen0 s t' d'
  where
    t' = t + s
    (_, d') = test k (scen0 ++ [(t', (0, 0))])

goToward :: Int -> Scen -> Int -> Int -> Double -> Int
goToward k scen0 s t d =
  trace ("t " ++ show t ++ " " ++ show d) $ if d < d'
    then t - s
    else goToward k scen0 s t' d'
  where
    t' = t + s
    (_, d') = test k (scen0 ++ [(t', (0, 0))])


-- TODO: 2002 needs more precision
-- we should check if we find a good enough solution (d < 1000)
-- if not, maybe we should enter slower orbit (max r1 r2 * 1.25)?

main :: Int -> Scen
main conf = scen2
  where
    run = runScen conf
    runP = (\(_, p, _, _) -> p) . run
    (_, p, q, _) = run [(1, (0, 0))]
    r1 = lenRV p
    r2 = lenRV q
    scen1 = if min r1 r2 / max r1 r2 < 0.5
      then hohTrans runP r1 (max r1 r2 * 0.7) [(1, (0, 0))]
      else [(1, (0, 0))]
    (t, scen2, d) = findBest conf scen1


-- main :: Int -> Scen
-- main conf = scen
--   where
--     (_, scen, _) = findBest conf [(1, (0, 0))]



-- 2001: 15926
-- 2002: 5416.5?
-- 2003: 34140
-- 2004: 14727