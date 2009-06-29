module Task1 (main) where

import Tools
import MoreTools
import qualified VM as VM


runScen :: Int -> Scen -> (Double, RV, M)
runScen conf scen = (f, (x, y), r)
  where
    s1 = VM.new "bin1.obf" conf
    s2 = VM.runScen s1 scen
    [_, f, x, y, r] = VM.getOutput s2


main :: Int -> Scen
main conf = scen3
  where
    run = runScen conf
    runP = (\(_, p, _) -> p) . run
    runFP = (\(f, p, _) -> (f, p)) . run
    (_, p, r2) = run [(1, (0, 0))]
    r1 = lenRV p
    scen1 = hohTrans runP r1 r2 [(1, (0, 0))]
    scen2 = burnFuel runFP scen1
    scen3 = scen2 ++ [(901, (0, 0))]
