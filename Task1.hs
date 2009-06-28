module Task1 (main) where

import Tools
import MoreTools
import qualified VM as VM


runVM :: Int -> String -> (Double, RV, M)
runVM k i = (f, (x, y), r)
  where
    [_, f, x, y, r] = VM.run 1 k i


main :: Int -> String
main k = i3
  where
    run = runVM k
    runP = (\(_, p, _) -> p) . run
    runFP = (\(f, p, _) -> (f, p)) . run
    (_, p, r2) = run (frame 1 (0, 0))
    r1 = lenRV p
    i1 = hohTrans runP r1 r2 (frame 1 (0, 0))
    i2 = burnFuel runFP i1
    i3 = i2 ++ frame 901 (0, 0)
