module VM (run) where

import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)


runVM :: String -> String -> String
runVM b i = unsafePerformIO (readProcess "./vm" [b] i)


parseOutput :: String -> [[Double]]
parseOutput = aux . lines
  where
    aux [] = []
    aux (l : ls) = map read (concat (map (tail . words) ls1)) : aux ls2
      where
        (ls1, ls2) = splitAt n ls
        n = read l


run :: Int -> Int -> String -> [Double]
run n k i = last (parseOutput o)
  where
    o = runVM b i'
    b = ("bin" ++ show n ++ ".obf")
    i' = show (n * 1000 + k) ++ " " ++ i
