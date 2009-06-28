module Main (main) where

import System.Environment (getArgs)
import qualified Task1 as Task1
import qualified Task2 as Task2
-- import qualified Task3 as Task3
-- import qualified Task4 as Task4


task :: Int -> Int -> String
task 1 = Task1.main
task 2 = Task2.main
-- task 3 = Task3.main
-- task 4 = Task4.main


main :: IO ()
main =
  do
    [s] <- getArgs
    putStrLn s
    let c = read s
    let (n, k) = (c `div` 1000, c `mod` 1000)
    putStrLn (task n k)
