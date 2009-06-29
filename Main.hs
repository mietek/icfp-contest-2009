module Main (main) where

import Control.Monad (forM_)
import System.Environment (getArgs)
import qualified Task1 as Task1
-- import qualified Task2 as Task2
-- import qualified Task3 as Task3
-- import qualified Task4 as Task4


main :: IO ()
main =
  do
    [str] <- getArgs
    putStrLn str
    let
      conf = read str
      scen = case conf `div` 1000 of
               1 -> Task1.main conf
               -- 2 -> Task2.main conf
               -- 3 -> Task3.main conf
               -- 4 -> Task4.main conf
    forM_ scen $ \(n, (dx, dy)) ->
      putStrLn (show n ++ " " ++ show dx ++ " " ++ show dy)
