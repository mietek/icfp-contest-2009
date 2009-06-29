module Main (main) where

import Control.Monad (forM_)
import System.Environment (getArgs)

import MoreTools
import qualified Task1 as Task1
import qualified Task2 as Task2
-- import qualified Task3 as Task3
-- import qualified Task4 as Task4
import VM


putScen :: Scen -> IO ()
putScen scen =
  forM_ scen $ \(n, (dx, dy)) ->
    putStrLn (show n ++ " " ++ show dx ++ " " ++ show dy)


solve :: Int -> Scen
solve conf =
  case conf `div` 1000 of
    1 -> Task1.main conf
    2 -> Task2.main conf
    -- 3 -> Task3.main conf
    -- 4 -> Task4.main conf


main :: IO ()
main =
  do
    [str] <- getArgs
    putStrLn str
    putScen (solve (read str))
