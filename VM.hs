{-# INCLUDE "vm.h" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module VM (new, copy, runStep, runNSteps, runScen, getOutput) where

import Control.Monad (mapM)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.C.String (CString, withCString)
import Foreign.Marshal.Alloc (finalizerFree)
import System.IO.Unsafe (unsafePerformIO)


data CState
type CStatePtr = Ptr CState
type CStateFPtr = ForeignPtr CState
data State = State !(CStateFPtr) deriving (Eq, Ord, Show)


foreign import ccall unsafe "vm.h new"
  cNew :: CString -> Word32 -> IO CStatePtr

foreign import ccall unsafe "vm.h copy"
  cCopy :: CStatePtr -> IO CStatePtr

foreign import ccall unsafe "vm.h run_step"
  cRunStep :: CStatePtr -> Double -> Double -> IO CStatePtr

foreign import ccall unsafe "vm.h run_n_steps"
  cRunNSteps :: CStatePtr -> Word32 -> Double -> Double -> IO CStatePtr

foreign import ccall unsafe "vm.h get_max_output"
  cGetMaxOutput :: CStatePtr -> IO Word32

foreign import ccall unsafe "vm.h get_output"
  cGetOutput :: CStatePtr -> Word32 -> IO Double


new :: String -> Int -> State
new b c =
  unsafePerformIO $
    withCString b $ \b' -> do
      p <- cNew b' (fromIntegral c)
      q <- newForeignPtr finalizerFree p
      return (State q)

copy :: State -> State
copy (State q1) =
  unsafePerformIO $
    withForeignPtr q1 $ \p1 -> do
      p2 <- cCopy p1
      q2 <- newForeignPtr finalizerFree p2
      return (State q2)

runStep :: State -> (Double, Double) -> State
runStep (State q1) (dx, dy) =
  unsafePerformIO $
    withForeignPtr q1 $ \p1 -> do
      p2 <- cRunStep p1 dx dy
      q2 <- newForeignPtr finalizerFree p2
      return (State q2)

runNSteps :: State -> Int -> (Double, Double) -> State
runNSteps (State q1) n (dx, dy) =
  unsafePerformIO $
    withForeignPtr q1 $ \p1 -> do
      p2 <- cRunNSteps p1 (fromIntegral n) dx dy
      q2 <- newForeignPtr finalizerFree p2
      return (State q2)

runScen :: State -> [(Int, (Double, Double))] -> State
runScen s ((n, d) : ps) = runScen (runNSteps s n d) ps
runScen s [] = s

getOutput :: State -> [Double]
getOutput (State q) =
  unsafePerformIO $
    withForeignPtr q $ \p -> do
      m <- cGetMaxOutput p
      mapM (cGetOutput p) [0 .. m]
