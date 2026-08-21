module Main where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE registry #-}
registry :: IORef Int
registry = unsafePerformIO (newIORef 0)

registryInstance :: IO (IORef Int)
registryInstance = pure registry

main :: IO ()
main = do
  first <- registryInstance
  second <- registryInstance
  atomicModifyIORef' first (\count -> (count + 1, ()))
  count <- readIORef second
  putStrLn $ "same=" ++ show (first == second)
  putStrLn $ "count=" ++ show count
