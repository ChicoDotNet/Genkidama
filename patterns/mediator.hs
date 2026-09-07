module Main where

import System.Process (callProcess)

main :: IO ()
main = callProcess "runghc" ["src/Functional/Haskell/patterns/mediator.hs"]
