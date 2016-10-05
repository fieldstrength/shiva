module Main where

import Shiva.Server (runServer)
import Shiva.Config (setup)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["setup"]  -> setup
    ["server"] -> runServer
    []         -> runServer
    _          -> putStrLn "Unrecognized command."
