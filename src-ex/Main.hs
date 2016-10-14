module Main where

import Shiva.Server        (runServer)
import Shiva.Config        (setup)

import System.Environment  (getArgs)
import System.Process      (callCommand)
import Control.Concurrent  (forkIO, threadDelay)


launch :: IO ()
launch = do
  threadDelay $ 5*10^4  -- 50 ms
  callCommand "open 'http://localhost:7777'"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["setup"]  -> setup
    ["server"] -> runServer
    []         -> forkIO launch *> runServer
    _          -> putStrLn "Unrecognized command."
