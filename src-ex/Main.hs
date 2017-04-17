module Main where

import Shiva.Server        (runServer)
import Shiva.Config        (setup, setupDB)
import Shiva.Sources       (sources)

import System.Environment  (getArgs)
import System.Process      (callCommand)
import Control.Concurrent  (forkIO, threadDelay)


launch :: IO ()
launch = do
  threadDelay $ 5*10^4  -- 50 ms, exact number not important
  callCommand "open 'http://localhost:7777'"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["setup"]    -> setup
    ["setup-db"] -> setupDB
    ["server"]   -> runServer sources
    []           -> forkIO launch *> runServer sources
    _            -> putStrLn "Unrecognized command."
