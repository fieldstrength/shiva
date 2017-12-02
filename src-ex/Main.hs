module Main where

import Shiva.Config       (setup, setupDB)
import Shiva.Server       (runServer)
import Shiva.Sources      (sources)

import Control.Concurrent (forkIO, threadDelay)
import System.Environment (getArgs)
import System.Process     (callCommand)


launch :: IO ()
launch = do
      threadDelay $ 5*10^5  -- 500 ms, exact number not important
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
