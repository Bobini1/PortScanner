module Main (main) where

import System.Environment
import Control.Concurrent.Async
import Control.Concurrent.MVar

import ParseParams(parseParams)
import ScanHosts(connectToPort)


runWithArgs :: [String] -> IO ()
runWithArgs args =
  case parseParams args of
    Nothing -> print "Invalid program arguments!"
    Just (ports, hosts) -> do
      lock <- newMVar ()
      mapConcurrently_ (\(host, port) -> reportStatus lock host port) ((,) <$> hosts <*> ports)
  where
    timeout = 500000
    reportStatus lock host port = do
      status <- connectToPort host port timeout
      withMVar lock (\_ ->
                       putStrLn $ (show host) ++ ":" ++ (show port) ++ " - " ++ (if status then "OPEN" else "CLOSED"))

main :: IO ()
main = do
  args <- getArgs
  runWithArgs args


