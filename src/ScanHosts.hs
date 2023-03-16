{-# LANGUAGE ScopedTypeVariables #-}

module ScanHosts(connectToPort) where

import Network.Socket
import qualified Control.Exception as E
import qualified System.Timeout as T

connectToPort :: HostName -> PortNumber -> Int -> IO Bool
connectToPort hostName portNumber timeout = do
  result <- E.try (do {addr <- resolve; open addr})
  case result of
    Right (Just s) -> do
      close s
      return True
    Left (_ :: E.IOException) -> do
      return False
    _ -> return False
  where
    resolve = do
      let hints = defaultHints { addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) (Just hostName) (Just (show portNumber))
    open addr = E.bracketOnError (openSocket addr) close $ \sock ->
      T.timeout timeout (do
      connect sock $ addrAddress addr
      return sock)
