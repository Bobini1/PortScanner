module ParseParams(parseParams, PortSpec(..), AddrSpec(..), parsePorts, parseAddrs, parseCidrAddr, parsePortList) where

import Network.Socket
import Control.Monad
import Net.Types
import Net.IPv4
import Data.Maybe
import Data.List(nub)
import Data.List.Split


data PortSpec = Port PortNumber | Ports [PortNumber] deriving(Show)
data AddrSpec = Address HostName | CidrNetwork [HostName] deriving(Show)

parsePorts :: String -> Maybe PortSpec
parsePorts str = do
  [(firstPort, rest)] <- Just $ reads str
  case rest of
    [] -> return $ Port firstPort
    '-':otherBound  -> do
      [(lastPort, [])] <- Just $ reads otherBound
      guard (lastPort > firstPort)
      return $ Ports $ [firstPort..lastPort] ++ [lastPort]
    _ -> Nothing

parsePortList :: String -> [Maybe PortSpec]
parsePortList str =
  map parsePorts (splitOn "," str)


parseCidrAddr :: String -> Maybe [IPv4]
parseCidrAddr str = do
  [(iprange, [])] <- Just $ reads str
  return $ toList iprange

parseAddrs :: String -> AddrSpec
parseAddrs str =
  case parseCidrAddr str of
    Just addrs -> CidrNetwork $ map encodeString addrs
    Nothing -> Address $ str


parseParams :: [String] -> Maybe ([PortNumber], [HostName])
parseParams argv =
  let parse (ports, hosts) ("-p":first:rest) =
        parse (parsePortList first ++ ports, hosts) rest
      parse (ports, hosts) (first:rest) | first /= "-p" =
        parse (ports, parseAddrs first:hosts) rest
      parse result [] = Just result
      parse _ _ = Nothing
  in do
    (ports, hosts) <- parse ([], []) argv
    guard (all isJust ports)
    let mergePorts (Port p) ports         = p:ports
        mergePorts (Ports ps) ports       = ps ++ ports
        mergeHosts (Address h) hosts      = h:hosts
        mergeHosts (CidrNetwork hs) hosts = hs ++ hosts
    return (nub (foldr mergePorts [] (catMaybes ports)), nub (foldr mergeHosts [] hosts))

