{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)



{-
Socket programming 
https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-Socket.html#v:AI_NUMERICHOST


Network types:
type HostName = String
  Either a host name e.g., "haskell.org" or a numeric host address string consisting of a dotted decimal IPv4 address or an IPv6 address e.g., "192.168.0.1".

type ServiceName = String
  Either a service name e.g., "http" or a numeric port number.

data AddrInfo
  Constructors - AddrInfo:

    addrFlags :: [AddrInfoFlag] 
    addrFamily :: Family 
    addrSocketType :: SocketType 
    addrProtocol :: ProtocolNumber 
    addrAddress :: SockAddr 
    addrCanonName :: Maybe String 

-}