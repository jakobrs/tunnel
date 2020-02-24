module Tunnel where

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString as BS
import Network.Socket
import Network.Socket.ByteString

data TunnelEnd
  = TCPEnd (Maybe HostName) (Maybe ServiceName)
  | UnixEnd String
  deriving (Show, Eq, Ord)

-- The runClient, runServer and resolve functions are based on
-- code from the "network-run" package
-- Copyright (c) 2019, IIJ Innovation Institute Inc.
-- BSD 3-Clause License
resolve :: [AddrInfoFlag] -> TunnelEnd -> IO AddrInfo
resolve flags (TCPEnd host port) = do
  let hints = defaultHints { addrSocketType = Stream, addrFlags = flags }
  head <$> getAddrInfo (Just hints) host port
resolve flags (UnixEnd location) = do
  pure $ AddrInfo
    { addrFlags = flags
    , addrFamily = AF_UNIX
    , addrSocketType = Stream
    , addrProtocol = defaultProtocol
    , addrAddress = SockAddrUnix location
    , addrCanonName = Nothing              -- I've no idea what this *is*,
                                           -- but we never use it anywhere
                                           -- anyways.
    }

runClient :: AddrInfo -> (Socket -> IO ()) -> IO ()
runClient addr client = E.bracket open close client
  where
    open :: IO Socket
    open = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      return sock

runServer :: AddrInfo -> (Socket -> SockAddr -> IO ()) -> IO ()
runServer addr server = E.bracket open close (forever . loop)
  where
    open :: IO Socket
    open = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      withFdSocket sock $ setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock

    loop :: Socket -> IO ()
    loop sock = do
      (conn, peer) <- accept sock
      void $ forkFinally (server conn peer) (const $ gracefulClose conn 5000)

sendFromTo :: Socket -> Socket -> IO ()
sendFromTo from to = loop
  where
    loop = do
      str <- recv from 1024
      when (not (BS.null str)) $ do
        sendAll to str
        loop

      putStrLn $ show from ++ " finished"

runTunnel :: TunnelEnd -> TunnelEnd -> IO ()
runTunnel local remote = do
  localAddr <- resolve [ AI_PASSIVE ] local
  remoteAddr <- resolve [] remote

  runServer localAddr $ \cli peer -> do
    putStrLn $ "Connection to " ++ show peer ++ ": " ++ show cli ++ " <--> ???"
    runClient remoteAddr $ \srv -> do
      putStrLn $ "Connection to " ++ show peer ++ ": " ++ show cli ++ " <--> " ++ show srv

      race_ (sendFromTo cli srv) (sendFromTo srv cli)

-- touch ~/.vimrc breaks backspace
-- vim: ts=2 sts=2 et ai sw=2:
