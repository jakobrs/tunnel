{-# LANGUAGE TypeApplications, LambdaCase #-}

module Main where

import System.Environment
import Data.Function
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString as BS
import Network.Socket
import Network.Socket.ByteString

-- The following two functions are based on
-- code from the "network-run" package
-- Copyright (c) 2019, IIJ Innovation Institute Inc.
-- BSD 3-Clause License
connectToServer :: Maybe HostName -> Maybe ServiceName -> (Socket -> IO ()) -> IO ()
connectToServer host port client = do
    let hints = defaultHints { addrSocketType = Stream }
    addr <- head <$> getAddrInfo (Just hints) host port
    print addr

    E.bracket (open addr) close client
  where
    open :: AddrInfo -> IO Socket
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      return sock

runServer :: Maybe HostName -> Maybe ServiceName -> (Socket -> SockAddr -> IO ()) -> IO ()
runServer host port server = do
    let hints = defaultHints
          { addrSocketType = Stream
          , addrFlags = [ AI_PASSIVE ]
          }
    addr <- head <$> getAddrInfo (Just hints) host port
    print addr

    E.bracket (open addr) close (forever . loop)
  where
    open :: AddrInfo -> IO Socket
    open addr = do
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

runTunnel :: Maybe HostName -> Maybe ServiceName -> Maybe HostName -> Maybe ServiceName -> IO ()
runTunnel localhost localport remotehost remoteport = runServer localhost localport $ \cli peer -> do
  putStrLn $ "Connection to " ++ show peer ++ ": " ++ show cli ++ " <--> ???"
  connectToServer remotehost remoteport $ \srv -> do
    putStrLn $ "Connection to " ++ show peer ++ ": " ++ show cli ++ " <--> " ++ show srv

    race_ (sendFromTo cli srv) (sendFromTo srv cli)

main :: IO ()
main = do
  (localhost, localport, remotehost, remoteport) <- getArgs >>= \case
    [           localport, remotehost, remoteport] -> pure (Nothing,        Just localport, Just remotehost, Just remoteport)
    [localhost, localport, remotehost, remoteport] -> pure (Just localhost, Just localport, Just remotehost, Just remoteport)

  runTunnel localhost localport remotehost remoteport

-- touch ~/.vimrc breaks backspace
-- vim: ts=2 sts=2 et ai sw=2:
