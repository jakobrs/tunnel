module Tunnel
    ( runTunnel
    ) where

import           Control.Monad
import           Control.Concurrent
import qualified Data.ByteString as BS
import           Data.Functor
import           Network.Run.TCP
import           Network.Socket
import           Network.Socket.ByteString

-- CLIENT    server <-----> cli    TUNNEL    srv <-----> client    SERVER

runTunnel :: Maybe HostName     -- ^ What hostname to listen on
          -> HostName           -- ^ Hostname to redirect traffic to
          -> ServiceName        -- ^ Port to listen on
          -> ServiceName        -- ^ Port to redirect traffic to
          -> Int                -- ^ Block size
          -> Bool               -- ^ Debug
          -> IO ()
runTunnel host host' port port' bs debug =
  withSocketsDo $ 
    runTCPServer host port $ \cli -> do
      when debug $ putStrLn $ "Connection with client established: " ++ show cli
      runTCPClient host' port' $ \srv -> do
        when debug $ putStrLn $ "Connection with server established: " ++ show srv

        block <- newEmptyMVar :: IO (MVar ())

        _cli2srv <- forkIO $ runOneWayTunnel cli srv bs block >> when debug (putStrLn "Client->server tunnel ended naturally")
        _srv2cli <- forkIO $ runOneWayTunnel srv cli bs block >> when debug (putStrLn "Server->client tunnel ended naturally")

        takeMVar block

        putStrLn "Closing connections"

runOneWayTunnel :: Socket -> Socket -> Int -> MVar () -> IO ()
runOneWayTunnel from to bs block = while $ do
  msg <- recv from bs
  if BS.null msg
    then putMVar block () $> False
    else sendAll to msg   $> True

while :: IO Bool -> IO ()
while action = go
  where
    go :: IO ()
    go = do
      cont <- action
      when cont go
