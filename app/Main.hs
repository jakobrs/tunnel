{-# LANGUAGE TypeApplications, LambdaCase #-}

module Main where

import System.Environment

import Tunnel

main :: IO ()
main = do
  (local, remote) <- getArgs >>= \case
    [  "p:p",              localport,             remoteport] -> pure (TCPEnd Nothing          (Just localport), TCPEnd Nothing           (Just remoteport))
    [  "p:h:p",            localport, remotehost, remoteport] -> pure (TCPEnd Nothing          (Just localport), TCPEnd (Just remotehost) (Just remoteport))
    ["h:p:p",   localhost, localport,             remoteport] -> pure (TCPEnd (Just localhost) (Just localport), TCPEnd Nothing           (Just remoteport))
    ["h:p:h:p", localhost, localport, remotehost, remoteport] -> pure (TCPEnd (Just localhost) (Just localport), TCPEnd (Just remotehost) (Just remoteport))
    [  "p:u",              localport, remoteunix            ] -> pure (TCPEnd Nothing          (Just localport), UnixEnd remoteunix                        )
    ["h:p:u",   localhost, localport, remoteunix            ] -> pure (TCPEnd (Just localhost) (Just localport), UnixEnd remoteunix                        )
    [  "u:p",   localunix,                        remoteport] -> pure (UnixEnd localunix,                        TCPEnd Nothing           (Just remoteport))
    [  "u:h:p", localunix,            remotehost, remoteport] -> pure (UnixEnd localunix,                        TCPEnd (Just remotehost) (Just remoteport))
    [  "u:u",   localunix,            remoteunix            ] -> pure (UnixEnd localunix,                        UnixEnd remoteunix                        )

  putStrLn "Starting tunnel"

  runTunnel local remote

-- touch ~/.vimrc breaks backspace
-- vim: ts=2 sts=2 et ai sw=2:
