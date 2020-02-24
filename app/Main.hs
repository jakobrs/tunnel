{-# LANGUAGE TypeApplications, LambdaCase #-}

module Main where

import System.Environment

import Tunnel

main :: IO ()
main = do
  (local, remote) <- getArgs >>= \case
    ["p:h:p",              localport, remotehost, remoteport] -> pure (TCPEnd Nothing          (Just localport), TCPEnd (Just remotehost) (Just remoteport))
    ["h:p:h:p", localhost, localport, remotehost, remoteport] -> pure (TCPEnd (Just localhost) (Just localport), TCPEnd (Just remotehost) (Just remoteport))
    ["p:s",                localport, remoteunix            ] -> pure (TCPEnd Nothing          (Just localport), UnixEnd remoteunix                        )
    ["h:p:s",   localhost, localport, remoteunix            ] -> pure (TCPEnd (Just localhost) (Just localport), UnixEnd remoteunix                        )
    ["s:h:p",   localunix,            remotehost, remoteport] -> pure (UnixEnd localunix,                        TCPEnd (Just remotehost) (Just remoteport))
    ["s:s",     localunix,            remoteunix            ] -> pure (UnixEnd localunix,                        UnixEnd remoteunix                        )

  runTunnel local remote

-- touch ~/.vimrc breaks backspace
-- vim: ts=2 sts=2 et ai sw=2:
