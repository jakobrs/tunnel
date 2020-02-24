{-# LANGUAGE TypeApplications, LambdaCase #-}

module Main where

import System.Environment

import Tunnel

main :: IO ()
main = do
  (localhost, localport, remotehost, remoteport) <- getArgs >>= \case
    [           localport, remotehost, remoteport] -> pure (Nothing,        Just localport, Just remotehost, Just remoteport)
    [localhost, localport, remotehost, remoteport] -> pure (Just localhost, Just localport, Just remotehost, Just remoteport)

  runTunnel localhost localport remotehost remoteport

-- touch ~/.vimrc breaks backspace
-- vim: ts=2 sts=2 et ai sw=2:
