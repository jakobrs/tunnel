{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment
import System.Exit

import Tunnel

main :: IO ()
main = do
  (host, port, host', port') <- getArgs >>= \case
    [host, port, host'       ] -> pure (Just host, port, host', port )
    [host, port, host', port'] -> pure (Just host, port, host', port')
    [      port, host'       ] -> pure (Nothing  , port, host', port )
    _                          -> putStrLn "Incorrect syntax" >> exitFailure

  putStrLn "Running tunnel"
  runTunnel host host' port port' 4096 True
