module Config where

{-
data Scheme = HTTP | HTTPS deriving (Show, Eq, Ord)
type Port = Int

data Config = Config
  { bind_address :: (        Address, Port)
  , homeserver   :: (Scheme, Address, Port)
  } deriving (Show, Eq, Ord)

parseConfig :: IO Config
parseConfig = pure $ Config { bind_address = "0.0.0.0:8888" }
-}
