module Config (Config, readConfig) where

import System.Posix.Env
import Data.Maybe

data Config = Config { postgresConnectString :: String }

readConfig :: IO Config
readConfig = do
  connStr <- getEnv "ANDB_PG_CONNECT_STRInG"
  fromMaybe "dbname=postgres" connStr
