{-# LANGUAGE ScopedTypeVariables #-}

module Config (Config(postgresConnectString), readConfig) where

import System.Posix.Env
import Data.Maybe

data Config = Config { postgresConnectString :: String } deriving (Show)

defaultConnectString :: String
defaultConnectString = "dbname=postgres"

connectStringEnvVar :: String
connectStringEnvVar = "ANDB_PG_CONNECT_STRING"

readConfig :: IO Config
readConfig = fmap (\envValue -> Config { postgresConnectString = fromMaybe defaultConnectString envValue }) $ getEnv connectStringEnvVar

             
