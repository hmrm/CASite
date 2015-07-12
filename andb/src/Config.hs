{-# LANGUAGE ScopedTypeVariables #-}

module Config (Config(postgresConnectString), readConfig) where

import System.Posix.Env
import Data.Maybe

-- | The full configuration of the application
data Config = Config {
  -- | Specification for connection to postgres, see <http://www.postgresql.org/docs/9.3/static/libpq-connect.html#LIBPQ-CONNSTRING>
  postgresConnectString :: String
  } deriving (Show)

defaultConnectString :: String
defaultConnectString = "dbname=postgres"

-- | The environment variable the value of which, if set, will be used for the connect string
connectStringEnvVar :: String
connectStringEnvVar = "ANDB_PG_CONNECT_STRING"

-- | Reads environment variables to construct a config object
-- >>> setEnvironment [("ANDB_PG_CONNECT_STRING", "test-connect-string")]
-- >>> readConfig >>= print
-- Config {postgresConnectString = "test-connect-string"}
readConfig :: IO Config
readConfig = fmap (\envValue -> Config {
                       postgresConnectString = fromMaybe defaultConnectString envValue
                   }) $ getEnv connectStringEnvVar

             
