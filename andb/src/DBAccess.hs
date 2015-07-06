module DBAccess
       ( initializeDB ) where

import Config
import Database.PostgreSQL.Simple
import Data.ByteString.Char8

initializeDB :: Config -> IO Connection
initializeDB = connectPostgreSQL . pack . postgresConnectString
    
