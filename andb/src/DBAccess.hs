module DBAccess
       ( initializeDB ) where

import Database.PostgreSQL.Simple

initializeDB :: IO Connection
