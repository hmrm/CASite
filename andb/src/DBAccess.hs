{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DBAccess
       ( initializeDB ) where

import Config
import Control.Exception
import Database.PostgreSQL.Simple hiding (connect)
import Data.ByteString.Char8
import Data.Int

connect :: Config -> IO Connection
connect = connectPostgreSQL . pack . postgresConnectString

duplicateTableSqlState :: ByteString
duplicateTableSqlState = "42P07"

-- TODO: Add index generation
-- TODO: Document
-- TODO: Separate into separate functions
initializeDB :: Config -> IO Connection
initializeDB conf = do
  conn <- connect conf
  catchJust handleExists (runQuery conn) ignoreError
  return conn
    where
      handleExists (e :: SqlError)
        | sqlState e == duplicateTableSqlState = Just ()
        | otherwise                          = Nothing
      runQuery conn = (execute_ conn " CREATE TABLE ids (         \
                                     \   id   SERIAL PRIMARY KEY, \
                                     \   name TEXT NOT NULL)      ")
      ignoreError _ = return (-1)
