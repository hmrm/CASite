{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DBAccess
       ( initializeDB
       , addName
       , getAllNames) where

import Config
import Control.Exception
import Database.PostgreSQL.Simple hiding (connect)
import Data.ByteString.Char8
import Data.Int

connect :: Config -> IO Connection
connect = connectPostgreSQL . pack . postgresConnectString

-- TODO: Extract to util module and newtype
type SqlState = ByteString

duplicateTableSqlState :: SqlState
duplicateTableSqlState = "42P07"

duplicateIndexSqlState :: SqlState
duplicateIndexSqlState = "42P07"

uniqueViolationSqlState :: SqlState
uniqueViolationSqlState = "23505"

ignoreError :: SqlState -> a -> IO a -> IO a
ignoreError state defaultRes query = catchJust handleError query (\x -> return defaultRes)
  where
    handleError (e :: SqlError)
      | sqlState e == state = Just ()
      | otherwise           = Nothing

-- TODO: Add index generation
-- TODO: Document
-- TODO: Separate into separate functions
initializeDB :: Config -> IO Connection
initializeDB conf = do
  conn <- connect conf
  ignoreError duplicateTableSqlState 0 $ execute_ conn createQuery
  ignoreError duplicateIndexSqlState 0 $ execute_ conn indexQuery
  return conn
    where
      createQuery = " CREATE TABLE ids (          \
                    \   id   SERIAL PRIMARY KEY,  \
                    \   name TEXT NOT NULL UNIQUE)"
      indexQuery  = "CREATE INDEX ids_name_char_ops_idx ON ids (name varchar_pattern_ops)"

-- TODO: Swap first and second arg for currying
-- TODO: Add ID returning
addName :: Connection -> ByteString -> IO Int64
addName conn name = ignoreError uniqueViolationSqlState 0 $ execute conn "INSERT INTO ids(name) VALUES (?)" (Only name)

getAllNames :: Connection -> IO (Either SqlError [ByteString])
getAllNames conn = try $ do
  result <- query_ conn "SELECT name FROM ids"
  return $ fmap (\(Only x) -> x) result

getAllPrefix :: Connection -> ByteString -> IO (Either SqlError [ByteString])
getAllPrefix conn prefix = try $ do
  result <- query conn "SELECT name FROM ids WHERE name LIKE '?%'" (Only prefix)
  return $ fmap (\(Only x) -> x) result

getAllNotPrefix :: Connection -> ByteString -> IO (Either SqlError [ByteString])
getAllNotPrefix conn prefix = try $ do
  result <- query conn "SELECT name FROM ids WHERE name NOT LIKE '?%'" (Only prefix)
  return $ fmap (\(Only x) -> x) result
