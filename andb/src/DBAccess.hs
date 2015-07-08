{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DBAccess
       ( initializeDB
       , addNames
       , getAllNames) where

import Config
import Control.Exception
import Database.PostgreSQL.Simple hiding (connect)
import Data.ByteString.Char8
import Data.Int

connect :: Config -> IO Connection
connect = connectPostgreSQL . pack . postgresConnectString

duplicateTableSqlState :: ByteString
duplicateTableSqlState = "42P07"

uniqueViolationSqlState :: ByteString
uniqueViolationSqlState = "23505"

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
        | otherwise                            = Nothing
      runQuery conn = (execute_ conn " CREATE TABLE ids (          \
                                     \   id   SERIAL PRIMARY KEY,  \
                                     \   name TEXT NOT NULL UNIQUE)")
      ignoreError _ = return (-1)

addNames :: Connection -> [ByteString] -> IO Int64
addNames conn names = catchJust handleDuplicate (runQuery conn) ignoreError
  where
    handleDuplicate (e :: SqlError)
      | sqlState e == uniqueViolationSqlState = Just ()
      | otherwise                             = Nothing
    runQuery conn = executeMany conn "INSERT INTO ids(name) VALUES (?)" (fmap Only names)
    ignoreError _ = return (-1)

getAllNames :: Connection -> IO (Either SqlError [ByteString])
getAllNames conn = try $ do
  result <- query_ conn "SELECT name FROM ids"
  return $ fmap (\(Only x) -> x) result
