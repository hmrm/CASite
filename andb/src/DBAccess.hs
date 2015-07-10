{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DBAccess
       ( initializeDB
       , addName
       , getAllNames
       , getAllPrefix
       , getAllNotPrefix) where

import Config
import Control.Exception
import Database.PostgreSQL.Simple hiding (connect)
import Data.ByteString.Char8
import Data.Int

-- | Internal method for creating the connection
connect :: Config -> IO Connection
connect = connectPostgreSQL . pack . postgresConnectString

-- TODO: Extract to util module and newtype
-- | Error codes, see <http://www.postgresql.org/docs/9.3/static/errcodes-appendix.html>
type SqlState = ByteString

duplicateTableSqlState :: SqlState
duplicateTableSqlState = "42P07"

duplicateIndexSqlState :: SqlState
duplicateIndexSqlState = "42P07"

uniqueViolationSqlState :: SqlState
uniqueViolationSqlState = "23505"

-- | Internal utility method for ignoring a specific sql error
ignoreError :: SqlState -- ^ The error to ignore
            -> a        -- ^ The default value to use if the specified error is thrown
            -> IO a     -- ^ The action over which to ignore the error
            -> IO a
ignoreError state defaultRes query = catchJust handleError query (\x -> return defaultRes)
  where
    handleError (e :: SqlError)
      | sqlState e == state = Just ()
      | otherwise           = Nothing

-- TODO: Add index generation
-- TODO: Separate into separate functions
-- | Connects to the database and ensures that the correct tables and indicies are extant
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
-- | Adds a name to the database
addName :: Connection -- ^ Connection to the database
        -> ByteString -- ^ The name to add
        -> IO Int64   -- ^ Number of names added
addName conn name = ignoreError uniqueViolationSqlState 0 $ execute conn "INSERT INTO ids(name) VALUES (?)" (Only name)

-- | Reads all names from the database
getAllNames :: Connection -> IO (Either SqlError [ByteString])
getAllNames conn = try $ do
  result <- query_ conn "SELECT name FROM ids"
  return $ fmap (\(Only x) -> x) result

-- | Reads all names with a particular prefix from the database
getAllPrefix :: Connection                        -- ^ Connection to the database
             -> ByteString                        -- ^ Prefix to check
             -> IO (Either SqlError [ByteString]) -- ^ Error or resulting names
getAllPrefix conn prefix = try $ do
  result <- query conn "SELECT name FROM ids WHERE name LIKE '?%'" (Only prefix)
  return $ fmap (\(Only x) -> x) result

-- | Reads all names not conforming to said prefix from the database
getAllNotPrefix :: Connection                        -- ^ Connection to the database
                -> ByteString                        -- ^ Prefix to check
                -> IO (Either SqlError [ByteString]) -- ^ Error or resulting names
getAllNotPrefix conn prefix = try $ do
  result <- query conn "SELECT name FROM ids WHERE name NOT LIKE '?%'" (Only prefix)
  return $ fmap (\(Only x) -> x) result
