{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE Rank2Types                #-}

module DBAccess
       ( initializeDB
       , addName
       , getAllNames
       , getAllPrefix
       , getAllNotPrefix
       , DBConnection) where

import Config
import Control.Exception
import Database.PostgreSQL.Simple hiding (connect)
import Data.ByteString.Char8
import Data.Int

data DBConnection = DBConnection {
  _query :: (ToRow q, FromRow r) => Query -> q -> IO [r],
  _execute :: ToRow q => Query -> q -> IO Int64
}

-- TODO: Extract to util module and newtype
-- | Error codes, see <http://www.postgresql.org/docs/9.3/static/errcodes-appendix.html>
type SqlState = ByteString

-- | Internal method for creating the connection
connect :: Config -> IO DBConnection
connect conf = fmap (\connection -> DBConnection {
  _query = query connection,
  _execute = execute connection
  }) conn
  where
    conn = connectPostgreSQL $ pack $ postgresConnectString conf

duplicateTableSqlState :: SqlState
duplicateTableSqlState = "42P07"

duplicateIndexSqlState :: SqlState
duplicateIndexSqlState = "42P07"

uniqueViolationSqlState :: SqlState
uniqueViolationSqlState = "23505"

-- | Internal utility method for ignoring a specific sql error
runIgnoring :: SqlState -- ^ The error to ignore
            -> a        -- ^ The default value to use if the specified error is thrown
            -> IO a     -- ^ The action over which to ignore the error
            -> IO a
runIgnoring state defaultRes query = catchJust handleError query (\x -> return defaultRes)
  where
    handleError (e :: SqlError)
      | sqlState e == state = Just ()
      | otherwise           = Nothing

-- | Connects to the database and ensures that the correct tables and indicies are extant
initializeDB :: Config -> IO DBConnection
initializeDB conf = do
  conn <- connect conf
  runIgnoring duplicateTableSqlState 0 $ _execute conn createQuery ()
  runIgnoring duplicateIndexSqlState 0 $ _execute conn indexQuery ()
  return conn
    where
      createQuery = " CREATE TABLE ids (          \
                    \   id   SERIAL PRIMARY KEY,  \
                    \   name TEXT NOT NULL UNIQUE)"
      indexQuery  = "CREATE INDEX ids_name_char_ops_idx ON ids (name varchar_pattern_ops)"

-- TODO: Swap first and second arg for currying
-- TODO: Add ID returning
-- | Adds a name to the database
addName :: DBConnection              -- ^ Connection to the database
        -> ByteString                 -- ^ The name to add
        -> IO (Either SqlError Int64) -- ^ Number of names added
addName conn name = try $ runIgnoring uniqueViolationSqlState 0 $ _execute conn "INSERT INTO ids(name) VALUES (?)" (Only name)

-- | Extracts results from the 'Only' container the library puts them in
getResults :: [Only ByteString] -> [ByteString]
getResults = fmap (\(Only x) -> x)

-- | Runs a query and extracts results
runQuery :: ToRow a =>
            Query -- ^ Query to run
            -> a  -- ^ Parameters for the query
            -> DBConnection -- ^ Connection on which to run the query
            -> IO (Either SqlError [ByteString])
runQuery sql params conn = try $ do
  result <- _query conn sql params
  return $ getResults result

-- | Reads all names from the database
getAllNames :: DBConnection -> IO (Either SqlError [ByteString])
getAllNames = runQuery "SELECT name FROM ids" ()

-- | Reads all names with a particular prefix from the database
getAllPrefix :: DBConnection                     -- ^ Connection to the database
             -> ByteString                        -- ^ Prefix to check
             -> IO (Either SqlError [ByteString]) -- ^ Error or resulting names
getAllPrefix conn prefix = runQuery "SELECT name FROM ids WHERE name LIKE '?%'" (Only prefix) conn

-- | Reads all names not conforming to said prefix from the database
getAllNotPrefix :: DBConnection                     -- ^ Connection to the database
                -> ByteString                        -- ^ Prefix to check
                -> IO (Either SqlError [ByteString]) -- ^ Error or resulting names
getAllNotPrefix conn prefix = runQuery "SELECT name FROM ids WHERE name NOT LIKE '?%'" (Only prefix) conn
