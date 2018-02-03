{-# LANGUAGE OverloadedStrings #-}
module DBloader.DBloader
    (
      getSQLdbconByName
    , getSQLstatement
    , getNaiveSQLstatment
    , getMongoRunnerSimple
    , getMongoPipe
    , DBname
    , connectionInfoJSON
    , M.count
    , M.select
    ) where

import Data.Text
import Database.HDBC
import Database.HDBC.MySQL
import Database.MongoDB as M
import Control.Monad.IO.Class

-- import inner library : abstraction of db conection informations
import DBloader.ConnectionInfo as I

-- | Map SQL DB name to its connection
--   abstract away the storage of DB connection
getSQLdbconByName :: String -> IO Connection
getSQLdbconByName dbname = do
    coninfoJSON <- connectionInfoJSON
    getSQLdbconJSON coninfoJSON dbname

-- | get a SQL statment which could be executed later.
getSQLstatement :: DBname -> String -> IO Statement
getSQLstatement dbname statement = do
    con <- getSQLdbconByName dbname
    prepare con statement

getNaiveSQLstatment :: Connection -> String -> IO Statement
getNaiveSQLstatment = prepare

getMongoRunnerSimple :: MonadIO m => DBname -> CollectionName -> Action m a -> m a
getMongoRunnerSimple dbname collectionName act =  do
    conInfo <- liftIO connectionInfoJSON
    pipe <- liftIO $ getMongoPipe conInfo dbname
    access pipe master collectionName act
{-
#
Auxiliary function for getting MySQL connection
#
-}
-- | get SQL connection with a given DB host name
-- The exception part "error" needs to be improved
getSQLdbconJSON :: Either String [JSONConnectionInfo] -> DBname -> IO Connection
getSQLdbconJSON eitherConnInfo dbName = connectEither jsonInfo
    where jsonInfo = eitherConnInfo >>= verifyConnectionInfoJSON dbName
          connectEither :: Either String [JSONConnectionInfo] -> IO Connection
          connectEither (Left msg) = fail msg
          connectEither (Right (x:_)) = connectMySQL defaultMySQLConnectInfo{
                                          mysqlHost = unpack $ I.host x,
                                          mysqlUser = unpack $ usr x,
                                          mysqlDatabase = unpack $ database x,
                                          mysqlPassword = unpack $ pwd x
                                        }
--getSQLdbconJSON :: Either String [JSONConnectionInfo] -> DBname -> IO Connection
--getSQLdbconJSON eitherConnInfo dbName =
--    let connInfo = verifyConnectionInfoJSON eitherConnInfo dbName
--    in connect2 connInfo
--      where connect2 :: [JSONConnectionInfo] -> IO Connection
--            connect2 [] = fail "No valid connection information!"
--            connect2 (x:_) =  connectMySQL defaultMySQLConnectInfo{
--              mysqlHost = unpack $ I.host x,
--              mysqlUser = unpack $ usr x,
--              mysqlDatabase = unpack $ database x,
--              mysqlPassword = unpack $ pwd x
--          }
{-
#
Auxiliar function for get mongoDB pipe
#
-}
-- The exception part "error" needs to be improved
getMongoPipe:: Either String [JSONConnectionInfo] -> DBname -> IO Pipe
getMongoPipe eitherConnInfo dbName =
    getPipe connInfo where
          getPipe:: Either String [JSONConnectionInfo] -> IO Pipe
          getPipe (Left msg)= fail "No valid connection information!"
          getPipe (Right(x:_))=  connect . M.host $ unpack . I.host $ x
          connInfo = do
              e <- eitherConnInfo
              verifyConnectionInfoJSON dbName e
