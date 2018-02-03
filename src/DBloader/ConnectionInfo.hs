{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-
Module              : DBinfors
Description         : Defined type ConnectionInfo and connectionInfo
                      which represent the connection information.
Maintainer          : emmettng@gmail.com
-}

module DBloader.ConnectionInfo
    (
      connectionInfoJSON                --- a list of JSON objects
    , verifyConnectionInfoJSON          ---  presumablly only one object in a list
    , JSONConnectionInfo (..)
    , DBname
    , CollectionName
    , jsonConfig
    , jsonConn
    ) where

import Control.Monad                          -- import guard
import Data.Text as DT                        -- Used for parsing JSON
import Data.Aeson                             -- Parsing JSON
import qualified Data.ByteString.Lazy as B    -- read config file
import GHC.Generics                           -- cooperate with Aeson

-- | Represent JSON object : JSONConnectionInfo
data JSONConnectionInfo =
    ConnectionInfo { hostname :: Text
                   , usr :: Text
                   , pwd :: Text
                   , host :: Text
                   , database :: Text
                   , port :: Int
                 } deriving (Show,  Generic)

instance FromJSON JSONConnectionInfo
instance ToJSON JSONConnectionInfo
-- $(deriveJSON defaultOptions ''ConnectionInfo)

type DBname = String
type CollectionName = Text
------------------------------------------------
-- | Define the JSON file path which follow the JSON format as defined in JSONConnectionInfo
jsonConfigPath= "./src/DBloader/dbhosts.json"


----------------------------------------------
listJSON :: IO B.ByteString
listJSON = B.readFile jsonConfigPath

-- | Define JSON as a list of JSON object :: JSONConnectionInfo
connectionInfoJSON :: IO (Either String [JSONConnectionInfo])
connectionInfoJSON = do
    bJSON<- listJSON
    return $ eitherDecode  bJSON

-- | Chech JSON objects in connectionInfoJSON contains hostname == DBname
verifyConnectionInfoJSON ::
                            DBname                             -- name of targe DB
                            -> [JSONConnectionInfo]    -- list of JSON objects
                            -> Either String [JSONConnectionInfo]               -- list of targe json object
verifyConnectionInfoJSON dbname coninfoList=
    let rlist = do
                con <- coninfoList
                guard ( DT.unpack (hostname con)== dbname)
                return con
    in case rlist of
        [] -> Left "No corresponding database!"
        _ -> Right rlist
--verifyConnectionInfoJSON :: Either String [JSONConnectionInfo]    -- list of JSON objects
--                            -> DBname                             -- name of targe DB
--                            -> [JSONConnectionInfo]               -- list of targe json object
--verifyConnectionInfoJSON (Right coninfoList) dbname = do
--    con <- coninfoList
--    guard ( DT.unpack (hostname con)== dbname)
--    return con
--verifyConnectionInfoJSON (Left _) _ = []


-- | Decouple the db config json file
-- Currently, this interface is not necessary.
jsonConfig :: String -> IO B.ByteString
jsonConfig = B.readFile

jsonConn ::  String -> IO (Either String [JSONConnectionInfo])
jsonConn path = do
    bJSON <- jsonConfig path
    return $ eitherDecode bJSON
