{-# LANGUAGE DataKinds, FlexibleInstances, FunctionalDependencies, RecordWildCards, OverloadedStrings, TemplateHaskell, TypeApplications #-}

module Promigrate.Up (migrateUp) where

import Amazonka (discover, newEnv)
import Amazonka.DynamoDB
import Control.Lens (_Just)
import Data.Generics.Product
import Data.Hashable
import Prelude (putStrLn)
import Promigrate.IO
import Prosumma
import Prosumma.AWS
import Prosumma.PG
import Prosumma.Settings
import RIO
import Text.Printf

import qualified RIO.HashMap as HashMap
import qualified RIO.Set as Set

data Variable = Variable {
  name :: !Text,
  value :: !Text,
  file :: !(Maybe Text)
} deriving Show

makeLensesWith addL ''Variable

instance Eq Variable where
  v1 == v2 = v1^.nameL == v2^.nameL

instance Ord Variable where
  v1 <= v2 = v1^.nameL <= v2^.nameL

instance Hashable Variable where
  hashWithSalt salt v = hashWithSalt salt (v^.nameL)

scanMigrationSettings
  :: (HasAWSEnv env, MonadReader env m, MonadThrow m, MonadUnliftIO m)
  => Text -> m (Text, String, String)
scanMigrationSettings table = scanSettings table $ \read ->
  (,,) <$> read "connection string" <*> read "migration schema" <*> (read "migration table" ??~ "__migration")

data MigrationParameters = MigrationParameters {
  connectionString :: !Text,
  metadataSchema   :: !String,
  metadataTable    :: !String,
  variables        :: !(Set Variable)
} deriving Show

makeLensesWith addL ''MigrationParameters

newtype VariableScanException = VariableScanException Int deriving Show
instance Exception VariableScanException

scanVariables :: Text -> RIO AWS (Set Variable)
scanVariables table = do
  response <- sendAWS $ newScan $ table <> ".variables"
  let httpStatus = response ^. (field @"httpStatus")
  if httpStatus == 401 
    then return $ Set.fromList []
    else if httpStatus `notElem` [200..299]
      then throwIO $ VariableScanException httpStatus 
      else return $ variables $ response ^. (field @"items") . _Just
  where
    variables :: [HashMap Text AttributeValue] -> Set Variable 
    variables [] = mempty
    variables (row:rows) = getRow <> variables rows 
      where
        getRow = case lookupText "name" of 
          Just name -> case lookupText "value" of 
            Just value -> Set.fromList [Variable name value (lookupText "file")]
            _other -> mempty
          _other -> mempty
        lookupText :: Text -> Maybe Text
        lookupText key = HashMap.lookup key row >>= getText
        getText :: AttributeValue -> Maybe Text
        getText (S text) = Just text
        getText _other = Nothing

migrationDDL :: String
migrationDDL = "\
\CREATE TABLE %s.%s (\
\   stamp CHAR(16) NOT NULL PRIMARY KEY\
\,  migrated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP\
\)"

getMigrationParameters :: Text -> RIO AWS MigrationParameters 
getMigrationParameters table = do 
  (connectionString, metadataSchema, metadataTable) <- scanMigrationSettings table 
  variables <- scanVariables table 
  return $ MigrationParameters{..}

getMigrationParametersTable :: Maybe Text -> RIO LogFunc Text 
getMigrationParametersTable (Just table) = return table
getMigrationParametersTable Nothing = envString Nothing "PROMIGRATE_SETTINGS_TABLE"

createDatabaseIfNeeded :: String -> RIO (PG Connection) ()
createDatabaseIfNeeded database = do
  found <- value1 "SELECT EXISTS (SELECT datname FROM pg_database WHERE datname = ?)" (Only database)
  if found
    then return ()
    else void $ execute_ $ fromString $ "CREATE DATABASE " <> database

createSchemaIfNeeded :: String -> RIO (PG Connection) ()
createSchemaIfNeeded schema = do
  found <- value1 "SELECT EXISTS (SELECT nspname FROM pg_namespace WHERE nspname = ?)" (Only schema)
  if found
    then return ()
    else void $ execute_ $ fromString $ "CREATE SCHEMA " <> schema

createMigrationTableIfNeeded :: String -> String -> RIO (PG Connection) ()
createMigrationTableIfNeeded schema table = do
  found <- value1 "SELECT EXISTS (SELECT c.relname FROM pg_class c INNER JOIN pg_namespace n ON n.oid = c.relnamespace WHERE c.relname = ? AND n.nspname = ?)" (table, schema)
  if found
    then return ()
    else void $ execute_ $ fromString $ printf migrationDDL schema table

getMaxStamp :: String -> String -> RIO (PG Connection) (Maybe String)
getMaxStamp metadataSchema metadataTable = value1_ $ fromString $ printf "SELECT MAX(stamp) FROM %s.%s" metadataSchema metadataTable 

data InvalidConnectionString = InvalidConnectionString deriving Show
instance Exception InvalidConnectionString

initialize :: MigrationParameters -> RIO LogFunc (Maybe String) 
initialize MigrationParameters{..} = case parseConnectInfo connectionString of 
  Left _ -> throwIO InvalidConnectionString
  Right connectInfo -> do
    logFunc <- asks (^.logFuncL)
    let database = connectDatabase connectInfo
    let initConnectInfo = connectInfo{connectDatabase="postgres"}
    initConn <- liftIO $ connect initConnectInfo
    runRIO (PG initConn logFunc) $ createDatabaseIfNeeded database
    databaseConn <- liftIO $ connect connectInfo
    runRIO (PG databaseConn logFunc) $ do
      createSchemaIfNeeded metadataSchema
      createMigrationTableIfNeeded metadataSchema metadataTable
      getMaxStamp metadataSchema metadataTable

migrateUp :: Maybe FilePath -> Maybe Text -> RIO LogFunc ()
migrateUp maybeMigrationsDirectory maybeMigrationParametersTable = do 
  migrationParametersTable <- getMigrationParametersTable maybeMigrationParametersTable 
  logFunc <- asks (^.logFuncL)
  env <- liftIO $ newEnv discover
  migrationParameters <- runRIO (AWS env logFunc) $ getMigrationParameters migrationParametersTable
  migrationsDirectory <- getMigrationsDirectory maybeMigrationsDirectory
  maybeMaxStamp <- initialize migrationParameters
  liftIO $ do
    putStrLn migrationsDirectory
    for_ maybeMaxStamp putStrLn