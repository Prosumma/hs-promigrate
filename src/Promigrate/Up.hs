{-# LANGUAGE DataKinds, FlexibleInstances, NamedFieldPuns, RecordWildCards, OverloadedStrings, ScopedTypeVariables, TemplateHaskell, TypeApplications #-}

module Promigrate.Up (migrateUp) where

import Amazonka (discover, newEnv)
import Amazonka.DynamoDB hiding (Query)
import Control.Lens (_Just)
import Data.Generics.Product
import Data.FileEmbed
import Data.Hashable
import Data.String.Conversions
import Formatting
import Promigrate.IO
import Prosumma
import Prosumma.AWS
import Prosumma.PG
import Prosumma.Settings
import RIO
import RIO.Directory
import RIO.FilePath
import RIO.List
import RIO.Process
import Text.Printf
import Text.Regex.TDFA

import qualified RIO.HashMap as HashMap
import qualified RIO.Set as Set
import qualified RIO.Text as Text

migrationRegex :: Text
migrationRegex = "^[0-9]{16}\\.[^.]+\\.up\\.psql$"

data Variable = Variable {
  name :: !Text,
  value :: !Text,
  stamp :: !(Maybe Text)
} deriving Show

nameL :: Lens' Variable Text
nameL = lens name $ \variable name -> variable{name}

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

-- makeLensesWith addL ''MigrationParameters

newtype VariableScanException = VariableScanException Int deriving Show
instance Exception VariableScanException

scanVariables :: Text -> RIO AWS (Set Variable)
scanVariables table = do
  response <- sendAWS $ newScan $ table <> ".variables"
  let httpStatus = response ^. (field @"httpStatus")
  if httpStatus == 404
    then return mempty
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

migrationDDL :: Query 
migrationDDL = $(embedStringFile "res/create_migration_table.psql") 

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
    else void $ execute migrationDDL (schema, table) 

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

getFilteredMigrations :: (FilePath -> Bool) -> FilePath -> RIO LogFunc [FilePath]
getFilteredMigrations predicate migrationsDirectory = map (migrationsDirectory </>) . sort .  filter predicate <$> getDirectoryContents migrationsDirectory

getMigrationsInOrderAfterStamp :: Maybe String -> FilePath -> RIO LogFunc [FilePath]
getMigrationsInOrderAfterStamp Nothing = getFilteredMigrations (=~ migrationRegex)
getMigrationsInOrderAfterStamp (Just stamp) = getFilteredMigrations $ \file -> file =~ migrationRegex && take 16 file > stamp

getVariablesForStamp :: String -> Set Variable -> [Variable]
getVariablesForStamp s = toList . Set.filter isMatch
  where
    isMatch Variable{..} = case stamp of
      Nothing -> True
      Just stamp -> stamp == Text.pack s

migrateUp :: Maybe FilePath -> Maybe Text -> RIO LogFunc ()
migrateUp maybeMigrationsDirectory maybeMigrationParametersTable = do
  migrationParametersTable <- getMigrationParametersTable maybeMigrationParametersTable
  logFunc <- asks (^.logFuncL)
  env <- liftIO $ newEnv discover
  migrationParameters@MigrationParameters{..} <- runRIO (AWS env logFunc) $
    getMigrationParameters migrationParametersTable
  migrationsDirectory <- getMigrationsDirectory maybeMigrationsDirectory
  maybeMaxStamp <- initialize migrationParameters
  migrations <- getMigrationsInOrderAfterStamp maybeMaxStamp migrationsDirectory
  pc <- mkDefaultProcessContext
  conn <- liftIO $ connectPostgreSQL $ convertString connectionString
  let insertion = fromString $ printf "INSERT INTO %s.%s(stamp) SELECT ?" metadataSchema metadataTable
  runRIO (LoggedProcessContext pc logFunc) $
    for_ migrations $ \migration -> do
      logInfo $ uformat ("About to migrate " % string % ".") migration
      let migrationStamp = take 16 $ takeFileName migration
      let vars = join $ map formatVariable $ getVariablesForStamp migrationStamp variables
      let params = [Text.unpack connectionString, "--echo-all", "-v", "ON_ERROR_STOP=1"] <> vars
      proc "psql" params $ \config -> do
        bytes <- readFileBinary migration
        let config' = flip setStdin config $ byteStringInput $ convertString bytes
        runProcess_ config'
        runRIO (PG conn logFunc) $ void $ execute insertion (Only migrationStamp)
      logInfo $ uformat ("Successfully migrated " % string % ".") migration
  liftIO $ close conn
  where
    formatVariable Variable{..} = ["-v", printf "%s=%s" name value]
