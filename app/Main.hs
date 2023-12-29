{-# LANGUAGE DataKinds, OverloadedStrings, ScopedTypeVariables, TypeApplications #-}

module Main (main) where

import Amazonka (discover, newEnv)
import Amazonka.DynamoDB
import Control.Lens (_Just)
import Data.Generics.Product
import Prelude (print, putStrLn)
import Prosumma
import Prosumma.AWS
import Prosumma.Logging
import Prosumma.PG
import Prosumma.Settings
import RIO
import RIO.List
import RIO.Directory
import RIO.List.Partial
import System.Environment
import Text.Printf

import qualified RIO.HashMap as HashMap

migrationDDL :: String
migrationDDL = "\
\CREATE TABLE %s.%s (\
\  id BIGSERIAL NOT NULL PRIMARY KEY\
\)"

scanMigrationSettings
  :: (HasAWSEnv env, MonadReader env m, MonadThrow m, MonadUnliftIO m)
  => Text -> m (Text, String, String)
scanMigrationSettings table = scanSettings table $ \read ->
  (,,) <$> read "connection string" <*> read "migration schema" <*> (read "migration table" ??~ "__migration")

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

initialize :: Text -> RIO AWS ()
initialize settings = do
  logFunc <- asks (^.logFuncL)
  (connectionString, metadataSchema, metadataTable) <- scanMigrationSettings settings
  case parseConnectInfo connectionString of
    Left e -> logError $ displayShow e
    Right connectInfo -> do
      let database = connectDatabase connectInfo
      let initConnectInfo = connectInfo{connectDatabase="postgres"}
      initConn <- liftIO $ connect initConnectInfo
      runRIO (PG initConn logFunc) $ createDatabaseIfNeeded database
      databaseConn <- liftIO $ connect connectInfo
      runRIO (PG databaseConn logFunc) $ do
        createSchemaIfNeeded metadataSchema
        createMigrationTableIfNeeded metadataSchema metadataTable

variables :: [HashMap Text AttributeValue] -> [(Text, Text, Maybe Text)]
variables [] = mempty
variables (row:rows) = getRow row <> variables rows 
  where
    getRow row = case HashMap.lookup "name" row of
      Just (S name) -> case HashMap.lookup "value" row of
        Just (S value) -> [(name, value, Nothing)]
        _other -> mempty
      _other -> mempty

main :: IO ()
main = do
  logOptions <- readLogOptions
  withLogFunc logOptions $ \logFunc -> do
    runRIO logFunc $ do
      settings <- envString Nothing "PROMIGRATE_SETTINGS_TABLE"
      env <- liftIO $ newEnv discover
      let aws = AWS env logFunc
      runRIO aws $ do
        initialize settings 
        response <- sendAWS $ newScan $ settings <> ".variables" 
        liftIO $ print $ response ^. (field @"items")
      migrationDirectory <- getMigrationDirectory
      contents <- sort <$> getDirectoryContents migrationDirectory
      for_ contents (liftIO . putStrLn) 
  where
    readLogOptions = do
      logMinLevel <- envValue (Just LevelDebug) readLogLevel "PROMIGRATE_LOGLEVEL"
      logOptionsHandle stderr True <&> setLogMinLevel logMinLevel
    getMigrationDirectory :: MonadIO m => m FilePath
    getMigrationDirectory = do
      defaultDirectory <- envString (Just ".") "PROMIGRATE_MIGRATIONS_DIRECTORY"
      args <- liftIO getArgs
      return $ if null args
        then defaultDirectory
        else head args