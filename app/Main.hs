{-# LANGUAGE DataKinds, FlexibleInstances, FunctionalDependencies, OverloadedStrings, ScopedTypeVariables, TemplateHaskell, TypeApplications #-}

module Main (main) where

import Amazonka (discover, newEnv)
import Amazonka.DynamoDB
import Data.Generics.Product
import Data.Hashable
import Data.Time.Clock.POSIX
import Options.Applicative hiding (HasName, HasValue)
import Prelude (print, putStrLn)
import Prosumma
import Prosumma.AWS
import Prosumma.Logging
import Prosumma.PG
import Prosumma.Settings
import RIO
import RIO.List
import RIO.Directory
import RIO.File
import RIO.FilePath
import RIO.Time
import Text.Printf

import qualified RIO.HashMap as HashMap
import qualified RIO.Set as Set

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

getStamp :: IO String
getStamp = do
  time <- posixSecondsToUTCTime <$> getPOSIXTime
  return $ take 16 $ formatTime defaultTimeLocale "%Y%m%d%H%M%S%q" time 

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

data Command
  = Up (Maybe FilePath)
  | New String (Maybe FilePath)
  deriving (Show)

upCommand :: Parser Command
upCommand = Up <$> optional (strArgument (metavar "PATH" <> help "Path to migrate, defaults to current directory"))

newCommand :: Parser Command
newCommand = New 
    <$> strArgument (metavar "DESCRIPTION" <> help "Description of the migration")
    <*> optional (strArgument (metavar "PATH" <> help "Path for the migration, defaults to current directory"))

commandParser :: Parser Command
commandParser = subparser
    ( command "up" (info upCommand (progDesc "Run migrations up to the latest"))
   <> command "new" (info newCommand (progDesc "Create a new migration"))
    )

main :: IO ()
main = do
  command <- customExecParser (prefs showHelpOnError) $ info (commandParser <**> helper)
    ( fullDesc
    <> progDesc "Prosumma PostgreSQL Migration Tool"
    <> header "promigrate - a Prosumma PostgreSQL migration tool"
    )
  logOptions <- readLogOptions
  withLogFunc logOptions $ \logFunc -> do
    runRIO logFunc $ do
      case command of
        New hint path -> do
          stamp <- liftIO getStamp
          let filename = printf "%s.%s.up.psql" stamp hint
          migrationDirectory <- getMigrationDirectory path
          let filepath = migrationDirectory </> filename
          writeFileUtf8 filepath ""
        Up path -> do
          settings <- envString Nothing "PROMIGRATE_SETTINGS_TABLE"
          env <- liftIO $ newEnv discover
          let aws = AWS env logFunc
          runRIO aws $ do
            initialize settings 
            response <- sendAWS $ newScan $ settings <> ".variables" 
            liftIO $ print $ response ^. (field @"items")
          migrationDirectory <- getMigrationDirectory path
          contents <- sort <$> getDirectoryContents migrationDirectory
          for_ contents (liftIO . putStrLn) 
  where
    readLogOptions = do
      logMinLevel <- envValue (Just LevelDebug) readLogLevel "PROMIGRATE_LOGLEVEL"
      logOptionsHandle stderr True <&> setLogMinLevel logMinLevel
    getMigrationDirectory :: MonadIO m => Maybe FilePath -> m FilePath
    getMigrationDirectory (Just path) = return path
    getMigrationDirectory Nothing = envString (Just ".") "PROMIGRATE_MIGRATIONS_DIRECTORY"
