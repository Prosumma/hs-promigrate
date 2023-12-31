{-# LANGUAGE DataKinds, FlexibleInstances, OverloadedStrings, ScopedTypeVariables #-}

module Main (main) where

import Options.Applicative hiding (HasName, HasValue)
import Promigrate.New
import Promigrate.Up
import Prosumma
import Prosumma.Logging
import RIO

data Command
  = Up (Maybe FilePath) (Maybe Text)
  | New String (Maybe FilePath)
  deriving (Show)

upCommand :: Parser Command
upCommand = Up
    <$> optional (strArgument (metavar "PATH" <> help "Path to migrate, defaults to current directory"))
    <*> optional (strOption (long "settings" <> short 's' <> metavar "SETTINGS" <> help "Name of DynamoDB settings table"))

newCommand :: Parser Command
newCommand = New
    <$> strArgument (metavar "DESCRIPTION" <> help "Description of the migration")
    <*> optional (strArgument (metavar "PATH" <> help "Path for the migration, defaults to current directory"))

commandParser :: Parser Command
commandParser = subparser
    (  command "up" (info upCommand (progDesc "Run migrations up to the latest"))
    <> command "new" (info newCommand (progDesc "Create a new migration"))
    )

main :: IO ()
main = do
  command <- customExecParser (prefs showHelpOnError) $ info (commandParser <**> helper)
    (  fullDesc
    <> progDesc "Prosumma PostgreSQL Migration Tool"
    <> header "promigrate - a Prosumma PostgreSQL migration tool"
    )
  logOptions <- readLogOptions
  withLogFunc logOptions $ \logFunc -> do
    runRIO logFunc $ do
      case command of
        New hint path -> newMigration hint path 
        Up maybeMigrationsDirectory maybeMigrationParametersTable ->
          migrateUp maybeMigrationsDirectory maybeMigrationParametersTable
  where
    readLogOptions = do
      logMinLevel <- envValue (Just LevelDebug) readLogLevel "PROMIGRATE_LOGLEVEL"
      logOptionsHandle stderr True <&> setLogMinLevel logMinLevel
