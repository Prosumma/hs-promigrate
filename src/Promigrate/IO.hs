{-# LANGUAGE OverloadedStrings #-}

module Promigrate.IO (
  getMigrationsDirectory,
  migrationRegex
) where

import Prosumma
import RIO
import Text.Regex.TDFA

migrationRegex :: Text
migrationRegex = "^[0-9]{16}\\.[^.]+\\.up\\.psql$"

getMigrationsDirectory :: MonadIO m => Maybe FilePath -> m FilePath
getMigrationsDirectory (Just path) = return path
getMigrationsDirectory Nothing = envString (Just ".") "PROMIGRATE_MIGRATIONS_DIRECTORY"