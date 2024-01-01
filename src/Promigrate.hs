{-# LANGUAGE OverloadedStrings #-}

module Promigrate (
  getMigrationDirectory
) where

import Prosumma
import RIO

getMigrationDirectory :: MonadIO m => Maybe FilePath -> m FilePath
getMigrationDirectory (Just path) = return path
getMigrationDirectory Nothing = envString (Just ".") "PROMIGRATE_MIGRATIONS_DIRECTORY"