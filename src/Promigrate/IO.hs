{-# LANGUAGE OverloadedStrings #-}

module Promigrate.IO (
  getMigrationsDirectory
) where

import Prosumma
import RIO

getMigrationsDirectory :: MonadIO m => Maybe FilePath -> m FilePath
getMigrationsDirectory (Just path) = return path
getMigrationsDirectory Nothing = envString (Just ".") "PROMIGRATE_MIGRATIONS_DIRECTORY"