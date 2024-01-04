{-# LANGUAGE OverloadedStrings #-}

module Promigrate.New (newMigration) where

import Data.Time.Clock.POSIX
import Promigrate.IO
import RIO
import RIO.Time
import RIO.FilePath
import Text.Printf

getStamp :: IO String
getStamp = do
  time <- posixSecondsToUTCTime <$> getPOSIXTime
  return $ take 16 $ formatTime defaultTimeLocale "%Y%m%d%H%M%S%q" time

newMigration :: MonadIO m => String -> Maybe String -> m ()
newMigration hint path = do 
  stamp <- liftIO getStamp
  let filename = printf "%s.%s.up.psql" stamp hint
  migrationDirectory <- getMigrationsDirectory path
  let filepath = migrationDirectory </> filename
  writeFileUtf8 filepath ""