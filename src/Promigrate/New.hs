{-# LANGUAGE OverloadedStrings #-}

module Promigrate.New (newMigration) where

import Data.Time.Clock.POSIX
import Formatting
import Promigrate.IO
import Prosumma
import RIO
import RIO.Time
import RIO.FilePath
import Text.Printf

getStamp :: MonadIO m => m String
getStamp = do
  time <- posixSecondsToUTCTime <$> liftIO getPOSIXTime
  return $ take 16 $ formatTime defaultTimeLocale "%Y%m%d%H%M%S%q" time

newMigration :: String -> Maybe String -> RIO LogFunc ()
newMigration hint path = do 
  stamp <- getStamp
  let filename = printf "%s.%s.up.psql" stamp hint
  migrationDirectory <- getMigrationsDirectory path
  let filepath = migrationDirectory </> filename
  writeFileUtf8 filepath ""
  logInfo $ uformat ("Created migration " % string % ".") filename
