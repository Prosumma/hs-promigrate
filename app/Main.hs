{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Database.PostgreSQL.Simple (connect, connectDatabase)
import Prelude (putStrLn)
import Prosumma.PG
import Prosumma.Util.Environment
import RIO

-- PROMIGRATE_CONNECTION_STRING
-- PROMIGRATE_DATABASE
-- PROMIGRATE_METADATA_SCHEMA
-- PROMIGRATE_METADATA_TABLE

put :: a -> [a] -> [a]
put x xs = xs ++ [x]

main :: IO ()
main = do 
  database <- envString Nothing "PROMIGRATE_DATABASE"
  let connectionString = "host=xyz database=postgres"
  for_ (parseConnectInfo connectionString) $ \connectInfo -> do
    let ci = connectInfo{connectDatabase=database}
    conn <- connect connectInfo
    liftIO $ putStrLn "ok"