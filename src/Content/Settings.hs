{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Content.Settings where

import Data.Aeson
import Database.Persist.Postgresql

data Settings = Settings
  { port :: Int,
    staticDir :: String,
    databaseConfig :: PostgresConf,
    sessionTimeout :: Int,
    sessionFilePath :: FilePath,
    copyright :: Maybe String
  }

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \obj -> do
    port <- obj .: "port"
    staticDir <- obj .: "static-dir"
    databaseConfig <- obj .: "database"
    sessionTimeout <- (obj .: "session") >>= (.: "timeout")
    sessionFilePath <- (obj .: "session") >>= (.: "file-path")
    copyright <- obj .:? "copyright"

    return Settings {..}
