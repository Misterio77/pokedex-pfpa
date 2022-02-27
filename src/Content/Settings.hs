{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Content.Settings where

import Control.Exception as Exception
import Data.Aeson
import Data.ByteString
import Data.FileEmbed
import Data.Yaml
import Database.Persist.Postgresql

data Settings = Settings
  { port :: Int,
    databaseConfig :: PostgresConf,
    sessionTimeout :: Int,
    sessionFilePath :: FilePath,
    copyright :: Maybe String
  }

configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile "config/settings.yml")

configSettingsYmlValue :: Value
configSettingsYmlValue =
  either Exception.throw id $
    decodeEither' configSettingsYmlBS

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \obj -> do
    port <- obj .: "port"
    databaseConfig <- obj .: "database"
    sessionTimeout <- (obj .: "session") >>= (.: "timeout")
    sessionFilePath <- (obj .: "session") >>= (.: "file-path")
    copyright <- obj .:? "copyright"

    return Settings {..}
