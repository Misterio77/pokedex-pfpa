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
import Yesod.Default.Config2

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

compileTimeAppSettings :: Settings
compileTimeAppSettings =
  case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
    Error e -> error e
    Success settings -> settings

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \obj -> do
    port <- obj .: "port"
    databaseConfig <- obj .: "database"
    sessionTimeout <- (obj .: "session") >>= (.: "timeout")
    sessionFilePath <- (obj .: "session") >>= (.: "file-path")
    copyright <- obj .:? "copyright"

    return Settings {..}
