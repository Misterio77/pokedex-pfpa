{-# LANGUAGE RecordWildCards #-}

module Main where

import Application ()
import Content.Settings
import Control.Monad.Logger
import Database.Models
import Database.Persist.Postgresql
import Foundation
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.MethodOverride (methodOverride)
import Yesod
import Yesod.Default.Config2

main :: IO ()
main = do
  Settings {..} <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv

  PostgresConf {..} <- return databaseConfig

  connectionPool <-
    runStdoutLoggingT $
      createPostgresqlPool pgConnStr pgPoolSize

  runSqlPersistMPool (runMigration migrateAll) connectionPool

  application <- toWaiAppPlain $ App {..}

  run port $ methodOverride application
