{-# LANGUAGE RecordWildCards #-}

module Main where

import Application ()
import Content.Settings
import Control.Monad.Logger
import Data.Yaml.Config
import Database.Models
import Database.Persist.Postgresql
import Foundation
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.MethodOverride (methodOverride)
import Yesod
import Yesod.Static

main :: IO ()
main = do
  Settings {..} <- loadYamlSettings ["config/settings.yml"] [] useEnv

  staticContent <- static staticDir

  PostgresConf {..} <- return databaseConfig

  connectionPool <-
    runStdoutLoggingT $
      createPostgresqlPool pgConnStr pgPoolSize

  runSqlPersistMPool (runMigration migrateAll) connectionPool

  application <- toWaiAppPlain $ App {..}

  run port $ methodOverride application
