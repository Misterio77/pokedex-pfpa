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
import Yesod (toWaiAppPlain)
import Yesod.Default.Config2 (loadYamlSettingsArgs, useEnv)

main :: IO ()
main = do
  -- Ler configurações do arquivo embedado em tempo de compilação, overridável
  -- por variáveis de ambiente
  Settings {..} <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv

  PostgresConf {..} <- return databaseConfig

  connectionPool <-
    runStdoutLoggingT $
      createPostgresqlPool pgConnStr pgPoolSize

  runSqlPersistMPool (runMigration migrateAll) connectionPool

  -- Esse middleware torna possível chamar rotas PUT, DELETE, etc usando POST
  -- e uma query string (_method). Tornando as rotas mais semânticas.
  application <- toWaiAppPlain $ App {..}

  run port $ methodOverride application
