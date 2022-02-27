{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Data.Text (Text, unpack)
import Database.Models
import Database.Persist.Sql
import Text.Hamlet (hamletFile)
import Text.Read (readMaybe)
import Yesod

data App = App
  { connectionPool :: ConnectionPool,
    sessionTimeout :: Int,
    sessionFilePath :: FilePath
  }

mkYesodData "App" $(parseRoutesFile "config/definitions/routes")

instance Yesod App where
  makeSessionBackend App {..} =
    Just <$> defaultClientSessionBackend sessionTimeout sessionFilePath

  authRoute _ = Just LoginR

  isAuthorized PokemonR True = isAdmin
  isAuthorized _ _ = return Authorized

  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    mtrainer <- getTrainerSession
    mmsg <- getMessage
    pc <- widgetToPageContent widget

    withUrlRenderer $(hamletFile "templates/default-layout.hamlet")

getTrainerSession :: Handler (Maybe Text)
getTrainerSession = lookupSession "_ID"

getTrainerSessionId :: Handler (Maybe TrainerId)
getTrainerSessionId = do
  maybeIdText <- getTrainerSession
  let maybeId = maybeIdText >>= readMaybe . unpack
  return $ toSqlKey <$> maybeId

isLogged :: Handler AuthResult
isLogged = do
  maybeSessionId <- getTrainerSession

  return $ case maybeSessionId of
    Just _ -> Authorized
    Nothing -> AuthenticationRequired

isAdmin :: Handler AuthResult
isAdmin = do
  maybeSessionId <- getTrainerSession

  return $ case maybeSessionId of
    Nothing -> AuthenticationRequired
    Just "1" -> Authorized
    Just _ -> Unauthorized "Você não é o Admin"

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend

  runDB action = do
    master <- getYesod
    runSqlPool action $ connectionPool master

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

type Form a = Html -> MForm Handler (FormResult a, Widget)
