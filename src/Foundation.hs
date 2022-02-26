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
import GHC.Int (Int64)
import Text.Hamlet (hamletFile)
import Text.Read (readMaybe)
import Yesod
import Yesod.Static

data App = App
  { staticContent :: Static,
    connectionPool :: ConnectionPool,
    sessionTimeout :: Int,
    sessionFilePath :: FilePath
  }

mkYesodData "App" $(parseRoutesFile "config/definitions/routes")

instance Yesod App where
  makeSessionBackend App {..} =
    Just <$> defaultClientSessionBackend sessionTimeout sessionFilePath

  authRoute _ = Just LoginR

  isAuthorized _ _ = return Authorized

  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    logged <- isLogged
    mmsg <- getMessage
    pc <- widgetToPageContent widget

    withUrlRenderer $(hamletFile "templates/default-layout.hamlet")

isLogged :: Handler AuthResult
isLogged = do
  maybeSessionId <- lookupSession "_ID"

  return $ case maybeSessionId of
    Just _ -> Authorized
    Nothing -> AuthenticationRequired

userIdFromSession :: Handler (Maybe UserId)
userIdFromSession = do
  maybeIdText <- lookupSession "_ID"
  let maybeId :: Maybe Int64 = maybeIdText >>= readMaybe . unpack
  return $ toSqlKey <$> maybeId

getSession :: Handler (Maybe Text)
getSession = lookupSession "_ID"

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend

  runDB action = do
    master <- getYesod
    runSqlPool action $ connectionPool master

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

type Form a = Html -> MForm Handler (FormResult a, Widget)
