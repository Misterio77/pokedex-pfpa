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

  -- Rotas de escrita em pokémons só podem ser chamadas por admins
  isAuthorized PokemonR True = isAdmin
  isAuthorized (PokemonByIdR _) True = isAdmin
  -- Rota de adicionar ao time só pode ser chamada se estiver logado
  isAuthorized (RecruitPokemonR _) _ = isLogged
  -- Rota de remover da equipe só pode ser chamada pelo próprio usuário
  isAuthorized (TrainerPokemonByIdR trainerId _) True = isSelf trainerId
  -- Outras rotas podem ser chamadas por todos
  isAuthorized _ _ = return Authorized

  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    maybeTrainerId <- getTrainerSessionId
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

isSelf :: TrainerId -> Handler AuthResult
isSelf trainerId = do
  maybeSessionId <- getTrainerSessionId

  return $ case maybeSessionId of
    Nothing -> AuthenticationRequired
    Just t | t == trainerId -> Authorized
    Just _ -> Unauthorized "Você não é esse usuário"

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend

  runDB action = do
    master <- getYesod
    runSqlPool action $ connectionPool master

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

type Form a = Html -> MForm Handler (FormResult a, Widget)
