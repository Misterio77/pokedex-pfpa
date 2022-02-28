{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Login where

import Data.Text
import Database.Models
import Database.Persist.Sql (fromSqlKey)
import Foundation
import Yesod

data Login = Login
  { loginName :: Text,
    loginPassword :: Text
  }

loginForm :: Form Login
loginForm =
  renderDivs $
    Login
      <$> areq textField "Nome" Nothing
      <*> areq passwordField "Senha" Nothing

getLoginR :: Handler Html
getLoginR = do
  maybeTrainerId <- getTrainerSession

  case maybeTrainerId of
    Just _ ->
      redirect HomeR
    Nothing -> do
      (widget, encType) <- generateFormPost loginForm

      defaultLayout $ do
        [whamlet|
          <article .slim>
            <header>
              <hgroup>
                <h1>Fazer login
                <p>Precisa de uma conta? #
                  <a href=@{RegisterR}>Registrar
            <form method=post action=@{LoginR} enctype=#{encType}>
              ^{widget}
              <button>Login
        |]

postLoginR :: Handler Html
postLoginR = do
  ((result, _), _) <- runFormPost loginForm

  case result of
    FormSuccess Login {..} -> do
      maybeTrainer <-
        runDB $
          selectFirst
            [ TrainerName ==. loginName,
              TrainerPassword ==. loginPassword
            ]
            []

      case maybeTrainer of
        Just (Entity e Trainer {}) -> do
          setSession "_ID" $ pack . show . fromSqlKey $ e
          redirect HomeR
        Nothing -> do
          setMessage "Usuário ou senha inválidos"
          redirect LoginR
    FormMissing -> do
      setMessage "Erro no envio do formulário"
      redirect LoginR
    FormFailure _ -> do
      setMessage "Conteúdo do formulário inválido"
      redirect LoginR

getLogoutR :: Handler Html
getLogoutR = do
  maybeTrainerId <- getTrainerSession

  case maybeTrainerId of
    Nothing ->
      redirect HomeR
    Just _ -> do
      defaultLayout $ do
        [whamlet|
          <article .slim>
            <header>
              <hgroup>
                <h1>Log out
                <p>Tem certeza?
            <form method=post action=@{LogoutR}>
              <button> Log out
        |]

postLogoutR :: Handler Html
postLogoutR = do
  deleteSession "_ID"
  redirect HomeR
