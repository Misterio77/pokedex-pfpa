{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Register where

import Data.Text
import Database.Models
import Foundation
import Yesod

data Register = Register
  { registerName :: Text,
    registerPassword :: Text
  }

registerForm :: Form Register
registerForm =
  renderDivs $
    Register
      <$> areq textField "Nome" Nothing
      <*> areq passwordField "Senha" Nothing

getRegisterR :: Handler Html
getRegisterR = do
  maybeTrainerId <- lookupSession "_ID"

  case maybeTrainerId of
    Just _ ->
      redirect HomeR
    Nothing -> do
      (widget, encType) <- generateFormPost registerForm

      defaultLayout $ do
        [whamlet|
          <article .slim>
            <header>
              <hgroup>
                <h1>Registrar
                <p>Já tem uma conta? #
                  <a href=@{LoginR}>Fazer login
            <form method=post action=@{RegisterR} enctype=#{encType}>
              ^{widget}
              <button> Registrar
        |]

postRegisterR :: Handler Html
postRegisterR = do
  ((result, _), _) <- runFormPost registerForm

  case result of
    FormSuccess Register {..} -> do
      let user = Trainer registerName registerPassword
      _ <- runDB $ insert user
      setMessage "Registrado com sucesso"
      redirect LoginR
    FormMissing -> do
      setMessage "Erro no envio do formulário"
      redirect RegisterR
    FormFailure _ -> do
      setMessage "Conteúdo do formulário inválido"
      redirect RegisterR
