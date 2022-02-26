{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Teams where

import Data.Text
import Database.Models
import Foundation
import Yesod

newtype NewTeam = NewTeam {name :: Text}

newTeamForm :: Form NewTeam
newTeamForm =
  renderDivs $
    NewTeam
      <$> areq textField "Nome da equipe" Nothing

getTeamsR :: Handler Html
getTeamsR = do
  (widget, encType) <- generateFormPost newTeamForm
  logged <- isLogged

  defaultLayout $ do
    [whamlet|
      <article .slim>
        <header>
          <h1>Criar novo time
        $if logged == Authorized
          <form method=post action=@{TeamsR} enctype=#{encType}>
            ^{widget}
            <button> Criar
        $else
          Faça <a href=@{LoginR}>login</a> para criar um time
    |]

postTeamsR :: Handler Html
postTeamsR = do
  maybeUserId <- userIdFromSession

  case maybeUserId of
    Nothing -> do
      setMessage "Faça login antes"
      redirect LoginR
    Just userId -> do
      ((result, _), _) <- runFormPost newTeamForm
      case result of
        FormSuccess NewTeam {..} -> do
          let team = Team name userId
          teamId <- runDB $ insert team
          setMessage "Equipe criada com sucesso"
          -- redirect $ TeamByIdR teamId
          redirect HomeR
        FormMissing -> do
          setMessage "Erro no envio do formulário"
          redirect TeamsR
        FormFailure _ -> do
          setMessage "Conteúdo do formulário inválido"
          redirect TeamsR
