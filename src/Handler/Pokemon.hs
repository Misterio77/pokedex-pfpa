{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Pokemon where

import Data.Text (Text)
import Database.Models
import Database.Persist.Postgresql
import Foundation
import Yesod

postPokemonForm :: Form Pokemon
postPokemonForm =
  renderDivs $
    Pokemon
      <$> areq textField "Nome" Nothing
      <*> areq textField "Tipo 1" Nothing
      <*> aopt textField "Tipo 2 (opcional)" Nothing
      <*> aopt textField "Link da Sprite (opcional)" Nothing

showType2 :: Maybe Text -> Text
showType2 (Just x) = ", " <> x
showType2 Nothing = ""

getPokemonR :: Handler Html
getPokemonR = do
  pokemonList <- runDB $ selectList [] [Asc PokemonId]
  (form, encType) <- generateFormPost postPokemonForm
  admin <- isAdmin

  defaultLayout
    [whamlet|
      <h1>Pokédex
      $if admin == Authorized
        <article .slim>
          <header>
            <h2>Adicionar Pokémon
          <form method=post action=@{PokemonR} encType=#{encType}>
            ^{form}
            <button>Adicionar
        <hr>
      <div .grid>
        $forall Entity id pokemon <- pokemonList
          <article>
            <header>
              <hgroup>
                <h2>
                  <a href=@{PokemonByIdR id}>#{pokemonName pokemon}
                <p>##{fromSqlKey id}
              <p>#{pokemonType1 pokemon}#{showType2 $ pokemonType2 pokemon}
            $maybe sprite <- pokemonSpriteUrl pokemon
              <img alt=#{pokemonName pokemon} src=#{sprite}>
            <footer>
              $if admin == Authorized
                <form method=post action=@{PokemonByIdR id}?_method=DELETE>
                  <button .delete>Apagar do Pokédex
  |]

postPokemonR :: Handler Html
postPokemonR = do
  ((result, _), _) <- runFormPost postPokemonForm

  case result of
    FormSuccess pokemon -> do
      _ <- runDB $ insert pokemon
      setMessage "Pokémon adicionado"
      redirect PokemonR
    FormMissing -> do
      setMessage "Erro no envio do formulário"
      redirect PokemonR
    FormFailure _ -> do
      setMessage "Conteúdo do formulário inválido"
      redirect PokemonR

getPokemonByIdR :: PokemonId -> Handler Html
getPokemonByIdR pokemonId = do
  pokemon <- runDB $ get404 pokemonId
  admin <- isAdmin

  defaultLayout
    [whamlet|
      <article .slim>
        <header>
          <hgroup>
            <h1>#{pokemonName pokemon}
            <p>##{fromSqlKey pokemonId}
          <p>#{pokemonType1 pokemon}#{showType2 $ pokemonType2 pokemon}
        $maybe sprite <- pokemonSpriteUrl pokemon
          <img alt=#{pokemonName pokemon} src=#{sprite}>
        <footer>
          $if admin == Authorized
            <form method=post action=@{PokemonByIdR pokemonId}?_method=DELETE>
              <button .delete>Apagar do Pokédex
  |]

deletePokemonByIdR :: PokemonId -> Handler Html
deletePokemonByIdR pokemonId = do
  _ <- runDB $ delete pokemonId
  setMessage "Pokémon apagado do Pokédex"
  redirect PokemonR
