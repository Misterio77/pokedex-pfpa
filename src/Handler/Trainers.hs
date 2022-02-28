{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Trainers where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.Esqueleto.Legacy ((^.))
import qualified Database.Esqueleto.Legacy as E
import Database.Models
import Database.Persist.Postgresql
import Foundation
import Yesod

getTrainersR :: Handler Html
getTrainersR = do
  maybeTrainerId <- getTrainerSessionId

  case maybeTrainerId of
    Just tid ->
      redirect $ TrainerByIdR tid
    Nothing ->
      redirect LoginR

getNickname :: Pokemon -> TrainerPokemon -> Text
getNickname (Pokemon name _ _ _) (TrainerPokemon _ _ nickname) =
  fromMaybe name nickname

getTrainerByIdR :: TrainerId -> Handler Html
getTrainerByIdR trainerId = do
  trainer <- runDB $ get404 trainerId
  self <- (== Authorized) <$> isSelf trainerId

  pokemonList <- runDB $
    E.select $
      E.from $ \(trainer_pokemon `E.InnerJoin` pokemon) -> do
        E.where_ (trainer_pokemon ^. TrainerPokemonTrainer E.==. E.val trainerId)
        E.on $ trainer_pokemon ^. TrainerPokemonPokemon E.==. pokemon ^. PokemonId
        return (pokemon, trainer_pokemon)

  defaultLayout
    [whamlet|
    <hgroup>
      <h1>#{trainerName trainer}
      <p>Treinador
    <hr>
    <h2>Equipe:
    <div .grid>
      $if null pokemonList
        <p>Nenhum pokémon ainda!
      $forall ((Entity pokemonId pokemon), (Entity trainerPokemonId trainerPokemon)) <- pokemonList
        <article>
          <header>
            <hgroup>
              <h2>#{getNickname pokemon trainerPokemon}
              <p>
                <a href=@{PokemonByIdR pokemonId}>#{pokemonName pokemon} ##{fromSqlKey pokemonId}
          $maybe sprite <- pokemonSpriteUrl pokemon
            <img alt=#{pokemonName pokemon} src=#{sprite}>
          <footer>
            $if self
              <form method=post action=@{TrainerPokemonByIdR trainerId trainerPokemonId}?_method=DELETE>
                <button .delete>Apagar da equipe
    |]

deleteTrainerPokemonByIdR :: TrainerId -> TrainerPokemonId -> Handler Html
deleteTrainerPokemonByIdR trainerId trainerPokemonId = do
  _ <- runDB $ delete trainerPokemonId
  setMessage "Pokémon apagado da equipe"
  redirect $ TrainerByIdR trainerId
