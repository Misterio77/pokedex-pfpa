{-# LANGUAGE TemplateHaskell #-}

module Content.Static where

import Data.Text
import Yesod.Static

staticFiles "static"

staticPieces :: StaticRoute -> [Text]
staticPieces (StaticRoute pieces _) = pieces
