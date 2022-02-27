{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application where

import Foundation
import Handler.Home
import Handler.Login
import Handler.Pokemon
import Handler.Register
-- import Handler.Trainers
import Yesod

mkYesodDispatch "App" resourcesApp
