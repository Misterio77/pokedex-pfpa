{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application where

import Foundation
import Handler.Home
import Handler.Login
import Handler.Register
import Handler.Teams
import Yesod

mkYesodDispatch "App" resourcesApp
