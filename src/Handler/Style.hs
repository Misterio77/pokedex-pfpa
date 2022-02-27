{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Style where

import Data.FileEmbed
import Foundation
import Yesod

getStyleR :: Handler TypedContent
getStyleR = do
  cacheSeconds $ 60 * 60 * 24 * 7
  return $
    TypedContent "text/css" $
      toContent $(embedFile "static/style.css")
