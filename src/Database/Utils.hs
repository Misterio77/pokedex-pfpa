{-# LANGUAGE OverloadedStrings #-}

module Database.Utils where

import Data.Text as T
import Yesod

(%=.) :: EntityField record Text -> Text -> Filter record
(%=.) column value =
  Filter
    column
    (FilterValue $ T.concat ["%", value, "%"])
    (BackendSpecificFilter "ILIKE")
