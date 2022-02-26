module Handler.Home where

import Foundation
import Yesod

getHomeR :: Handler ()
getHomeR = redirect TeamsR
