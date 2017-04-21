module Handler.LoginToken where

import           Import

getLoginTokenR :: Handler Value
getLoginTokenR = do
  uid <- requireAuthId

  mAccessToken <- runDB $ selectFirst [AccessTokenUserId ==. uid] []

  returnJson mAccessToken
