module Utils.AccessToken
  ( getOrGenerateToken
  , generateToken
  ) where

import qualified Data.List           as DL (head)
import           Import.NoFoundation
import           Test.RandomStrings


getOrGenerateToken :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend)
                   => Key User -> HandlerT site IO Text
getOrGenerateToken uid = do
  mAccessToken <- runDB $ selectFirst [AccessTokenUserId ==. uid] []
  case mAccessToken of
    Nothing -> generateToken
    Just accessToken -> return $ accessTokenToken (entityVal accessToken)

generateToken :: (MonadIO m) => m Text
generateToken = do
    let isoAlpha = onlyAlphaNum randomASCII
    accessTokenStrings <- liftIO $ randomStrings (randomString isoAlpha 25) 1
    return . pack $ DL.head accessTokenStrings
