module Utils.AccessToken
  ( generateToken
  ) where

import qualified Data.List           as DL (head)
import           Import.NoFoundation
import           Test.RandomStrings


generateToken :: (MonadIO m) => m Text
generateToken = do
    let isoAlpha = onlyAlphaNum randomASCII
    accessTokenStrings <- liftIO $ randomStrings (randomString isoAlpha 25) 1
    return . pack $ DL.head accessTokenStrings
