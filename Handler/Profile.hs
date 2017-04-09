module Handler.Profile where

import Import

getProfileR :: Handler Html
getProfileR = do
    (uid, user) <- requireAuthPair

    -- Get the access token.
    mAccessToken <- runDB $ selectFirst [AccessTokenUserId ==. uid] []
    let accessTokenText = maybe "" (\accessToken -> accessTokenToken $ entityVal accessToken) mAccessToken :: Text

    defaultLayout $ do
        setTitle . toHtml $ userIdent user `mappend` "'s User page"
        $(widgetFile "profile")
