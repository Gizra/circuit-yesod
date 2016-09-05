module Handler.Profile where

import Import
import           Database.Persist.Sql (fromSqlKey)

getProfileR :: Handler Html
getProfileR = do
    (userId, user) <- requireAuthPair

    bids <- runDB $ selectList [BidBidder ==. userId] [Desc BidId]

    defaultLayout $ do
        setTitle . toHtml $ userIdent user `mappend` "'s User page"
        $(widgetFile "profile")
