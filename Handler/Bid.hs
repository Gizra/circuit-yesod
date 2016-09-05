module Handler.Bid where

import Import

getBidR :: BidId -> Handler Html
getBidR bidId = do
    bid <- runDB $ get404 bidId
    defaultLayout $ do
        -- setTitle . toHtml $ "Bid #" `mappend` bidId
        setTitle "Bid #"
        $(widgetFile "bid")
