{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Item where

import Import
import Database.Persist.Sql (fromSqlKey)

-- @todo: Avoid this import
import Models.Bid (Bid(..))
import Models.BidUtility (bidPostForm, getAmount)
import Models.Item (Item(..))
import Models.ItemUtility (mkItem)
import qualified Data.Map.Strict as Map
import qualified Network.Pusher as Pusher

getItemR :: Text -> Handler Html
getItemR itemUuid_ = do
    itemDb <- runDB $ getBy404 $ UniqueItemUuid itemUuid_
    let (Entity itemDbId itemDb_) = itemDb
    item <- mkItem (itemDbId, itemDb_)
    -- Generate the form to be displayed
    (widget, enctype) <- generateFormPost (bidPostForm itemDbId)

    yesod <- getYesod
    let pusher = appPusher yesod
    triggerRes <- Pusher.trigger pusher [Pusher.Channel Pusher.Public "my-channel"] "my-event" "my-data" Nothing

    _ <- liftIO $ print $ show triggerRes

    let bidsList = reverse $ Map.toList (itemMailBids item)
    defaultLayout $ do
        setTitle . toHtml $ "Item #" <> itemUuid_
        $(widgetFile "item")