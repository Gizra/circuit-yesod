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


getItemR :: Text -> Handler Html
getItemR itemUuid_ = do
    itemDb <- runDB $ getBy404 $ UniqueItemUuid itemUuid_
    let (Entity itemDbId itemDb_) = itemDb
    item <- mkItem (itemDbId, itemDb_)
    -- Generate the form to be displayed
    (widget, enctype) <- generateFormPost (bidPostForm itemDbId)

    -- Show last 10 Bids.
    let bidsList = take 10 $ reverse $ Map.toList (itemMailBids item)
    defaultLayout $ do
        setTitle . toHtml $ "Item #" <> itemUuid_
        $(widgetFile "item")