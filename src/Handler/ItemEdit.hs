{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.ItemEdit where

import Database.Persist.Sql (fromSqlKey)
import Import

import qualified Data.Map.Strict as Map
-- @todo: Avoid this import
import Models.Bid (Bid(..))
import Models.Item (Item(..))
import Models.ItemUtility (ivfForm, mkItem)

getItemEditR :: Text -> Handler Html
getItemEditR itemUuid_ = do
    (Entity itemDbId itemDb) <- runDB $ getBy404 $ UniqueItemUuid itemUuid_
    item <- mkItem (itemDbId, itemDb)
    -- Generate the form to be displayed
    (widget, enctype) <- generateFormPost (ivfForm itemDbId (Just item))
    defaultLayout $ do
        setTitle . toHtml $ "Edit Item #" <> itemUuid_
        $(widgetFile "item-edit")

postItemEditR :: Text -> Handler Html
postItemEditR itemUuid_ = do
    (Entity itemDbId itemDb) <- runDB $ getBy404 $ UniqueItemUuid itemUuid_
    ((result, widget), enctype) <- runFormPost (ivfForm itemDbId Nothing)
    case result of
        FormSuccess ivf ->
            defaultLayout $ do
                setTitle "Item post"
                $(widgetFile "item-edit-post")
        _ ->
            defaultLayout
                [whamlet|
                  <p>Invalid input, let's try again.
                  <form method=post action=@{ItemEditR itemUuid_} enctype=#{enctype}>
                      ^{widget}
                      <button>Submit
              |]