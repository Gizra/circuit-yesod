{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Item where

import Import
import Database.Persist.Sql (fromSqlKey)

-- @todo: Avoid this import
import Models.Bid (Bid(..), bidPostForm)
import Models.Item (mkItem, Item(..))
import qualified Data.Map.Strict as Map

getItemR :: Text -> Handler Html
getItemR itemUuid_ = do
  (userId, user) <- requireAuthPair
  itemDb <- runDB $ getBy404 $ UniqueItemUuid itemUuid_
  let (Entity itemId itemDb_) = itemDb
  eitherItem <- mkItem (itemId, itemDb_)
  case eitherItem of
    Left err -> invalidArgs [err]
    Right item -> do
      -- Generate the form to be displayed
      (widget, enctype) <- generateFormPost (bidPostForm itemId (userId, userUuid user))
      let bidsList = Map.toList (itemMailBids item)
      defaultLayout $ do
        setTitle . toHtml $ "Item #" <> itemUuid_
        $(widgetFile "item")
