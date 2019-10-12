{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Item where

import Import

-- @todo: Avoid this import
import Models.Item (mkItem)

getItemR :: Text -> Handler Html
getItemR itemUuid = do
  itemDb <- runDB $ getBy404 $ UniqueItemUuid itemUuid
  let (Entity itemId itemDb_) = itemDb
  eitherItem <- mkItem (itemId, itemDb_)
  case eitherItem of
    Left err -> invalidArgs [err]
    Right item ->
      defaultLayout $ do
        setTitle . toHtml $ "Item #" <> itemUuid
        $(widgetFile "item")
