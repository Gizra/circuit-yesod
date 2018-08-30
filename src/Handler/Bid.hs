{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Bid where

import Data.Aeson.Text (encodeToLazyText)
import Database.Persist.Sql (fromSqlKey)
import Import

-- @todo: Avoid this import
import Models.Bid

getBidR :: BidId -> Handler Html
getBidR bidId = do
  bidDb <- runDB $ get404 bidId
  case mkBid (bidId, bidDb) of
    Left err -> invalidArgs [err]
    Right bid ->
      let encodedBid = encodeToLazyText $ toJSON bid
      in defaultLayout $ do
           setTitle . toHtml $ "Bid #" <> show (fromSqlKey bidId)
           $(widgetFile "bid")
