{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Bid where

import Data.Aeson.Text (encodeToLazyText)
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Import

-- @todo: Avoid this import
import Models.Bid (Bid(..), BidEntityWithPrivileges(..), BidId, BidVPrivileges(..), mkBid, bidPostForm)
import Models.Item (mkItem, Item(..))

getBidR :: BidId -> Handler Html
getBidR bidId = do
  bidDb <- runDB $ get404 bidId
  eitherBid <- mkBid bidDb
  case eitherBid of
    Left err -> invalidArgs [err]
    Right bid -> do
        let itemId = bidItemDbId bid
        itemDb <- runDB $ get404 itemId
        eitherItem <- mkItem (itemId, itemDb)
        case eitherItem of
          Left err -> invalidArgs [err]
          Right item ->
            let bidTuple = (bidId, bid)
                encodedBidPrivileged = encodeToLazyText $ toJSON (BidEntityWithPrivileges bidTuple Models.Bid.Privileged)
                encodedBidNonPrivileged = encodeToLazyText $ toJSON (BidEntityWithPrivileges bidTuple Models.Bid.NonPrivileged)
            in defaultLayout $ do
                   setTitle . toHtml $ "Bid #" <> show (fromSqlKey bidId)
                   $(widgetFile "bid")


postBidPostR :: ItemDbId -> Handler Html
postBidPostR itemDbId = do
      ((result, widget), enctype) <- runFormPost (bidPostForm itemDbId)
      case result of
          FormSuccess bvf -> defaultLayout [whamlet|<p> #{show bvf}|]
          _ -> defaultLayout
              [whamlet|
                  <p>Invalid input, let's try again.
                  <form method=post action=@{BidPostR itemDbId} enctype=#{enctype}>
                      ^{widget}
                      <button>Submit
              |]