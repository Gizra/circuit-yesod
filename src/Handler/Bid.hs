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
import Models.Bid (Bid(..), BidEntityWithPrivileges(..), BidId, BidVPrivileges(..), mkBid, bidPostForm, BidViaForm(..), BidDeleted(..), save)
import Models.Item (mkItem, Item(..))
import Types (BidType(BidTypeMail))

getBidR :: BidId -> Handler Html
getBidR bidId = do
  bidDb <- runDB $ get404 bidId
  eitherBid <- mkBid bidDb
  case eitherBid of
    Left err -> invalidArgs [err]
    Right bid -> do
        let itemId = bidItemDbId bid
        itemDb <- runDB $ get404 itemId
        item <- mkItem (itemId, itemDb)
        let bidTuple = (bidId, bid)
            encodedBidPrivileged = encodeToLazyText $ toJSON (BidEntityWithPrivileges bidTuple Models.Bid.Privileged)
            encodedBidNonPrivileged = encodeToLazyText $ toJSON (BidEntityWithPrivileges bidTuple Models.Bid.NonPrivileged)
        defaultLayout $ do
               setTitle . toHtml $ "Bid #" <> show (fromSqlKey bidId)
               $(widgetFile "bid")


postBidPostR :: ItemDbId -> Handler Html
postBidPostR itemId = do
    ((result, widget), enctype) <- runFormPost (bidPostForm itemId)
    case result of
        FormSuccess bvf -> do
            bid <- bidViaPostToBid bvf
            itemDb <- runDB $ get404 itemId
            item <- mkItem (itemId, itemDb)
            maybeBidId <- Models.Bid.save (Nothing, bid) True
            case maybeBidId of
                Left errors -> invalidArgs errors
                Right bidId ->
                    defaultLayout $ do
                        setTitle "Bid post"
                        $(widgetFile "bid-post")
        _ -> defaultLayout
              [whamlet|
                  <p>Invalid input, let's try again.
                  <form method=post action=@{BidPostR itemId} enctype=#{enctype}>
                      ^{widget}
                      <button>Submit
              |]



-- @todo: Should this be here, since it returns a Handler?
bidViaPostToBid :: BidViaForm -> Handler Bid
bidViaPostToBid bvf = do
    userId <- requireAuthId
    let itemDbId = bvfItemDbId bvf
    -- Confirm Item ID is valid, if not short-circuit it.
    -- @todo: Would it be better to have a pure BVF -> BId, which gets a validated ItemId?
    itemDb <- runDB $ get404 itemDbId
    now <- liftIO getCurrentTime
    return $
        Bid
        { bidItemDbId = itemDbId
        , bidType = BidTypeMail
        , bidAmount = bvfAmount bvf
        , bidAuthor = userId
        , bidBidderNumber = bvfBidderNumber bvf
        , bidDeleted = NotDeleted
        , bidCreated = now
        }

