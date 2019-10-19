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
import Models.Bid (Bid(..), BidEntityWithPrivileges(..), BidId, BidPrivileges(..), BidViaForm(..), BidDeleted(..), BidContext(..))
import Models.BidUtility (bidPostForm, save)
import Models.Item (Item(..))
import Models.ItemUtility (mkItem, mkBid)
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

        author <- runDB $ get404 $ bidAuthor bid
        let bidctx = BidContext
                { bidctxBid = (bidId, bid)
                , bidctxAuthor = author
                , bidctxPrivileges = Models.Bid.Privileged
                }

        let bidTuple = (bidId, bid)
            encodedBidPrivileged = encodeToLazyText $ toJSON (BidEntityWithPrivileges bidctx)
            encodedBidNonPrivileged = encodeToLazyText $ toJSON (BidEntityWithPrivileges $ bidctx { bidctxPrivileges = Models.Bid.NonPrivileged })
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
            maybeBidId <- Models.BidUtility.save (Nothing, bid) True
            case maybeBidId of
                Left err -> invalidArgs [err]
                Right _ ->
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
    _ <- runDB $ get404 itemDbId
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

