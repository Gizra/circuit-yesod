{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Bid where

import Data.Aeson.Text (encodeToLazyText)
import Database.Persist.Sql (fromSqlKey)
import Import
import Types (BidDelete(..), BidType)

-- @todo: Avoid this import
import Models.Bid
       (Amount(..), Bid(..), BidDeleted(..), BidEntity(..),
        BidEntityWithViewAccess(..), BidId, BidViewAccess(..))

getBidR :: BidId -> Handler Html
getBidR bidId = do
  bidDb <- runDB $ get404 bidId
  eitherBid <- mkBid (bidId, bidDb)
  case eitherBid of
    Left err -> invalidArgs [err]
    Right bid ->
      let encodedBidPrivileged =
            encodeToLazyText $
            toJSON (BidEntityWithViewAccess bid Models.Bid.Privileged)
          encodedBidNonPrivileged =
            encodeToLazyText $
            toJSON (BidEntityWithViewAccess bid Models.Bid.NonPrivileged)
      in defaultLayout $ do
           setTitle . toHtml $ "Bid #" <> show (fromSqlKey bidId)
           $(widgetFile "bid")

mkBid :: (BidId, BidDb) -> Handler (Either Text BidEntity)
mkBid (bidId, bidDb) = do
  let eitherBidDeleted =
        case (bidDbDeletedReason bidDb, bidDbDeletedAuthor bidDb) of
          (Nothing, Nothing) -> Right NotDeleted
          (Nothing, Just _) ->
            Left "Bid has no deleted reason, but has a deleted author."
          (Just BidDeleteByStaff, Just userId) -> Right $ DeletedByStaff userId
          (Just BidDeleteChangedToFloor, Just userId) ->
            Right $ ChangedToFloor userId
          _ -> Left "Invalid Bid delete state"
  let authorId = bidDbAuthor bidDb
  maybeAuthor <- runDB $ get authorId
  case (maybeAuthor, eitherBidDeleted) of
    (_, Left err) -> return $ Left err
    (Nothing, _) ->
      return $
      Left
        -- ("Author ID #" <> (show (fromSqlKey authorId) :: Text) <> " is not known")
        -- @todo
        "Author ID is not known"
    (Just author, Right bidDeleted_) ->
      return $
      Right $
      BidEntity
        bidId
        Bid
        { bidItemId = bidDbItemId bidDb
        , bidType = bidDbType_ bidDb
        , bidAmount = Amount $ bidDbAmount bidDb
        , bidAuthor = (bidDbAuthor bidDb, userUuid author)
        , bidDeleted = bidDeleted_
        , bidCreated = bidDbCreated bidDb
        }
