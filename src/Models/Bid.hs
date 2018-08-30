{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Models.Bid where

import Data.Aeson.Types
import Database.Persist.Sql (fromSqlKey)
import GHC.Generics
import Import.NoFoundation
import Types (BidDelete(..), BidType)

type BidId = BidDbId

data Amount =
  Amount Int
  deriving (Show, Generic)

data BidDeleted
  = NotDeleted
  | DeletedByStaff UserId
  | ChangedToFloor UserId
  -- @todo:
  -- | SuspendedDueToGroup BidDbId -- We mark the Bid Id of the group, that caused this to be suspended.
  deriving (Show, Generic)

data Bid = Bid
  { bidItemId :: ItemId
  , bidType :: BidType
  , bidAmount :: Amount
  , bidAuthor :: UserId
  , bidDeleted :: BidDeleted
  , bidCreated :: UTCTime
  } deriving (Show, Generic)

data BidEntity =
  BidEntity BidId
            Bid
  deriving (Show, Generic)

instance ToJSON BidEntity where
  toJSON (BidEntity bidId bid) =
    object ["id" .= fromSqlKey bidId, "amount" .= bidAmount bid]

instance ToJSON Amount where
  toJSON (Amount amount) = toJSON amount

mkBid :: (BidId, BidDb) -> Either Text BidEntity
mkBid (bidId, bidDb) =
  let eitherBidDeleted =
        case (bidDbDeletedReason bidDb, bidDbDeletedAuthor bidDb) of
          (Nothing, Nothing) -> Right NotDeleted
          (Nothing, Just _) ->
            Left "Bid has no deleted reason, but has a deleted author."
          (Just BidDeleteByStaff, Just userId) -> Right $ DeletedByStaff userId
          (Just BidDeleteChangedToFloor, Just userId) ->
            Right $ ChangedToFloor userId
          _ -> Left "Invalid Bid delete state"
  in case eitherBidDeleted of
       Left err -> Left err
       Right bidDeleted_ ->
         Right $
         BidEntity
           bidId
           Bid
           { bidItemId = bidDbItemId bidDb
           , bidType = bidDbType_ bidDb
           , bidAmount = Amount $ bidDbAmount bidDb
           , bidAuthor = bidDbAuthor bidDb
           , bidDeleted = bidDeleted_
           , bidCreated = bidDbCreated bidDb
           }
