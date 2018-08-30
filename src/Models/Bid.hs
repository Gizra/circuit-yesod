{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Models.Bid where

import Data.Aeson.Types
import Database.Persist.Sql (fromSqlKey)
import GHC.Generics
import Import.NoFoundation
import Types (BidType)

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

type UserUuid = Text

data Bid = Bid
  { bidItemId :: ItemId
  , bidType :: BidType
  , bidAmount :: Amount
  , bidAuthor :: (UserId, UserUuid)
  , bidDeleted :: BidDeleted
  , bidCreated :: UTCTime
  } deriving (Show, Generic)

data BidViewAccess
  = NonPrivileged
  | Author
  | Privileged
  deriving (Show, Generic)

data BidEntity =
  BidEntity BidId
            Bid
  deriving (Show, Generic)

data BidEntityWithViewAccess =
  BidEntityWithViewAccess BidEntity
                          BidViewAccess
  deriving (Show, Generic)

instance ToJSON BidEntityWithViewAccess where
  toJSON (BidEntityWithViewAccess (BidEntity bidId bid) bidViewAccess) =
    let jsonDefault = ["id" .= fromSqlKey bidId, "amount" .= bidAmount bid]
        jsonPerViewAccess =
          case bidViewAccess of
            NonPrivileged -> []
            Author -> []
            Privileged -> ["author_uuid" .= snd (bidAuthor bid)]
    in object $ jsonDefault <> jsonPerViewAccess

instance ToJSON Amount where
  toJSON (Amount amount) = toJSON amount
