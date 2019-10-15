{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Models.Bid where

import Data.Aeson.Types
import Data.Either
import Data.Monoid
import Database.Persist.Sql (fromSqlKey)
import GHC.Generics
import Import
import Types (Amount(..), BidDelete(..), BidType(..))

type BidId = BidDbId

data BidDeleted
  = NotDeleted
  | DeletedByStaff UserId
  | ChangedToFloor UserId
  -- @todo:
  -- | SuspendedDueToGroup BidDbId -- We mark the Bid Id of the group, that caused this to be suspended.
  deriving (Show, Generic)

type UserUuid = Text

data Bid = Bid
  { bidItemDbId :: ItemDbId
  , bidType :: BidType
  , bidAmount :: Amount
  , bidAuthor :: UserId
  , bidBidderNumber :: Maybe Int
  , bidDeleted :: BidDeleted
  , bidCreated :: UTCTime
  } deriving (Show, Generic)

{-| Bid submitted via form, abbreviated as `bvf`.
-}
data BidViaForm = BidViaForm
    { bvfItemDbId :: ItemDbId
    , bvfAmount :: Amount
    , bvfBidderNumber :: Maybe Int
    } deriving (Show, Generic)

data BidVPrivileges
  = NonPrivileged
  | Author
  | Privileged
  deriving (Show, Generic)



data BidEntityWithPrivileges =
  BidEntityWithPrivileges (BidId, Bid) BidVPrivileges
  deriving (Show, Generic)

instance ToJSON BidEntityWithPrivileges where
  toJSON (BidEntityWithPrivileges (bidId, bid) bidVPrivileges) =
    let jsonDefault = ["id" .= fromSqlKey bidId, "amount" .= bidAmount bid]
        jsonPerViewAccess =
          case bidVPrivileges of
            NonPrivileged -> []
            Author -> []
            Privileged ->

            -- @todo: How to get the UUID now?
                []
            -- ["author_uuid" .= snd (bidAuthor bid)]
    in object $ jsonDefault <> jsonPerViewAccess

instance ToJSON Amount where
  toJSON (Amount amount) = toJSON amount

