{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Database.Persist.TH
import GHC.Generics
import Prelude

newtype Amount =
    Amount Int
    deriving (Show, Generic)

data SaleStatus
    = SaleStatusNotStarted
    | SaleStatusPreLive
    | SaleStatusLive
    | SalesStatusLiveEnded
    | SaleStatusClosed
    deriving (Show, Eq, Enum, Read)

derivePersistField "SaleStatus"

data MailType
    = MailTypeObscured
    | MailTypeRegular
    deriving (Show, Eq, Enum, Read)

derivePersistField "MailType"

data ItemStatus
    = ItemStatusPending
    | ItemStatusActive
    | ItemStatusGoing
    | ItemStatusGone
    | ItemStatusSold
    | ItemStatusUnsold
    deriving (Show, Eq, Enum, Read)

derivePersistField "ItemStatus"

data BidType
    = BidTypeMail
    | BidTypeAgent
    | BidTypeAutoMail
    | BidTypeAutoAgent
    | BidTypeLive
    | BidTypePostLive
    deriving (Show, Eq, Enum, Read)

derivePersistField "BidType"

data BidDelete
    = BidDeleteByStaff
    | BidDeleteChangedToFloor
  -- @todo
  -- | BidDeleteSuspendedDueToGroup
    deriving (Show, Eq, Enum, Read)

derivePersistField "BidDelete"

-- Bidder Info
data BidderInfoDbType
    = BidderInfoDbAgent
    | BidderInfoDbMail
    | BidderInfoDbWebsite
    deriving (Show, Eq, Enum, Read)

derivePersistField "BidderInfoDbType"