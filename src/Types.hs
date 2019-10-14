{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Database.Persist.TH
import GHC.Generics
import Prelude

newtype Amount =
  Amount Int
  deriving (Show, Generic)

-- @todo: Move to FieldTypes?
data SaleStatus
  = SaleStatusNotStarted
  | SaleStatusPreLive
  | SaleStatusLive
  | SalesStatusLiveEnded
  | SaleStatusClosed
  deriving (Show, Eq, Enum, Bounded, Read, Generic)

derivePersistField "SaleStatus"

data MailType
  = MailTypeObscured
  | MailTypeRegular
  deriving (Show, Eq, Enum, Bounded, Read, Generic)

derivePersistField "MailType"

data ItemStatus
  = ItemStatusPending
  | ItemStatusActive
  | ItemStatusGoing
  | ItemStatusSold
  | ItemStatusUnsold
  deriving (Show, Eq, Enum, Bounded, Read, Generic)

derivePersistField "ItemStatus"

data BidType
  = BidTypeMail
  | BidTypeAgent
  | BidTypeAutoMail
  | BidTypeAutoAgent
  | BidTypeLive
  | BidTypePostLive
  deriving (Show, Eq, Enum, Bounded, Read, Generic)

derivePersistField "BidType"

data BidDelete
  = BidDeleteByStaff
  | BidDeleteChangedToFloor
  -- @todo
  -- | BidDeleteSuspendedDueToGroup
  deriving (Show, Eq, Enum, Bounded, Read, Generic)

derivePersistField "BidDelete"
