{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Types where

import Database.Persist.TH
import GHC.Generics
import Prelude

data SaleType = SaleTypeLive | SaleTypeMail
    deriving (Show, Eq, Enum, Bounded, Read, Generic)

derivePersistField "SaleType"

data SaleStatus = SaleStatusClosed | SaleStatusPaused | SaleStatusActive
    deriving (Show, Eq, Enum, Bounded, Read, Generic)

derivePersistField "SaleStatus"

data BidType = BidTypeBook | BidTypeLive | BidTypeMail
    deriving (Show, Eq, Enum, Bounded, Read, Generic)

derivePersistField "BidType"
