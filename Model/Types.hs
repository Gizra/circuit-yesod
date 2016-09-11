{-# LANGUAGE TemplateHaskell #-}
module Model.Types where

import Database.Persist.TH
import Prelude

data SaleType = SaleTypeLive | SaleTypeMail
    deriving (Show, Eq, Enum, Bounded, Read)

derivePersistField "SaleType"

data SaleStatus = SaleStatusClosed | SaleStatusPaused | SaleStatusActive
    deriving (Show, Eq, Enum, Bounded, Read)

derivePersistField "SaleStatus"

data BidType = BidTypeBook | BidTypeLive | BidTypeMail
    deriving (Show, Eq, Enum, Bounded, Read)

derivePersistField "BidType"
