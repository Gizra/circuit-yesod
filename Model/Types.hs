{-# LANGUAGE TemplateHaskell #-}
module Model.Types where

import Database.Persist.TH
import Prelude
import Data.Aeson

data SaleType = SaleTypeLive | SaleTypeMail
    deriving (Show, Eq, Enum, Bounded, Read)

data SaleStatus = SaleStatusClosed | SaleStatusPaused | SaleStatusActive
    deriving (Show, Eq, Enum, Bounded, Read)

data BidType = BidTypeBook | BidTypeLive | BidTypeMail
    deriving (Show, Eq, Enum, Bounded, Read)

-- derivePersistField "SaleStatus"
