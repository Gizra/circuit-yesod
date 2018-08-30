{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Database.Persist.TH
import GHC.Generics
import Prelude

data MailType
  = MailTypeObscured
  | MailTypeRegular
  deriving (Show, Eq, Enum, Bounded, Read, Generic)

derivePersistField "MailType"

data SaleStatus
  = SaleStatusClosed
  | SaleStatusPaused
  | SaleStatusActive
  deriving (Show, Eq, Enum, Bounded, Read, Generic)

derivePersistField "SaleStatus"
