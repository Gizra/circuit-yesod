{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Models.Item where

import Data.Aeson.Types
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as Map
import Database.Persist.Sql (fromSqlKey)
import GHC.Generics
import Import

-- @todo: How to avoid this import?
import Models.Bid (Bid, BidId)
import Types (Amount(..), ItemStatus(..))

type ItemId = ItemDbId

data Item =
    Item
        { itemUuid :: Text
        , itemMailBids :: Map.Map BidId Bid
        , itemOpeningPrice :: Amount
        , itemStatus :: ItemStatus
        }
    deriving (Show, Generic)

{-| Item submitted via form, abbreviated as `ivf`.
-}
data ItemViaForm =
    ItemViaForm
        { ivfOpeningPrice :: Amount
        , ivfStatus :: ItemStatus
        }
    deriving (Show, Generic)