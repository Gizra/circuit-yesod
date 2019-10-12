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
import Models.Bid (Bid, BidId, mkBid)

type ItemId = ItemDbId

data Item = Item
  { itemUuid :: Text
  , itemMailBids :: Map.Map BidId Bid
  } deriving (Show, Generic)



mkItem :: (ItemDbId, ItemDb) -> Handler (Either Text Item)
mkItem (itemDbId, itemDb) = do
  bidDbs <- runDB $ selectList [BidDbItemId ==. itemDbId] []
  bids <-
    Data.Foldable.foldlM
      (\accum (Entity bidId bidDb) -> do
         eitherBid <- mkBid bidDb
             -- @todo: We should skip the invalid Bid, and indeed let the Item
             -- come back, but we should raise some kind of exception.
         return $
           case eitherBid of
             Left err -> accum
             Right bid -> Map.insert bidId bid accum)
      Map.empty
      bidDbs
  return $ Right Item {
    itemUuid = itemDbUuid itemDb
    , itemMailBids = bids
    }



