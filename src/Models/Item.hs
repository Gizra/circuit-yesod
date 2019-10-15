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
import Types (Amount(..), ItemStatus(..))

type ItemId = ItemDbId

data Item = Item
  { itemUuid :: Text
  , itemMailBids :: Map.Map BidId Bid
  , itemOpeningPrice :: Amount
  , itemStatus ::  ItemStatus
  } deriving (Show, Generic)


{-| Item submitted via form, abbreviated as `ivf`.
-}
data ItemViaForm = ItemViaForm
    { ivfOpeningPrice :: Amount
    , ivfStatus ::  ItemStatus
    } deriving (Show, Generic)



mkItem :: (ItemDbId, ItemDb) -> Handler Item
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
  return $ Item {
    itemUuid = itemDbUuid itemDb
    , itemMailBids = bids
    , itemOpeningPrice = Amount $ itemDbOpeningPrice itemDb
    , itemStatus = itemDbStatus itemDb
    }


ivfForm :: ItemDbId -> Maybe Item -> Form ItemViaForm
ivfForm itemDbId mitem = renderDivs $ ItemViaForm
    <$> areq amountField "Amount" (itemOpeningPrice <$> mitem)
    <*> areq (selectFieldList statusList) "Status" (itemStatus <$> mitem)
    where
        statusList :: [(Text, ItemStatus)]
        statusList = [
              ("Pending" :: Text, ItemStatusPending)
              , ("Active" :: Text, ItemStatusActive)
              , ("Going" :: Text, ItemStatusGoing)
              , ("Gone" :: Text, ItemStatusGone)
              , ("Sold" :: Text, ItemStatusSold)
              , ("Unsold" :: Text, ItemStatusUnsold)
            ]


-- @todo: Where to move those to avoid duplication?
-- @todo: Fix type signature
-- amountField :: (Functor m, Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m (Sum Int)
amountField = convertField Amount getAmount intField


-- @todo: Move to helper to types?
-- @todo: Fix type signature
-- getAmount :: Amount => Int
getAmount (Amount amount) =
  amount