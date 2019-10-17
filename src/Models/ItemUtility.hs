{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Models.ItemUtility where

import Data.Aeson.Types
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as Map
import Database.Persist.Sql (fromSqlKey)
import GHC.Generics
import Import
import Models.Bid (Bid(..), BidViaForm(..), BidDeleted(..), BidId)
import Models.Item (ItemId, Item(..), ItemViaForm(..))
import Types (BidDelete(..), BidType(..), ItemStatus(..), Amount(..))

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


{-| To prevent import cycle error, we have mkBid here.

@todo: Is there a better way?
-}
mkBid :: BidDb -> Handler (Either Text Bid)
mkBid bidDb = do
  let eitherBidDeleted =
        case (bidDbDeletedReason bidDb, bidDbDeletedAuthor bidDb) of
          (Nothing, Nothing) -> Right NotDeleted
          (Nothing, Just _) ->
            Left "Bid has no deleted reason, but has a deleted author."
          (Just BidDeleteByStaff, Just userId) -> Right $ DeletedByStaff userId
          (Just BidDeleteChangedToFloor, Just userId) ->
            Right $ ChangedToFloor userId
          _ -> Left "Invalid Bid delete state"
  let authorId = bidDbAuthor bidDb
  maybeAuthor <- runDB $ get authorId
  case (maybeAuthor, eitherBidDeleted) of
    (_, Left err) -> return $ Left err
    (Nothing, _) ->
      return $
      Left
        -- ("Author ID #" <> (show (fromSqlKey authorId) :: Text) <> " is not known")
        -- @todo
        "Author ID is not known"
    (Just author, Right bidDeleted_) ->
      return $
      Right
        Bid
        { bidItemDbId = bidDbItemId bidDb
        , bidType = bidDbType_ bidDb
        , bidAmount = Amount $ bidDbAmount bidDb
        , bidAuthor = bidDbAuthor bidDb
        , bidBidderNumber = bidDbBidderNumber bidDb
        , bidDeleted = bidDeleted_
        , bidCreated = bidDbCreated bidDb
        }


-- Crud
{-| Save an Item.
-}
save :: (Maybe ItemId, Item) -> Bool -> Handler (Either Text (ItemId, Item))
save (maybeItemId, item) validate =
    return $ Left "@todo: Implement"



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