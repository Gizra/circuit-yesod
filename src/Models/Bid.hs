{-# LANGUAGE OverloadedStrings #-}

module Models.Bid where

import Import
import Types (BidDelete(..), BidType)

-- Processed types.
-- itemId ItemId
-- type_ BidType
-- amount Int
-- author UserId -- We authored the Bid. The bidder itself, or a staff member.
-- bidderNumber Int Maybe
-- isDeleted Bool
-- deletedAuthor UserId Maybe -- If the Bid is deleted, we mark the deleting author, in case it was done by staff
-- deleteReason BidDelete Maybe
-- created UTCTime
-- deriving Show
data Amount =
  Amount Int

data BidDeleted
  = NotDeleted
  | DeletedByStaff UserId
  | ChangedToFloor UserId
  -- @todo:
  -- | SuspendedDueToGroup BidDbId -- We mark the Bid Id of the group, that caused this to be suspended.

data Bid = Bid
  { bidItemId :: ItemId
  , bidType :: BidType
  , bidAmount :: Amount
  , bidAuthor :: UserId
  , bidDeleted :: BidDeleted
  , bidCreated :: UTCTime
  }

mkBid :: BidDb -> Either Text Bid
mkBid bidDb =
  let eitherBidDeleted =
        case (bidDbDeletedReason bidDb, bidDbDeletedAuthor bidDb) of
          (Nothing, Nothing) -> Right NotDeleted
          (Nothing, Just _) ->
            Left "Bid has no deleted reason, but has a deleted author."
          (Just BidDeleteByStaff, Just userId) -> Right $ DeletedByStaff userId
          (Just BidDeleteChangedToFloor, Just userId) ->
            Right $ ChangedToFloor userId
          _ -> Left "Invalid Bid delete state"
  in case eitherBidDeleted of
       Left error -> Left error
       Right bidDeleted ->
         Right
           Bid
           { bidItemId = bidDbItemId bidDb
           , bidType = bidDbType_ bidDb
           , bidAmount = Amount $ bidDbAmount bidDb
           , bidAuthor = bidDbAuthor bidDb
           , bidDeleted = bidDeleted
           , bidCreated = bidDbCreated bidDb
           }
