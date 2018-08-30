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

mkBid :: BidDb -> Maybe Bid
mkBid bidDb =
  let maybeBidDeleted =
        case (bidDbDeletedReason bidDb, bidDbDeletedAuthor bidDb) of
          (Nothing, _) -> Just NotDeleted
          (Just BidDeleteByStaff, Just userId) -> Just $ DeletedByStaff userId
          (Just BidDeleteChangedToFloor, Just userId) ->
            Just $ ChangedToFloor userId
          _ -> Nothing
  in case maybeBidDeleted of
       Nothing -> Nothing
       Just bidDeleted ->
         Just
           Bid
           { bidItemId = bidDbItemId bidDb
           , bidType = bidDbType_ bidDb
           , bidAmount = Amount $ bidDbAmount bidDb
           , bidAuthor = bidDbAuthor bidDb
           , bidDeleted = bidDeleted
           , bidCreated = bidDbCreated bidDb
           }
