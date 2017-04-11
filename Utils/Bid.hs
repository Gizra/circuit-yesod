module Utils.Bid
  ( isWinning
  ) where

import qualified Data.List          as DL (head)
import           Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import           Import

isWinning :: BidId -> Bid -> Handler Bool
isWinning bidEntityId bidEntity = do
    -- Get the height bid of an item.
    highestBidsResult <- runDB
               . E.select
               . E.from $ \bid -> do
                    -- Bids of item, except of current bid.
                    E.where_ $ bid ^. BidItem E.==. (E.val $ bidItem bidEntity)
                             E.&&. bid ^. BidId E.!=. (E.val bidEntityId)
                    return
                        -- SELECT MAX(price).
                        ( E.max_ (bid   ^. BidPrice)
                        )

    let (E.Value mHighestBid) = DL.head highestBidsResult
    return $ maybe True (\highestBid ->  bidPrice bidEntity > highestBid) mHighestBid
