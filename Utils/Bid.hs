module Utils.Bid
  ( isWinningBid
  ) where

import           Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import           Import

isWinningBid :: Bid -> Handler Bool
isWinningBid bidEntity = do
    -- Get the height bid of an item.
    highestBid <- runDB
               . E.select
               . E.from $ \bid -> do
                    E.where_ $ bid ^. BidItem E.==. (E.val $ bidItem bidEntity)
                            --  E.&&. bid ^. BidId E./=. (E.val $ entityKey bidEntity)
                    return
                        ( E.max_ (bid   ^. BidPrice)
                        )
    return True
