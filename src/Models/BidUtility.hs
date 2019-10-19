{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Models.BidUtility where

import           Control.Concurrent     (forkIO)
import Data.Aeson.Types
import Data.Aeson.Text (encodeToLazyText)
import Data.Either
import qualified Data.Map.Strict as Map
import qualified Network.Pusher as Pusher
import Data.Monoid
import Database.Persist.Sql (fromSqlKey)
import GHC.Generics
import Import
import Types (Amount(..), BidDelete(..), BidType(..))
import Models.Bid (Bid(..), BidViaForm(..), BidDeleted(..), BidId, BidPrivileges(..), BidEntityWithPrivileges(..))
import Models.Item (Item(..))
import Models.ItemUtility (mkItem)

{-| All data needed to validate a Bid before save.

@todo: Should live here?
-}
data ContextForBidSave = ContextForBidSave
    -- Saved Bid already has the Item ID, so we just need the Item here.
    { cbsItem :: Item
    -- @todo
    -- , cbsSale :: (SaleId, Sale)
    }


-- Crud
lockBid :: TMVar () -> STM ()
lockBid var = takeTMVar var

unlockBid :: TMVar () -> STM ()
unlockBid var = putTMVar var ()


{-| Save a Bid.
-}
save :: (Maybe BidId, Bid) -> Bool -> Handler (Either Text BidId)
save (maybeBidId, bid) validate =
    let bidDb = getDbValues bid
        saveDo = do
            bidId <- case maybeBidId of
                Just bidId -> do
                    _ <- runDB $ replace bidId bidDb
                    return bidId
                Nothing -> do
                    bidId <- runDB $ insert bidDb
                    return bidId

            -- Trigger Pusher.
            yesod <- getYesod
            let pusher = appPusher yesod

                bidTuple = (bidId, bid)
                -- @todo: Why if I try to use `.` it says ambigous with `Perdule` vs `Import`?
                encodedBidPrivileged = Import.toStrict $ encodeToLazyText $ toJSON (BidEntityWithPrivileges bidTuple Models.Bid.Privileged)

            -- @todo: forking, means it doesn't block the request?
            liftIO $ forkIO $ do
                res <- Pusher.trigger pusher [Pusher.Channel Pusher.Public "my-channel"] "bid_create" encodedBidPrivileged Nothing
                -- Import.print $ show res
                return ()

            return $ Right bidId

    in if validate
        then do
        let itemDbId = bidItemDbId bid
        mitemDb <- runDB $ selectFirst [ItemDbId ==. itemDbId] []
        case mitemDb of
            Nothing -> return $ Left "Item of Bid not found"
            Just (Entity _ itemDb) -> do
                item <- mkItem (itemDbId, itemDb)
                let contextForBidSave =
                        ContextForBidSave
                            { cbsItem = item
                            }

--                yesod <- getYesod
--                action <- liftIO $ atomically $ do
--                    let appBidPlace_ = appBidPlace yesod
--                    lockBid appBidPlace_
--                    unlockBid appBidPlace_


                -- @todo: Run this inside STM.
                let validations =
                        [ positiveAmount
                        , higherAmount
                        ]

                    hasError = foldl
                                (\accum func ->
                                    if isJust accum
                                        -- We found the first error, so we can stop validating.
                                        then accum
                                        else func contextForBidSave (maybeBidId, bid)
                                )
                                Nothing
                                validations

                case hasError of
                    Nothing -> saveDo
                    Just err ->
                        return $ Left err
        else
            -- Save without validations.
            saveDo


getDbValues :: Bid -> BidDb
getDbValues bid =
  let (Amount amount) = bidAmount bid
      (deletedReason, deletedAuthor) =
        case bidDeleted bid of
          NotDeleted -> (Nothing, Nothing)
          DeletedByStaff userId -> (Just BidDeleteByStaff, Just userId)
          ChangedToFloor userId -> (Just BidDeleteChangedToFloor, Just userId)
  in BidDb
     { bidDbItemId = bidItemDbId bid
     , bidDbType_ = bidType bid
     , bidDbAmount = amount
     , bidDbAuthor = bidAuthor bid
     , bidDbBidderNumber = bidBidderNumber bid
     , bidDbDeletedAuthor = deletedAuthor
     , bidDbDeletedReason = deletedReason
     , bidDbCreated = bidCreated bid
     }


-- Validations
positiveAmount :: ContextForBidSave -> (Maybe BidId, Bid) -> Maybe Text
positiveAmount context (_, bid) =
  let (Amount amount) = bidAmount bid
      zeroAllowed =
        case bidType bid of
          BidTypeMail -> True
          _ -> False
  in if amount < 0
       then Just $
            pack
              ("Bid amount must be a positive value, but it is " <> show amount)
       else if amount == 0 && zeroAllowed
              then Just $
                   pack
                     ("Bid amount must be above zero, but it is " <> show amount)
              else Nothing


{-| Assert no existing Bid with higher amount.

@todo: Adapt business logic.
-}
higherAmount :: ContextForBidSave -> (Maybe BidId, Bid) -> Maybe Text
higherAmount context (maybeBidId, bid) =
    let item = (cbsItem context)
        highestBidAmount =
            Map.foldl' (\accum bid_ ->
                            case (bidDeleted bid_) of
                                NotDeleted ->
                                    let amount = getAmount $ bidAmount bid_
                                    in if amount  > accum
                                        then amount
                                        else accum
                                _ ->
                                    accum
                       )
                       0
                       (itemMailBids item)

        currentBidAmount = getAmount $ bidAmount bid

    in if (currentBidAmount <= highestBidAmount)
        then
            Just "Bid amount should be higher than other bids"
        else
            Nothing





bidPostForm :: ItemDbId -> Form BidViaForm
bidPostForm itemDbId = renderDivs $ BidViaForm
    <$> pure itemDbId
    <*> areq amountField "Amount" (Just $ Amount 100)
    -- @todo: Add Bidder number as select list
    <*> pure Nothing




bidViaPostToBid :: BidViaForm -> Handler Bid
bidViaPostToBid bvf = do
    userId <- requireAuthId
    let itemDbId = bvfItemDbId bvf
    -- Confirm Item ID is valid, if not short-circuit it.
    itemDb <- runDB $ get404 itemDbId
    now <- liftIO getCurrentTime
    return $
        Bid
        { bidItemDbId = itemDbId
        , bidType = BidTypeMail
        , bidAmount = bvfAmount bvf
        , bidAuthor = userId
        , bidBidderNumber = bvfBidderNumber bvf
        , bidDeleted = NotDeleted
        , bidCreated = now
        }


-- @todo: Where to move those to avoid duplication?
-- @todo: Fix type signature
-- amountField :: (Functor m, Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m (Sum Int)
amountField = convertField Amount getAmount intField


-- @todo: Move to helper to types?
-- @todo: Fix type signature
-- getAmount :: Amount => Int
getAmount (Amount amount) =
  amount

