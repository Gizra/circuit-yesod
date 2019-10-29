{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Models.BidUtility where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (retry)
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types
import Data.Either
import qualified Data.Map.Strict as Map
import Data.Monoid
import qualified Data.Set as Set
import Database.Persist.Sql (fromSqlKey)
import GHC.Generics
import Import
import Models.Bid
    ( Bid(..)
    , BidContext(..)
    , BidDeleted(..)
    , BidEntityWithPrivileges(..)
    , BidId
    , BidPrivileges(..)
    , BidViaForm(..)
    )
import Models.Item (Item(..))
import Models.ItemUtility (mkItem)
import qualified Network.Pusher as Pusher
import Types (Amount(..), BidDelete(..), BidType(..))

{-| All data needed to validate a Bid before save.

@todo: Should live here?
-}
data ContextForBidSave =
    ContextForBidSave
    -- Saved Bid already has the Item ID, so we just need the Item here.
        { cbsItem :: Item
    -- @todo
    -- , cbsSale :: (SaleId, Sale)
        }

-- Crud
{-| Save a Bid.
-}
save :: (Maybe BidId, Bid) -> Bool -> Handler (Either Text BidId)
save (maybeBidId, bid) validate =
    if validate
        then do
            yesod <- getYesod
            let itemDbId = bidItemDbId bid
            -- Lock saving.
            _ <-
                atomically
                    (do appBidPlaceSet <- readTVar $ appBidPlace yesod
                        let isNotLocked = Set.notMember itemDbId appBidPlaceSet
                        unless isNotLocked retry
                        writeTVar (appBidPlace yesod) (Set.insert itemDbId appBidPlaceSet))
            mitemDb <- runDB $ selectFirst [ItemDbId ==. itemDbId] []
            case mitemDb of
                Nothing -> return $ Left "Item of Bid not found"
                Just (Entity _ itemDb) -> do
                    item <- mkItem (itemDbId, itemDb)
                    let contextForBidSave = ContextForBidSave {cbsItem = item}
                    let hasError = validateBidBeforeSave (maybeBidId, bid) contextForBidSave
                    case hasError of
                        Nothing -> do
                            res <- saveDo (maybeBidId, bid)
                            -- Release the lock.
                            -- @todo: How to break lock after certain amount of time. e.g. if save caused an error.
                            atomically
                                (do appBidPlaceSet <- readTVar $ appBidPlace yesod
                                    writeTVar (appBidPlace yesod) (Set.delete itemDbId appBidPlaceSet))
                            return res
                        Just err -> return $ Left err
        else saveDo (maybeBidId, bid)

validateBidBeforeSave :: (Maybe BidId, Bid) -> ContextForBidSave -> Maybe Text
validateBidBeforeSave (maybeBidId, bid) contextForBidSave =
    let validations = [positiveAmount, higherAmount]
     in foldl
            (\accum func ->
                 if isJust accum
                    -- We found the first error, so we can stop validating.
                     then accum
                     else func contextForBidSave (maybeBidId, bid))
            Nothing
            validations

{-| Do the actual saving to the DB.
-}
saveDo :: (Maybe BidId, Bid) -> Handler (Either Text BidId)
saveDo (maybeBidId, bid) = do
    let bidDb = getDbValues bid
    bidId <-
        case maybeBidId of
            Just bidId -> do
                _ <- runDB $ replace bidId bidDb
                return bidId
            Nothing -> do
                bidId <- runDB $ insert bidDb
                return bidId
    -- Trigger Pusher.
    yesod <- getYesod
    let pusher = appPusher yesod
    author <- runDB $ get404 $ bidAuthor bid
    let bidctx = BidContext {bidctxBid = (bidId, bid), bidctxAuthor = author, bidctxPrivileges = Privileged}
        -- @todo: Why if I try to use `.` it says ambigous with `Perdule` vs `Import`?
        encodedBidPrivileged = Import.toStrict $ encodeToLazyText $ toJSON (BidEntityWithPrivileges bidctx)
    -- @todo: forking, means it doesn't block the request?
    liftIO $
        forkIO $ do
            res <-
                Pusher.trigger
                    pusher
                    [Pusher.Channel Pusher.Public "my-channel"]
                    "bid_create"
                    encodedBidPrivileged
                    Nothing
            return ()
    return $ Right bidId

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
    let amount = getAmount $ bidAmount bid
        zeroAllowed =
            case bidType bid of
                BidTypeMail -> True
                _ -> False
     in if amount < 0
            then Just $ pack ("Bid amount must be a positive value, but it is " <> show amount)
            else if amount == 0 && zeroAllowed
                     then Just $ pack ("Bid amount must be above zero, but it is " <> show amount)
                     else Nothing

{-| Assert no existing Bid with higher amount.

@todo: Adapt business logic.
-}
higherAmount :: ContextForBidSave -> (Maybe BidId, Bid) -> Maybe Text
higherAmount context (maybeBidId, bid) =
    let item = (cbsItem context)
        highestBidAmount =
            Map.foldl'
                (\accum bid_ ->
                     case (bidDeleted bid_) of
                         NotDeleted ->
                             let amount = getAmount $ bidAmount bid_
                              in if amount > accum
                                     then amount
                                     else accum
                         _ -> accum)
                0
                (itemMailBids item)
        currentBidAmount = getAmount $ bidAmount bid
     in if (currentBidAmount <= highestBidAmount)
            then Just "Bid amount should be higher than other bids"
            else Nothing

bidPostForm :: ItemDbId -> Form BidViaForm
bidPostForm itemDbId =
    renderDivs $ BidViaForm <$> pure itemDbId <*> areq amountField "Amount" (Just $ Amount 100) <*> pure Nothing

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
getAmount (Amount amount) = amount