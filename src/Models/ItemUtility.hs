{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.ItemUtility where

import Data.Aeson.Types
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as Map
import Database.Persist.Sql (fromSqlKey)
import GHC.Generics
import Import
import Models.Bid (Bid(..), BidDeleted(..), BidId, BidViaForm(..))
import Models.Item (Item(..), ItemId, ItemViaForm(..))
import Types (Amount(..), BidDelete(..), BidType(..), ItemStatus(..))

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
    return $
        Item
            { itemUuid = itemDbUuid itemDb
            , itemMailBids = bids
            , itemOpeningPrice = Amount $ itemDbOpeningPrice itemDb
            , itemStatus = itemDbStatus itemDb
            }

-- @todo: Where to place those?
{-| Cache Users.
-}
newtype CachedUser =
    CachedUser
        { unCachedUser :: Maybe User
        }
    deriving (Typeable)

cachedMaybeUser :: UserId -> Handler (Maybe User)
cachedMaybeUser userId = fmap unCachedUser $ cachedBy userIdAsByteString $ fmap CachedUser (runDB $ get userId)
  where
    userIdAsByteString = encodeUtf8 $ pack $ show userId

{-| To prevent import cycle error, we have mkBid here.

@todo: Is there a better way?
-}
mkBid :: BidDb -> Handler (Either Text Bid)
mkBid bidDb = do
    let eitherBidDeleted =
            case (bidDbDeletedReason bidDb, bidDbDeletedAuthor bidDb) of
                (Nothing, Nothing) -> Right NotDeleted
                (Nothing, Just _) -> Left "Bid has no deleted reason, but has a deleted author."
                (Just BidDeleteByStaff, Just userId) -> Right $ DeletedByStaff userId
                (Just BidDeleteChangedToFloor, Just userId) -> Right $ ChangedToFloor userId
                _ -> Left "Invalid Bid delete state"
    let authorId = bidDbAuthor bidDb
    maybeAuthor <- cachedMaybeUser authorId
    case (maybeAuthor, eitherBidDeleted) of
        (_, Left err) -> return $ Left err
        (Nothing, _) -> return $ Left ("Author ID #" <> (pack $ show (fromSqlKey authorId)) <> " is not known")
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
save (maybeItemId, item) validate = return $ Left "@todo: Implement"

ivfForm :: ItemDbId -> Maybe Item -> Form ItemViaForm
ivfForm itemDbId mitem =
    renderDivs $
    ItemViaForm <$> areq amountField "Amount" (itemOpeningPrice <$> mitem) <*>
    areq (selectFieldList statusList) "Status" (itemStatus <$> mitem)
  where
    statusList :: [(Text, ItemStatus)]
    statusList =
        [ ("Pending" :: Text, ItemStatusPending)
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
getAmount (Amount amount) = amount