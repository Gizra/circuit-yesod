{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Models.Bid where

import Data.Aeson.Types
import Data.Either
import Data.Monoid
import Database.Persist.Sql (fromSqlKey)
import GHC.Generics
import Import
import Types (Amount(..), BidDelete(..), BidType(..))

type BidId = BidDbId

data BidDeleted
  = NotDeleted
  | DeletedByStaff UserId
  | ChangedToFloor UserId
  -- @todo:
  -- | SuspendedDueToGroup BidDbId -- We mark the Bid Id of the group, that caused this to be suspended.
  deriving (Show, Generic)

type UserUuid = Text

data Bid = Bid
  { bidItemDbId :: ItemDbId
  , bidType :: BidType
  , bidAmount :: Amount
  , bidAuthor :: UserId
  , bidBidderNumber :: Maybe Int
  , bidDeleted :: BidDeleted
  , bidCreated :: UTCTime
  } deriving (Show, Generic)

{-| Bid info submitted via form, abbreviated as `bvf` .-}
data BidViaForm = BidViaForm
    { bvfItemDbId :: ItemDbId
    , bvfAmount :: Amount
    , bvfBidderNumber :: Maybe Int
    } deriving (Show, Generic)

data BidVPrivileges
  = NonPrivileged
  | Author
  | Privileged
  deriving (Show, Generic)



data BidEntityWithPrivileges =
  BidEntityWithPrivileges (BidId, Bid) BidVPrivileges
  deriving (Show, Generic)

instance ToJSON BidEntityWithPrivileges where
  toJSON (BidEntityWithPrivileges (bidId, bid) bidVPrivileges) =
    let jsonDefault = ["id" .= fromSqlKey bidId, "amount" .= bidAmount bid]
        jsonPerViewAccess =
          case bidVPrivileges of
            NonPrivileged -> []
            Author -> []
            Privileged ->

            -- @todo: How to get the UUID now?
                []
            -- ["author_uuid" .= snd (bidAuthor bid)]
    in object $ jsonDefault <> jsonPerViewAccess

instance ToJSON Amount where
  toJSON (Amount amount) = toJSON amount

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


-- Crud
{-| We don't use Data.Either.Validation, as we want to break on the first failure.

@todo: Break on first error. EitherT?
-}
save :: (Maybe BidId, Bid) -> Bool -> Handler (Either Text BidId)
save (maybeBidId, bid) validate =
    let bidDb = getDbValues bid
        saveDo =
            case maybeBidId of
                Just bidId -> do
                    _ <- runDB $ replace bidId bidDb
                    return $ Right bidId
                Nothing -> do
                    bidId <- runDB $ insert bidDb
                    return $ Right bidId
    in if validate
        then do
        let validations =
                [ positiveAmount
                ]

            hasError = foldl
                        (\accum func ->
                            if isJust accum
                                -- We found the first error, so we can stop validating.
                                then accum
                                else func (maybeBidId, bid)
                        )
                        Nothing
                        validations

        case hasError of
            Nothing -> do
                -- Keep validating on functions that require IO.
                let validationsHandler =
                              [ higherAmount
                              ]

                hasErrorHandler <- foldM
                                      (\accum func ->
                                          if isJust accum
                                              -- We found the first error, so we can stop validating.
                                              then return accum
                                              else func (maybeBidId, bid)
                                      )
                                      Nothing
                                      validationsHandler

                case hasErrorHandler of
                    Nothing -> saveDo
                    Just error ->
                        return $ Left error
            Just error ->
                return $ Left error
        else
            -- Save without validations.
            saveDo



-- Validations
positiveAmount :: (Maybe BidId, Bid) -> Maybe Text
positiveAmount (_, bid) =
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
higherAmount :: (Maybe BidId, Bid) -> Handler (Maybe Text)
higherAmount (maybeBidId, bid) = do
    -- @todo: If Just Bid ID, ignore it.
    let (Amount amount) = bidAmount bid
    count_ <- runDB $ count [BidDbItemId ==. bidItemDbId bid, BidDbAmount >=. amount]
    return $
        if count_ == 0
        then
            Nothing
        else
            Just "Bid amount should be higher than other bids"




bidPostForm :: ItemDbId -> Form BidViaForm
bidPostForm itemDbId = renderDivs $ BidViaForm
    <$> pure itemDbId
    <*> areq amountField "Amount" (Just $ Amount 100)
    -- @todo: Add Bidder number as select list
    <*> pure Nothing


-- amountField :: (Functor m, Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m (Sum Int)
amountField = convertField Amount getAmount intField


-- @todo: Move to types
-- getAmount :: Amount => Int
getAmount (Amount amount) =
  amount



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
