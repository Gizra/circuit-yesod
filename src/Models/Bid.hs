{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Models.Bid where

import Data.Aeson.Types
import Data.Either
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
  { bidItemId :: ItemDbId
  , bidType :: BidType
  , bidAmount :: Amount
  , bidAuthor :: (UserId, UserUuid)
  , bidBidderNumber :: Maybe Int
  , bidDeleted :: BidDeleted
  , bidCreated :: UTCTime
  } deriving (Show, Generic)

data BidViewAccess
  = NonPrivileged
  | Author
  | Privileged
  deriving (Show, Generic)

data BidEntity =
  BidEntity BidId
            Bid
  deriving (Show, Generic)

data BidEntityWithViewAccess =
  BidEntityWithViewAccess BidEntity
                          BidViewAccess
  deriving (Show, Generic)

instance ToJSON BidEntityWithViewAccess where
  toJSON (BidEntityWithViewAccess (BidEntity bidId bid) bidViewAccess) =
    let jsonDefault = ["id" .= fromSqlKey bidId, "amount" .= bidAmount bid]
        jsonPerViewAccess =
          case bidViewAccess of
            NonPrivileged -> []
            Author -> []
            Privileged -> ["author_uuid" .= snd (bidAuthor bid)]
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
        { bidItemId = bidDbItemId bidDb
        , bidType = bidDbType_ bidDb
        , bidAmount = Amount $ bidDbAmount bidDb
        , bidAuthor = (bidDbAuthor bidDb, userUuid author)
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
     { bidDbItemId = bidItemId bid
     , bidDbType_ = bidType bid
     , bidDbAmount = amount
     , bidDbAuthor = fst (bidAuthor bid)
     , bidDbBidderNumber = bidBidderNumber bid
     , bidDbDeletedAuthor = deletedAuthor
     , bidDbDeletedReason = deletedReason
     , bidDbCreated = bidCreated bid
     }

-- Crud
{-| We don't use Data.Either.Validation, as we want to break on the first failure.

@todo: Break on first error. EitherT?
-}
save :: (Maybe BidId, Bid) -> Bool -> Handler (Either [Text] BidId)
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
         let validations = [positiveAmount bid]
             lefts' = Data.Either.lefts validations
         if not $ Import.null lefts'
           then return $ Left lefts'
           else saveDo
       else saveDo

-- Validations
positiveAmount :: Bid -> Either Text ()
positiveAmount bid =
  let (Amount amount) = bidAmount bid
      zeroAllowed =
        case bidType bid of
          BidTypeMail -> True
          _ -> False
  in if amount < 0
       then Left $
            pack
              ("Bid amount must be a positive value, but it is " <> show amount)
       else if amount == 0 && zeroAllowed
              then Left $
                   pack
                     ("Bid amount must be above zero, but it is " <> show amount)
              else Right ()
