module Handler.RestfulBid where

import qualified Data.HashMap.Strict as HM (insert)
import           Import



-- @todo: Use [(ParamName, ParamValue)] instead of String from Yesod.Request
addMetaData :: (Route App -> [(Text, Text)] -> Text)
            -> [(Text, Text)]
            -> BidId
            -> Bid
            -> Maybe (HashMap Text Value)
addMetaData urlRenderParams params bidId bid =
    case toJSON (Entity bidId bid) of
        Object obj -> Just $ HM.insert "self" self obj
        _          -> Nothing

    where self = String $ urlRenderParams (RestfulBidR bidId) params

getRestfulBidR :: BidId -> Handler Value
getRestfulBidR bidId = do
    bid <- runDB $ get404 bidId
    urlRenderParams <- getUrlRenderParams
    params <- reqGetParams <$> getRequest
    let bidWithMetaData = addMetaData urlRenderParams params bidId bid

    muid <- maybeAuthId

    let bidWithMetaDataAndSanitizedUser = sanitiziePrivateProperties muid bid bidWithMetaData

    return $ object ["data" .= bidWithMetaDataAndSanitizedUser]


sanitiziePrivateProperties :: Maybe (Key User) -> Bid -> Maybe (HashMap Text Value) -> Maybe (HashMap Text Value)
sanitiziePrivateProperties muid bid mBidHash =
  case muid of
      Nothing -> mBidHash
      Just uid ->
        case mBidHash of
          Nothing -> mBidHash
          Just bidHash ->
              if (bidBidder bid == uid)
                then Just bidHash
                else Just $ HM.insert "bidder" Null bidHash

putRestfulBidR :: BidId -> Handler Value
putRestfulBidR bidId = do
    bid <- requireJsonBody :: Handler Bid

    runDB $ replace bidId bid

    sendResponseStatus status204 ()

deleteRestfulBidR :: BidId -> Handler Value
deleteRestfulBidR bidId = do
    runDB $ delete bidId

    sendResponseStatus status204 ()
