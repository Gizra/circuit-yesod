module Handler.RestfulBid where

import qualified Data.HashMap.Strict   as HM (insert)
import           Import
import           Model.Types           (BidType (BidTypeLive))
import           Utils.ServerSentEvent



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
  maybe Nothing (\uid ->
    maybe Nothing (\bidHash ->
      if (bidBidder bid == uid)
        then Just bidHash
        else Just $ HM.insert "bidder" Null bidHash
    ) mBidHash
  ) muid


putRestfulBidR :: BidId -> Handler Value
putRestfulBidR bidId = do
    bid <- requireJsonBody :: Handler Bid

    runDB $ replace bidId bid

    sendResponseStatus status204 ()

deleteRestfulBidR :: BidId -> Handler Value
deleteRestfulBidR bidId = do
    runDB $ delete bidId

    sendResponseStatus status204 ()

postRestfulBidsR :: Handler Value
postRestfulBidsR = do
    currentTime <- liftIO getCurrentTime
    userId <- requireAuthId

    semiBid <- requireJsonBody :: Handler SemiBid
    let bid = Bid
          { bidType = BidTypeLive
          , bidItem = semiBidItem semiBid
          , bidPrice = semiBidPrice semiBid
          , bidCreated = currentTime
          , bidChanged = Nothing
          , bidBidder = userId
          , bidUser  = Nothing
          }

    bidId   <- runDB $ insert bid
    sendMessage BidCreate (Entity bidId bid)

    returnVal <- getRestfulBidR bidId

    sendResponseStatus status201 returnVal
