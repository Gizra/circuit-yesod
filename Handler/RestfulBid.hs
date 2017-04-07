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
    liftIO $ print muid


    let bidWithMetaDataAndSanitizedUser =
          case muid of
              Nothing -> bidWithMetaData
              Just uid ->
                case bidWithMetaData of
                  Nothing -> bidWithMetaData
                  Just bidWithMetaData' ->
                      if (bidBidder bid == uid)
                        then Just bidWithMetaData'
                        else Just $ HM.insert "bidder" Null bidWithMetaData'



    return $ object ["data" .= bidWithMetaDataAndSanitizedUser]


putRestfulBidR :: BidId -> Handler Value
putRestfulBidR bidId = do
    bid <- requireJsonBody :: Handler Bid

    runDB $ replace bidId bid

    sendResponseStatus status204 ()

deleteRestfulBidR :: BidId -> Handler Value
deleteRestfulBidR bidId = do
    runDB $ delete bidId

    sendResponseStatus status204 ()
