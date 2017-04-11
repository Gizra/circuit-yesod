module Utils.ServerSentEvent
  ( sendMessage
  ) where

import           Data.Aeson.Encode       (encodeToBuilder)
import           Import
import           Network.Wai.EventSource


sendMessage :: ToJSON a => SseEventName -> a -> Handler ()
sendMessage eventName msg = do
    chan <- fmap appServerEvent getYesod
    liftIO $ writeChan chan
        $ ServerEvent (Just . encodeToBuilder $ toJSON eventName) Nothing
        $ return $ encodeToBuilder $ toJSON msg
