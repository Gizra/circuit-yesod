module Handler.SseReceive where

import Import
import Network.Wai.EventSource

getSseReceiveR :: Handler ()
getSseReceiveR = do
    chan <- fmap appServerEvent getYesod
    duppedChan <- dupChan chan
    sendWaiApplication $ eventSourceAppChan duppedChan
