module Handler.Home where

import           Database.Persist.Sql (fromSqlKey)
import           Import

getHomeR :: Handler Html
getHomeR = do
    sales <- runDB $ selectList [] [Asc SaleId]
    bids <- runDB $ selectList [] [Desc BidId]
    defaultLayout $ do
        setTitle "Welcome To Circuit-Yesod"
        $(widgetFile "homepage")
