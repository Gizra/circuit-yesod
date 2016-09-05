module Handler.Home where

import Import
import Database.Persist.Sql (fromSqlKey)

getHomeR :: Handler Html
getHomeR = do
    sales <- runDB $ selectList [] [Asc SaleId]
    bids <- runDB $ selectList [] [Desc BidId]
    defaultLayout $ do
        setTitle "Welcome To Circuit-Yesod"
        $(widgetFile "homepage")
