{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Bid where

import Data.Aeson.Text (encodeToLazyText)
import Database.Persist.Sql (fromSqlKey)
import Import

-- @todo: Avoid this import
import Models.Bid (BidEntityWithPrivileges(..), BidId, BidVPrivileges(..), mkBid)

getBidR :: BidId -> Handler Html
getBidR bidId = do
  bidDb <- runDB $ get404 bidId
  eitherBid <- mkBid bidDb
  case eitherBid of
    Left err -> invalidArgs [err]
    Right bid ->
      let bidTuple = (bidId, bid)
          encodedBidPrivileged =
            encodeToLazyText $
            toJSON (BidEntityWithPrivileges bidTuple Models.Bid.Privileged)
          encodedBidNonPrivileged =
            encodeToLazyText $
            toJSON (BidEntityWithPrivileges bidTuple Models.Bid.NonPrivileged)
      in defaultLayout $ do
           setTitle . toHtml $ "Bid #" <> show (fromSqlKey bidId)
           $(widgetFile "bid")
