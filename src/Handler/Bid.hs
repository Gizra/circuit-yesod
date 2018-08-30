{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Bid where

import Import

-- @todo: Avoid this import
import Models.Bid

getBidR :: BidId -> Handler Html
getBidR bidId = do
  (_, user) <- requireAuthPair
  bidDb <- runDB $ get404 bidId
  case mkBid bidDb of
    Left err -> invalidArgs [err]
    Right bid ->
      defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "bid")
