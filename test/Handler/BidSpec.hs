{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.BidSpec
  ( spec
  ) where

import Database.Persist.Sql (fromSqlKey)
import TestImport

spec :: Spec
spec =
  withApp $ do
    describe "Bid API" $ do
      it "saves a Bid" $ do
        userEntity <- createUser "bar"
        let (Entity uid _) = userEntity
        (Entity siteId _) <- createSite id
        -- @todo: Remove
        get HomeR
        statusIs 200
