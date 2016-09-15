module Handler.RestfulBidSpec (spec) where

import TestImport
import Database.Persist.Sql (fromSqlKey)

spec :: Spec
spec = withApp $ do

    describe "REStful Bid page" $ do
        it "Checks a valid bid ID" $ do
            userEntity <- createUser "bar"
            authenticateAs userEntity
            let (Entity uid _) = userEntity

            (Entity saleId _)  <-  createSale uid "sale1"
            (Entity itemId _)  <- createItem uid saleId "item1" 0 10 100
            (Entity bidId _) <- createBid uid itemId 150

            get $ RestfulBidR bidId
            statusIs 200
