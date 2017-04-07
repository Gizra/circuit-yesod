module Handler.RestfulBidSpec (spec) where

import           Database.Persist.Sql (fromSqlKey)
import           TestImport


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

        it "should show bidder property to bid owner" $ do
            userEntity <- createUser "john"
            let (Entity uid _) = userEntity

            (Entity saleId _)  <-  createSale uid "sale1"
            (Entity itemId _)  <- createItem uid saleId "item1" 0 10 100
            (Entity bidId _) <- createBid uid itemId 150

            authenticateAs userEntity
            get $ RestfulBidR bidId
            bodyContains $ "\"bidder\":" ++ (show $ fromSqlKey uid)

        it "should not show bidder property to another user" $ do
            userEntity <- createUser "john"
            otherUserEntity <- createUser "alice"
            let (Entity uid _) = userEntity

            (Entity saleId _)  <-  createSale uid "sale1"
            (Entity itemId _)  <- createItem uid saleId "item1" 0 10 100
            (Entity bidId _) <- createBid uid itemId 150

            authenticateAs otherUserEntity
            get $ RestfulBidR bidId
            bodyContains "\"bidder\":null"
