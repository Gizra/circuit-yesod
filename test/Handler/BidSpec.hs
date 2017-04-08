module Handler.BidSpec (spec) where

import           Database.Persist.Sql (fromSqlKey)
import           TestImport

spec :: Spec
spec = withApp $ do

    describe "Bid page" $ do
        it "shows the bid price and ID" $ do
            userEntity <- createUser "bar"
            authenticateAs userEntity
            let (Entity uid _) = userEntity


            (Entity saleId _)  <-  createSale uid "sale1"
            (Entity itemId _)  <- createItem uid saleId "item1" 0 10 100
            (Entity bidId _) <- createBid uid itemId 150

            get $ BidR bidId

            htmlAnyContain "span.price" "150"
            htmlAnyContain "span.item-id" (show $ fromSqlKey itemId)
