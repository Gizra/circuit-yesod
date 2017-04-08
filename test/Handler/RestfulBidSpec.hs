module Handler.RestfulBidSpec (spec) where

import           Data.Aeson
import           Database.Persist.Sql (fromSqlKey)
import           TestImport



spec :: Spec
spec = withApp $ do

    describe "REStful Bid page" $ do
        it "Checks a valid bid ID" $ do
            currentTime <- liftIO getCurrentTime

            userEntity <- createUser "bar"
            let (Entity uid _) = userEntity

            _ <- runDB . insert $ AccessToken currentTime uid "someRandomToken"

            (Entity saleId _)  <-  createSale uid "sale1"
            (Entity itemId _)  <- createItem uid saleId "item1" 0 10 100
            (Entity bidId _) <- createBid uid itemId 150

            request $ do
                setMethod "GET"
                setUrl $ RestfulBidR bidId
                addGetParam "access_token" "someRandomToken"

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

        it "should allow user to create a bid" $ do
            userEntity <- createUser "john"
            let (Entity uid _) = userEntity

            (Entity saleId _)  <-  createSale uid "sale1"
            (Entity itemId _)  <- createItem uid saleId "item1" 0 10 100
            (Entity bidId _) <- createBid uid itemId 150

            let body = object [ "price" .= (42 :: Int)
                              , "item"  .= fromSqlKey itemId
                              ]

            authenticateAs userEntity
            request $ do
              setMethod "POST"
              setUrl $ RestfulBidsR
              addRequestHeader ("Content-Type", "application/json")
              setRequestBody $ encode body

            printBody
            statusIs 201

            -- Assert current user is the bidder
            bodyContains $ "\"bidder\":" ++ (show $ fromSqlKey uid)
