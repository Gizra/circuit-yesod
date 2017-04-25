module Handler.RestfulItemSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do

    describe "getRestfulItemR" $ do
        it "should allow user to access /api/items/[sale-id]/[item-id]" $ do
          userEntity <- createUser "john"
          let (Entity uid _) = userEntity

          (Entity saleId _)  <-  createSale uid "sale-open"
          (Entity anotherSaleId _)  <-  createSale uid "another sale-open"
          (Entity closedSaleId _)  <- createSaleWithStatus uid "sale-closed" SaleStatusClosed
          (Entity pausedSaleId _)  <- createSaleWithStatus uid "sale-closed" SaleStatusPaused

          (Entity itemId _)  <- createItem uid saleId "active sale item" 0 10 100
          (Entity closedSaleItemId _)  <- createItem uid saleId "closed sale item" 0 10 100
          (Entity anotherSaleIdItemId _)  <- createItem uid anotherSaleId "another active sale item" 0 10 100

          let userName = "alice"
          -- We don't use the helper function `createUser` as that one doesn't
          -- auto-create the access token for us. Instead we will use the login
          -- directly.
          request $ do
              setMethod "POST"
              addPostParam "ident" userName
              setUrl . AuthR $ PluginR "dummy" []

          mUser <- runDB $ selectFirst [UserIdent ==. userName] []

          maybe (assertEq "New logged in user was not found" False True)
                (\user  -> do
                    -- Get the user's access token.
                    mToken <- runDB $ selectFirst [AccessTokenUserId ==. entityKey user] []
                    let token = maybe "" (\(Entity _ t) -> accessTokenToken t) mToken

                    request $ do
                        setMethod "GET"
                        setUrl $ RestfulItemR saleId itemId
                        addGetParam "access_token" token
                    statusIs 200

                    request $ do
                        setMethod "GET"
                        setUrl $ RestfulItemR closedSaleId closedSaleItemId
                        addGetParam "access_token" token
                    statusIs 400

                    -- Item's sale that doesn't match the passed sale ID
                    request $ do
                        setMethod "GET"
                        setUrl $ RestfulItemR saleId anotherSaleIdItemId
                        addGetParam "access_token" token
                    statusIs 400

                ) mUser
