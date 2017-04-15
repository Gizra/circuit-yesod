module Handler.RestfulItemsSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do

    describe "getRestfulItemsR" $ do
      it "should allow user to access /api/items/[sale-id]" $ do
        userEntity <- createUser "john"
        let (Entity uid _) = userEntity

        (Entity saleId _)  <-  createSale uid "sale-open"
        (Entity closedSaleId _)  <- createSaleWithStatus uid "sale-closed" SaleStatusClosed
        (Entity pausedSaleId _)  <- createSaleWithStatus uid "sale-closed" SaleStatusPaused
        (Entity itemId _)  <- createItem uid saleId "item1" 0 10 100

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
                      setUrl $ RestfulItemsR saleId
                      addGetParam "access_token" token
                  statusIs 200

                  request $ do
                      setMethod "GET"
                      setUrl $ RestfulItemsR closedSaleId
                      addGetParam "access_token" token
                  statusIs 400

                  request $ do
                      setMethod "GET"
                      setUrl $ RestfulItemsR pausedSaleId
                      addGetParam "access_token" token
                  statusIs 400

              ) mUser
