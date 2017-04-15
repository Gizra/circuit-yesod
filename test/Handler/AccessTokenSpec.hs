module Handler.AccessTokenSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do

    describe "Access token" $ do
        it "should be created for a new user" $ do
            let userName = "someUser"

            -- We don't use the helper function `createUser` as that one doesn't
            -- auto-create the access token for us. Instead we will use the login
            -- directly.
            request $ do
                setMethod "POST"
                addPostParam "ident" userName
                setUrl $ AuthR $ PluginR "dummy" []


            -- Validate access token for the new user exists.
            mUser <- runDB $ selectFirst [UserIdent ==. userName] []
            tokenCreated <- maybe (return False) (\user -> do
                          mToken <- runDB $ selectFirst [AccessTokenUserId ==. entityKey user] []
                          return $ isJust mToken
                        ) mUser

            assertEq "Token created for user" tokenCreated True

        it "should show the access token on the profile page" $ do
            let userName = "foo"
            -- We don't use the helper function `createUser` as that one doesn't
            -- auto-create the access token for us. Instead we will use the login
            -- directly.
            request $ do
                setMethod "POST"
                addPostParam "ident" userName
                setUrl . AuthR $ PluginR "dummy" []

            mUser <- runDB $ selectFirst [UserIdent ==. userName] []
            case mUser of
                Nothing ->
                    assertEq "New logged in user was not found" False True
                Just _ -> do
                    get ProfileR
                    htmlAnyContain ".access-token > div" "Access token:"


        it "should show allow access to RESTful routes with valid access token" $ do
          currentTime <- liftIO getCurrentTime

          userEntity <- createUser "foo"
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

          request $ do
              setMethod "GET"
              setUrl $ RestfulBidR bidId

          statusIs 403
