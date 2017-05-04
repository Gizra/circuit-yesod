module Handler.RegenerateAccessTokenSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do

    describe "Regenerate access tokenR" $ do
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

            maybe (assertEq "New logged in user was not found" False True)
                  (\user  -> do
                      -- The original token, before it's regenerated.
                      mToken <- runDB $ selectFirst [AccessTokenUserId ==. entityKey user] []

                      -- GET the page so we can extract the CSRF token out of it.
                      get $ RegenerateAccessTokenR (entityKey user)
                      request $ do
                          setMethod "POST"
                          setUrl $ RegenerateAccessTokenR (entityKey user)
                          addToken
                      statusIs 303

                      -- Confirm access token has changed.
                      mTokenAfterChange <- runDB $ selectFirst [AccessTokenUserId ==. entityKey user] []

                      assertEq "Access token has changed" (mToken /= mTokenAfterChange) True
                  ) mUser

        it "should user to access own regenerate access token page" $ do
            userEntity <- createUser "alice"
            let (Entity uid _) = userEntity
            authenticateAs userEntity

            get $ RegenerateAccessTokenR uid
            statusIs 200

        it "should not allow user to access another user's regenerate access token page" $ do
            userEntity <- createUser "alice"
            let (Entity uid _) = userEntity
            authenticateAs userEntity

            anotherUserEntity <- createUser "john"
            authenticateAs anotherUserEntity

            get $ RegenerateAccessTokenR uid
            statusIs 403


        it "should allow admin to access regenerate access token page on behalf of another user" $ do
          -- Create "admin" role
          roleId <- runDB . insert $ Role "admin"

          -- Create users
          userEntity <- createUser "alice"
          let (Entity uid _) = userEntity

          adminUserEntity <- createUser "john"
          let (Entity adminUid _) = adminUserEntity

          -- Make user an admin.
          _ <- runDB . insert $ UserRole roleId adminUid
          authenticateAs adminUserEntity

          get $ RegenerateAccessTokenR uid
          statusIs 200
