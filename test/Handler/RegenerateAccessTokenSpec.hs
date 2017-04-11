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

                      -- assertEq "Access token has changed" mToken mTokenAfterChange
                  ) mUser
