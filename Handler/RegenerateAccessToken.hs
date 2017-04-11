module Handler.RegenerateAccessToken where

import Import
import Utils.AccessToken

-- @todo: The `uid` here is just to satisfy the types. Need to figure how to
-- create a confirmation form without any data - just the CSRF token.
data Confirmation = Confirmation
    { uid :: UserId
    }

confirmationForm :: UserId -> Form Confirmation
confirmationForm userId = renderDivs $ Confirmation
    <$> pure userId

getRegenerateAccessTokenR :: UserId -> Handler Html
getRegenerateAccessTokenR userId = do
  (widget, enctype) <- generateFormPost $ confirmationForm userId

  defaultLayout $ do
      setTitle "Regenerate Access Token"
      toWidget [whamlet|
        <form class="ui form" method="post" action="@{RegenerateAccessTokenR userId}" enctype=#{enctype}>
            ^{widget}
            <h2>
              Regenerate access token?
            <div>
              This oepration cannot be undone.

            <input type="submit" value="Submit">
      |]

postRegenerateAccessTokenR :: UserId -> Handler Html
postRegenerateAccessTokenR userId = do
  ((result, _), _) <- runFormPost $ confirmationForm userId
  case result of
      FormSuccess _ -> do
        -- Update acceess token.
        accessTokenText <- generateToken
        currentTime <- liftIO getCurrentTime
        mToken <- runDB $ selectFirst [AccessTokenUserId ==. userId] []

        case mToken of
          Nothing -> do
            -- This shouldn't normally happen.
            _ <- runDB $ insert $ AccessToken currentTime userId accessTokenText
            defaultLayout [whamlet|
                <p>Token didn't exist, but a new one was created.
            |]

          Just token -> do
            runDB $ update (entityKey token) [AccessTokenToken =. accessTokenText]
            setMessage "New access token generated."
            -- Redirect to profile page.
            -- @todo: Redirect could be also to other user's page, if admin did this.
            redirect $ ProfileR

      _ -> defaultLayout
          [whamlet|
              <p>Form is out-dated. Re-submit it.
          |]
