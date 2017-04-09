module Handler.RegenerateAccessToken where

import qualified Data.List as DL (head)
import Import
import Test.RandomStrings

data Confirmation = Confirmation
    { uid :: UserId
    }

confirmationForm :: UserId -> Form Confirmation
confirmationForm userId = renderDivs $ Confirmation
    <$> pure userId

getRegenerateAccessTokenR :: UserId -> Handler Html
getRegenerateAccessTokenR userId = do
  (widget, enctype) <- generateFormPost $ confirmationForm userId
  request <- getRequest

  defaultLayout $ do
      setTitle "Regenerate Access Token"
      toWidget [whamlet|
        <form method="post" action="@{RegenerateAccessTokenR userId}" enctype=#{enctype}>
            ^{widget}
            <input type="submit" value="Submit">
      |]

postRegenerateAccessTokenR :: UserId -> Handler Html
postRegenerateAccessTokenR userId = do
  ((result, widget), enctype) <- runFormPost $ confirmationForm userId
  case result of
      FormSuccess confirmation -> do
        -- Update acceess token.
        let isoAlpha = onlyAlphaNum randomASCII
        accessTokenStrings <- liftIO $ randomStrings (randomString isoAlpha 25) 1
        let accessTokenText = pack $ DL.head accessTokenStrings :: Text

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
