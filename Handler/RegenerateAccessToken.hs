module Handler.RegenerateAccessToken where

import Import

getRegenerateAccessTokenR :: UserId -> Handler Html
getRegenerateAccessTokenR userId = do
  request <- getRequest

  defaultLayout $ do
      setTitle "Regenerate Access Token"
      toWidget [hamlet|
      $newline never
      <form method="post" action="@{RegenerateAccessTokenR userId}">
          $maybe t <- reqToken request
              <input type=hidden name=#{defaultCsrfParamName} value=#{t}>
          <input type="submit" value="Submit">
      |]

postRegenerateAccessTokenR :: UserId -> Handler Html
postRegenerateAccessTokenR userId = error "Not yet implemented: postRegenerateAccessTokenR"
