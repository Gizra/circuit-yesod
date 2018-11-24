{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Profile where

import Import

import Lucid
import Text.Blaze.Html
import YesodLucid

getProfileR :: Handler Import.Html
getProfileR = do
  (_, user) <- requireAuthPair
  lucidHtml <-
    lucid $ \urlRender ->
      p_
        (do a_ [href_ (urlRender ProfileR)] (Lucid.toHtml $ userIdent user)
            div_ "Some text")
  defaultLayout $ do
    setTitle . Import.toHtml $ userIdent user <> "'s User page"
    toWidget . preEscapedToHtml . renderText $ lucidHtml
