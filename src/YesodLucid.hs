{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Lucid support for Yesod.
--
-- Example Handler for a route, using Lucid to generate html,
-- including a rendered url:
--
-- > import Yesod.Lucid
-- > import Lucid
-- >
-- > getExampleR :: Handler LucidHtml
-- > getExampleR = lucid $ \url ->
-- >   p_ $ a_ [href_ (url ExampleR)] "self link"
module YesodLucid where

import Control.Monad.Identity
import Data.Text (Text)
import Lucid
import Yesod.Core
       (HandlerSite, HasContentType(..), MonadHandler, Route, ToContent,
        ToTypedContent)
import qualified Yesod.Core as Y

-- | Handler LucidHtml can be used for yesod handlers that use lucid to
-- generate html.
type LucidHtml = Html ()

-- | A lucid generator.
type LucidGen a = (Route a -> Text) -> LucidHtml

-- | Output some lucid, passes a URL renderer to the continuation.
lucid :: MonadHandler m => LucidGen (HandlerSite m) -> m LucidHtml
lucid cont = fmap cont Y.getUrlRender

instance ToTypedContent (Html ()) where
  toTypedContent m = Y.TypedContent (getContentType (Just m)) (Y.toContent m)

instance ToContent (Html ()) where
  toContent html = Y.ContentBuilder (runIdentity (execHtmlT html)) Nothing

instance HasContentType (Html ()) where
  getContentType _ = "text/html"
