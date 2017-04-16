module Utils.Restful
  ( getEntityList
  , getItemsForEntityList
  , getTotalCount
  , renderEntityList
  ) where

import qualified Data.Text        as T (pack)
import qualified Data.Text.Read   as T (decimal)
import           Import
import           Yesod.Core.Types


getEntityList :: (PersistEntity val, ToJSON (Entity val), YesodPersist site,
                  PersistQuery (YesodPersistBackend site),
                  PersistQuery (PersistEntityBackend val),
                  PersistEntityBackend val ~ YesodPersistBackend site) =>
                 Route site -> [Filter val] -> HandlerT site IO Value
getEntityList route selectFilters = do
    (mItems, totalCount) <- getItemsForEntityList selectFilters

    renderEntityList route mItems totalCount


getItemsForEntityList :: (PersistEntity val, YesodPersist site,
                          PersistQuery (YesodPersistBackend site),
                          PersistQuery (PersistEntityBackend val),
                          PersistEntityBackend val ~ YesodPersistBackend site) =>
                         [Filter val] -> HandlerT site IO ([Entity val], Int)
getItemsForEntityList selectFilters = do
    mpage <- lookupGetParam "page"
    selectOpt <- returnValueOrThrowException . (addPager mpage 3) $ []

    totalCount <- getTotalCount selectFilters

    -- @todo: Re-query only if count has values.
    mItems <- runDB $ selectList selectFilters selectOpt

    return (mItems, totalCount)

renderEntityList :: (MonadHandler m, ToJSON v, ToJSON a) =>
                    Route (HandlerSite m) -> a -> v -> m Value
renderEntityList route mItems totalCount = do
    params <- reqGetParams <$> getRequest
    urlRenderParams <- getUrlRenderParams

    let eventsWithMetaData = addListMetaData urlRenderParams route params totalCount ["data" .= toJSON mItems]
    return $ object eventsWithMetaData



returnValueOrThrowException :: MonadIO m
                            => Either Text a
                            -> m a
returnValueOrThrowException eitherResult =
    case eitherResult of
        Right val -> return val
        Left val  -> liftIO . throwIO . HCError $ InvalidArgs [val]

getCurrentPage :: Maybe Text -> Either Text Int
getCurrentPage mpage =
    case (T.decimal $ fromMaybe "0" mpage) of
        Left _ -> Left $ T.pack "Invalid page ID"
        Right (val, _) -> Right val

addPager :: Maybe Text
            -> Int
            -> [SelectOpt record]
            -> Either Text [SelectOpt record]
addPager mpage resultsPerPage selectOpt =
    case getCurrentPage mpage of
        Left val -> Left val
        Right pageNumber ->
            Right $ selectOpt ++ limitTo ++ offsetBy
            where limitTo = [ LimitTo resultsPerPage ]
                  offsetBy = [ OffsetBy $ (pageNumber - 1) * resultsPerPage | pageNumber > 0]

addListMetaData urlRenderParams route params totalCount keyValues =
    keyValues ++ metaData

    where metaData =
            [ "self" .= urlRenderParams route params
            , "count" .= totalCount
            ]


getTotalCount :: (PersistEntity val, YesodPersist site,
                  PersistQuery (YesodPersistBackend site),
                  YesodPersistBackend site ~ PersistEntityBackend val) =>
                 [Filter val] -> HandlerT site IO Int
getTotalCount filters =
  runDB $ count filters
