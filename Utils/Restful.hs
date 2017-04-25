module Utils.Restful
  ( addEntityMetaData
  , getEntityList
  , getItemsForEntityList
  , getTotalCount
  , renderEntityList
  ) where

import qualified Data.Text           as T (pack)
import qualified Data.Text.Read      as T (decimal)
import qualified Data.HashMap.Strict as HM (insert)
import           Import
import           Yesod.Core.Types


getEntityList :: (PersistEntity t, ToJSON (Entity t), YesodPersist site,
                  PersistQuery (YesodPersistBackend site),
                  PersistQuery (PersistEntityBackend t),
                  PersistEntityBackend t ~ YesodPersistBackend site) =>
                 Route site
                 -> (Key t -> Route site)
                 -> [Filter t]
                 -> HandlerT site IO Value
getEntityList listRoute partialEntityRoute selectFilters = do
    (items, totalCount) <- getItemsForEntityList selectFilters

    renderEntityList listRoute partialEntityRoute items totalCount


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
    items <- runDB $ selectList selectFilters selectOpt

    return (items, totalCount)

renderEntityList :: (MonadHandler m, ToJSON v, ToJSON (Entity t)) =>
                    Route (HandlerSite m)
                    -> (Key t -> Route (HandlerSite m))
                    -> [Entity t]
                    -> v
                    -> m Value
renderEntityList listRoute partialEntityRoute items totalCount = do
    urlRender <- getUrlRender

    let itemsWithEntityMetaData =
          fmap
          (\(Entity entityId entity) ->
              addEntityMetaData urlRender partialEntityRoute entityId entity
          )
          items :: [Maybe (HashMap Text Value)]

    let itemsWithListMetaData = addListMetaData urlRender listRoute totalCount ["data" .= (itemsWithEntityMetaData)]
    return $ object itemsWithListMetaData



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

addListMetaData urlRender route totalCount keyValues =
    keyValues ++ metaData

    where metaData =
            [ "self" .= urlRender route
            , "count" .= totalCount
            ]

getTotalCount :: (PersistEntity val, YesodPersist site,
                  PersistQuery (YesodPersistBackend site),
                  YesodPersistBackend site ~ PersistEntityBackend val) =>
                 [Filter val] -> HandlerT site IO Int
getTotalCount filters =
  runDB $ count filters


addEntityMetaData urlRender route entityId entity =
    case toJSON (Entity entityId entity) of
        Object obj -> Just $ HM.insert "self" self obj
        _          -> Nothing

    where self = String $ urlRender (route entityId)
