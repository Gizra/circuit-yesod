module Utils.Restful
  ( getEntityList
  , getTotalCount
  ) where

import qualified Data.Text        as T (pack)
import qualified Data.Text.Read   as T (decimal)
import           Import
import           Yesod.Core.Types



getEntityList route selectFilters = do
    mpage <- lookupGetParam "page"
    params <- reqGetParams <$> getRequest

    selectOpt <- returnValueOrThrowException . (addPager mpage 3) $ []

    totalCount <- getTotalCount selectFilters

    -- @todo: Re-query only if count has values.
    mItems <- runDB $ selectList selectFilters selectOpt

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

-- addPager :: Maybe Text
--          -> Int
--          -> [ SelectOpt val ]
--          -> Either Text [ SelectOpt val ]
addPager mpage resultsPerPage selectOpt =
    case getCurrentPage mpage of
        Left val -> Left val
        Right pageNumber ->
            Right $ selectOpt ++ limitTo ++ offsetBy
            where limitTo = [ LimitTo resultsPerPage ]
                  offsetBy = [ OffsetBy $ (pageNumber - 1) * resultsPerPage | pageNumber > 0]


-- addListMetaData :: KeyValue t
--                 => (Route App -> [(Text, Text)] -> Text)
--                 -> Route
--                 -> [(Text, Text)]
--                 -> Int
--                 -> [t]
--                 -> [t]
addListMetaData urlRenderParams route params totalCount keyValues =
    keyValues ++ metaData

    where metaData =
            [ "self" .= urlRenderParams route params
            , "count" .= totalCount
            ]


-- getTotalCount :: (PersistEntity val) => [Filter val] -> Handler Int
getTotalCount filters =
  runDB $ count filters
