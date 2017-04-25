module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude         as X hiding (delete, deleteBy)
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import Model                 as X
import Model.Types           as X
import Test.Hspec            as X
import Text.Shakespeare.Text (st)
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Yesod.Auth            as X
import Yesod.Test            as X

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)


withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask

    let escapedTables = map (connEscapeName sqlBackend . DBName) tables
        query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
    rawExecute query []

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |] []

    return $ map unSingle tables

-- | Authenticate as a user. This relies on the `development: true` flag being set in test-settings.yaml,
-- which enables dummy authentication in Foundation.hs
authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    request $ do
        setMethod "POST"
        addPostParam "ident" $ userIdent u
        setUrl $ AuthR $ PluginR "dummy" []

-- | Create a user.
createUser :: Text -> YesodExample App (Entity User)
createUser ident = do
    runDB $ insertEntity User
        { userIdent = ident
        , userPassword = Nothing
        , userVerkey = Nothing
        , userVerified = True
        }

-- | Create a Sale.
createSale :: Key User -> Text -> YesodExample App (Entity Sale)
createSale uid name = do
    createSaleWithStatus uid name SaleStatusActive


-- | Create a Sale with different status.
createSaleWithStatus :: Key User -> Text -> SaleStatus -> YesodExample App (Entity Sale)
createSaleWithStatus uid name saleStatus = do
    currentTime <- liftIO getCurrentTime
    runDB $ insertEntity Sale
        { saleName = name
        , saleStatus = saleStatus
        , saleType = SaleTypeLive
        , saleCurrentItem = Nothing
        , saleCreated = currentTime
        , saleUser = uid
        }

-- | Create an Item.
createItem :: Key User -> Key Sale -> Text -> Int -> Int -> Int -> YesodExample App (Entity Item)
createItem uid saleId name minimumPrice startPrice currentPrice = do
    currentTime <- liftIO getCurrentTime
    runDB $ insertEntity Item
        { itemSale = saleId
        , itemLabel = name
        , itemMinimumPrice = minimumPrice
        , itemStartPrice = startPrice
        , itemCurrentPrice = currentPrice
        , itemCreated = currentTime
        , itemUser = uid
        }

-- | Create a Bid
createBid :: Key User -> Key Item -> Int -> YesodExample App (Entity Bid)
createBid uid itemId price = do
    currentTime <- liftIO getCurrentTime
    runDB $ insertEntity Bid
        { bidType = BidTypeLive
        , bidItem = itemId
        , bidPrice = price
        , bidCreated = currentTime
        , bidChanged = Nothing
        , bidBidder = uid
        , bidUser = Nothing
        }
