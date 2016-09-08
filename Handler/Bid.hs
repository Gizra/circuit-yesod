module Handler.Bid where

import           Import
import           Database.Persist.Sql (fromSqlKey)
import           Utils.ServerSentEvent

getBidR :: BidId -> Handler Html
getBidR bidId = do
    bid <- runDB $ get404 bidId
    defaultLayout $ do
      setTitle . toHtml $ "Bid #" ++ (show $ fromSqlKey bidId)
      $(widgetFile "bid")


getCreateBidR :: Handler Html
getCreateBidR = do
    (userId, _) <- requireAuthPair

    (widget, enctype) <- generateFormPost $ bidForm userId Nothing
    defaultLayout $(widgetFile "bid-create")

postCreateBidR :: Handler Html
postCreateBidR = do
    (userId, _) <- requireAuthPair
    ((result, widget), enctype) <- runFormPost $ bidForm userId Nothing
    case result of
        FormSuccess bid -> do
            bidId <- runDB $ insert bid
            sendMessage BidCreate (Entity bidId bid)

            setMessage "Bid saved"
            redirect $ BidR bidId
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{CreateBidR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]


getEditBidR :: BidId -> Handler Html
getEditBidR bidId = do
    bid <- runDB $ get404 bidId
    (userId, _) <- requireAuthPair

    (widget, enctype) <- generateFormPost $ bidForm userId (Just bid)
    defaultLayout $(widgetFile "bid-update")

postEditBidR :: BidId -> Handler Html
postEditBidR bidId = do
    (userId, _) <- requireAuthPair
    ((result, widget), enctype) <- runFormPost $ bidForm userId Nothing
    case result of
        FormSuccess bid -> do
            updateRes <- updateBid bidId bid
            case updateRes of
                Right _ -> do
                    sendMessage BidEdit (Entity bidId bid)
                    setMessage "Bid updated"
                    redirect $ BidR bidId
                Left errorMessages -> do
                    setMessage "Save failed"
                    defaultLayout $(widgetFile "bid-update")

        _ -> do
            setMessage "Saving failed."
            defaultLayout $(widgetFile "bid-update")

updateBid :: Key Bid -> Bid -> Handler (Either [Text] ())
updateBid bidId bid = do
    let validations =
            [ validateBidPrice bid
            , validateBidPrice' bid
            ]
    validations' <- sequenceA validations
    let lefts' = lefts validations'
    if (not $ null lefts')
        then return $ Left lefts'
        else do
            _ <- runDB $ replace bidId bid
            return $ Right ()


validateBidPrice :: Bid -> Handler (Either Text Bool)
validateBidPrice bid =
    if (bidPrice bid <= 0)
        then return $ Left "Price should be above 0"
        else return $ Right True

validateBidPrice' :: Bid -> Handler (Either Text Bool)
validateBidPrice' bid =
    if (bidPrice bid <= 10)
        then return $ Left "Price should be above 10"
        else return $ Right True

bidForm :: UserId -> Maybe Bid -> Form Bid
bidForm userId mbid = renderSematnicUiDivs $ Bid
    <$> areq (selectField optionsEnum) (selectSettings "Type") (bidType <$> mbid)
    <*> areq (selectField items) (selectSettings "Item") (bidItem <$> mbid)
    <*> areq intField "Price" (bidPrice <$> mbid)
    <*> lift (liftIO getCurrentTime)
    <*> pure Nothing
    <*> areq (selectField bidders) (selectSettings "Bidder") (bidBidder <$> mbid)
    <*> pure (Just userId)
    where
        selectSettings label =
          FieldSettings
            { fsLabel = label
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = [("class", "ui fluid dropdown")]
            }
        items = do
            entities <- runDB $ selectList [] [Asc ItemLabel]
            optionsPairs $ map (\item -> (itemLabel $ entityVal item, entityKey item)) entities

        bidders = do
            entities <- runDB $ selectList [] [Asc UserIdent]
            optionsPairs $ map (\item -> (userIdent $ entityVal item, entityKey item)) entities



-- @todo: Move to Utils.

-- Adaptation of renderDivs.
renderSematnicUiDivs :: Monad m => FormRender m a
renderSematnicUiDivs = renderSematnicUiDivsMaybeLabels True

-- Only difference here is that we add a ".field" class on the wrapper div.
renderSematnicUiDivsMaybeLabels :: Monad m => Bool -> FormRender m a
renderSematnicUiDivsMaybeLabels withLabels aform fragment = do
  (res, views') <- aFormToForm aform
  let views = views' []
  let widget = [whamlet|
$newline never
\#{fragment}
$forall view <- views
  <div.field :fvRequired view:.required :not $ fvRequired view:.optional>
      $if withLabels
              <label for=#{fvId view}>#{fvLabel view}
      $maybe tt <- fvTooltip view
          <div .tooltip>#{tt}
      ^{fvInput view}
      $maybe err <- fvErrors view
          <div .errors>#{err}
|]
  return (res, widget)
