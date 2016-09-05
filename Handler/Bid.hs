module Handler.Bid where

import Import
import qualified Model.Types as Types

getBidR :: BidId -> Handler Html
getBidR bidId = do
    bid <- runDB $ get404 bidId
    defaultLayout $ do
        -- setTitle . toHtml $ "Bid #" `mappend` bidId
        setTitle "Bid #"
        $(widgetFile "bid")


getCreateBidR :: Handler Html
getCreateBidR = do
    (userId, _) <- requireAuthPair

    (widget, enctype) <- generateFormPost $ bidForm userId
    defaultLayout $(widgetFile "create-bid")

postCreateBidR :: Handler Html
postCreateBidR = do
    (userId, _) <- requireAuthPair
    ((result, widget), enctype) <- runFormPost $ bidForm userId
    case result of
        FormSuccess bid -> do
          bidId <- runDB $ insert bid
          --   sendMessage AddMembership (Entity mid membership)

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

    (widget, enctype) <- generateFormPost $ bidForm userId
    defaultLayout $(widgetFile "create-bid")

postEditBidR :: BidId -> Handler Html
postEditBidR bidId = do
    (userId, _) <- requireAuthPair
    ((result, widget), enctype) <- runFormPost $ bidForm userId
    case result of
        FormSuccess bid -> do
          _ <- runDB $ replace bidId bid
          --   sendMessage AddMembership (Entity mid membership)

          setMessage "Bid updated"
          redirect $ BidR bidId
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{EditBidR bidId} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]


bidForm :: UserId -> Form Bid
bidForm userId = renderSematnicUiDivs $ Bid
    <$> areq (selectField optionsEnum) (selectSettings "Type") (Just Types.BidTypeLive)
    <*> areq (selectField items) (selectSettings "Item") Nothing
    <*> areq intField "Price" (Just 0)
    <*> lift (liftIO getCurrentTime)
    <*> pure Nothing
    <*> areq (selectField bidders) (selectSettings "Bidder") Nothing
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
