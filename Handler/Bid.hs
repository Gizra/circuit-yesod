module Handler.Bid where

import Import

getBidR :: BidId -> Handler Html
getBidR bidId = do
    bid <- runDB $ get404 bidId
    defaultLayout $ do
        -- setTitle . toHtml $ "Bid #" `mappend` bidId
        setTitle "Bid #"
        $(widgetFile "bid")

postBidR :: Handler Html
postBidR = do
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
                <form method=post action=@{BidR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]

bidForm :: UserId -> Form GroupMembership
bidForm userId = renderSematnicUiDivs $ Bid
    <$> areq (selectField optionsEnum) (selectSettings "Type") BidTypeLive
    <*> areq (selectField items) (selectSettings "Item") Nothing
    <*> areq intField "Price" 0
    <*> lift (liftIO getCurrentTime)
    <*> pure Nothing
    <*> pure userId
    <*> pure Nothing
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
          entities <- runDB $ selectList [] [ASC ItemLabel]
          optionsPairs $ map (\item -> (itemLabel $ entityVal item, entityKey item)) entities



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
