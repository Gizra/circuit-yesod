module Handler.RestfulItems where

import           Import
import           Model.Types (SaleStatus (..))

getRestfulItemsR :: SaleId -> Handler Value
getRestfulItemsR saleId= do
  sale <- runDB $ get404 saleId
  case saleStatus sale of
              SaleStatusActive -> do
                  -- Get Items related to the sale.
                  -- @todo: Add pagination.
                   items <- runDB $ selectList [ItemSale ==. saleId] [] :: Handler [Entity Item]
                   return $ object ["data" .= toJSON items]
              _ ->
                  -- Don't show items for non-active sales.
                  invalidArgs ["Cannot get items for a Sale that is not currently active."]
