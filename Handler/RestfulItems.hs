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
                   let selectFilters = [ItemSale ==. saleId]
                   items <- runDB $ selectList selectFilters [] :: Handler [Entity Item]
                   totalCount <- runDB $ count (selectFilters :: [Filter Item])
                   return $ object
                              [ "data" .= toJSON items
                              , "count" .= totalCount
                              ]
              _ ->
                  -- Don't show items for non-active sales.
                  invalidArgs ["Cannot get items for a Sale that is not currently active."]
