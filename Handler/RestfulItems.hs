module Handler.RestfulItems where

import           Import
import           Model.Types   (SaleStatus (..))
import           Utils.Restful (getEntityList)

getRestfulItemsR :: SaleId -> Handler Value
getRestfulItemsR saleId= do
  sale <- runDB $ get404 saleId
  case saleStatus sale of
              SaleStatusActive ->
                   getEntityList (RestfulItemsR saleId) selectFilters
                   where selectFilters = [ItemSale ==. saleId]
              _ ->
                  -- Don't show items for non-active sales.
                  invalidArgs ["Cannot get items for a Sale that is not currently active."]
