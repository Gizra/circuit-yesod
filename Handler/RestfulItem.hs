module Handler.RestfulItem where

import           Import
import           Model.Types   (SaleStatus (..))
import           Utils.Restful

getRestfulItemR :: SaleId -> ItemId -> Handler Value
getRestfulItemR saleId itemId = do
    sale <- runDB $ get404 saleId

    if (saleStatus sale == SaleStatusActive)
        then do
            -- Active sale.
            item <- runDB $ get404 itemId

            if (itemSale item == saleId)
              then do
                  urlRender <- getUrlRender
                  let itemWithMetaData = addEntityMetaData urlRender (RestfulItemR saleId) itemId item

                  return $ object ["data" .= toJSON itemWithMetaData]
              else
                  -- Don't allow seeing an item if the passed sale ID isn't the right
                  -- one. That is if for example we have /api/items/10/1 and
                  -- Item ID (1) doesn't belong to sale ID (10).
                  invalidArgs ["Item's sale doesn't match the Sale ID you have passed."]
      else
        invalidArgs ["Cannot get items for a Sale that is not currently active."]
