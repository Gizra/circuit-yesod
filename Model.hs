{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Sql (fromSqlKey)
import Database.Persist.Quasi
import Model.Types

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


instance ToJSON (Entity Bid) where
    toJSON (Entity bidId bid) = object
        [ "id"      .= (fromSqlKey bidId)
        , "created" .= bidCreated bid
        , "bidder"    .= bidBidder bid
        ]
