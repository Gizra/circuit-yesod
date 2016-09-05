module Utils.ServerSentEvent.Data
  ( SseEventName(..)
  ) where

import Import.NoFoundation

-- @todo: Can this be in Model.Types ?
data SseEventName = BidCreate | BidEdit
    deriving (Show, Eq, Enum, Bounded, Read)

-- @todo: Try to derive generic
instance ToJSON (SseEventName) where
    toJSON BidCreate = "BidCreate"
    toJSON BidEdit = "BidEdit"
