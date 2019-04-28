{-# LANGUAGE TemplateHaskell #-}
-- |Abstractions for endpoints on instances of services that can be monitored.
module Mattrai.Endpoint where

import Control.Lens (makeLenses, makePrisms)
import GHC.Generics
import Data.Text (Text)


-- |A Url on an instance.
-- Endpoints are wrapped in MiscEndpoints which make clear what the URL does.
newtype Endpoint = Endpoint {
  -- |The URL represented as a string
  _endpointUrl :: Text
} deriving (Show, Eq, Generic)

-- |A URL on an instance.
-- Constructors inform us of the type of the endpoint
-- and allow us to reflect each URLs type appropriately on the dashboard
data MiscEndpoint = MiscEndpoint Text Endpoint
                  | LogsEndpoint Endpoint
                  | DocsEndpoint Endpoint
                  | HealthCheckEndpoint Endpoint

-- * Lenses and Prisms
-- $lenses
-- It's normal to have lots of duplication between instances.
-- Lenses can be extremely useful for writing functions to make changes across endpoints

makeLenses ''Endpoint
makePrisms ''MiscEndpoint
