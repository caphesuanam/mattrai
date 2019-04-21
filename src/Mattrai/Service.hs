{-# LANGUAGE RankNTypes, TemplateHaskell #-}
module Mattrai.Service where

import Control.Lens
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics

newtype Endpoint = Endpoint {_endpointUrl :: Text} deriving (Show, Eq, Generic)
instance Hashable Endpoint

newtype EnvironmentName = Environment {_environmentNameAsString :: Text} deriving Show

newtype ServiceName = ServiceName {
  serviceName :: Text
} deriving Show


-- data EndpointWithContext = EndpointWithContext {
--   endpoint                               :: Endpoint
-- , endpointWithContextEndpointType        :: EndpointType
-- , endpointWithContextService             :: ServiceName
-- , endpointWithContextEnvironment         :: EnvironmentName
-- } deriving Show

-- data EndpointType = Ping | HealthCheck deriving (Show, Eq)

data MiscEndpoint = MiscEndpoint Text Endpoint
                  | LogsEndpoint Endpoint
                  | DocsEndpoint Endpoint
                  | HealthCheckEndpoint Endpoint

data Instance = Instance {
  _instEnvironmentName      :: EnvironmentName
, _instPingEndpoint         :: Endpoint -- |The endpoint that is hit to determine whether to colour the instance box green or red.
                                        -- A 2XX response results in a green box
, _instMiscEndpoints        :: [MiscEndpoint]
, _instStaticInfo           :: [(Text, Text)]
}

data Service = Service {
  _serName      :: ServiceName
, _serInstances :: [Instance]
}

-- * Endpoint Constructors
-- $construct
-- Convenience functions for constructing the appropriate endpoints

docsEndpoint :: Text -> MiscEndpoint
docsEndpoint = DocsEndpoint . Endpoint

healthCheckEndpoint :: Text -> MiscEndpoint
healthCheckEndpoint = HealthCheckEndpoint . Endpoint

-- * Lenses and Prisms
-- $lenses
-- It's normal to have lots of duplication between instances.
-- Lenses can be extremely useful for writing functions to make changes across multiple instances or endpoints

makeLenses ''Endpoint
makeLenses ''EnvironmentName
makePrisms ''MiscEndpoint
makeLenses ''Instance
makeLenses ''Service

-- * Miscellaneous

-- |An alias to make the Mattrai config files read a bit nicer
(-->) = (,)
