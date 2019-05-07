{-# LANGUAGE RankNTypes, TemplateHaskell #-}
-- |Abstractions for services and instances of services that can be monitored.
module Mattrai.Service where

import Control.Lens
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Hashable (Hashable)
import Data.Monoid (First)
import Data.Text (Text)
import Network.Wreq (Response)

import Mattrai.Endpoint

instance Hashable Endpoint

-- |Each instance can live in one environment. E.g. Producion or Staging.
-- Environments are columns on the dashboard
newtype EnvironmentName = Environment {
  -- |A friendly, descriptive name for the environment
  _environmentNameAsString :: Text
} deriving Show

-- |A friendly, descriptive name for a service
newtype ServiceName = ServiceName {
  -- |The service name as text
  serviceName :: Text
} deriving Show


data DynamicProperty = DynamicProperty {
  _dynamicPropertyName     :: Text
, _dynamicPropertyEndpoint :: Endpoint
, _dynamicPropertyAccessor :: Getting (First Text) (Response ByteString) Text
}

-- |Instances of services that need to be observed. Instances have unique endpoints and live in
-- one and only one environment.
data Instance = Instance {
  -- |The environment the instance belongs to
  _instEnvironmentName      :: EnvironmentName

 -- |The endpoint that is hit to determine whether to colour the instance box green or red.
 -- A 2XX response results in a green box
, _instPingEndpoint         :: Endpoint

  -- |Other interesting endpoints relevant to the instance e.g. Jenkins links, Wiki links, metrics etc.
, _instMiscEndpoints        :: [MiscEndpoint]

  -- |Information on the instance expressed as key/value pairs of strings
, _instStaticInfo           :: [(Text, Text)]

  -- |Dynamic Properties consist of a property name, an endpoint and a lens/prism.
  -- The endpoint is called and the prism is used on the result with view.
  -- This can be used to extract the git version number from an endpoint, for example.
, _instDynamicInfo          :: [DynamicProperty]
}

-- |Create a service instance with the minimum mandatory information.
-- Config helpers can then be used to add information across instances.
serviceInstance :: EnvironmentName -> Text -> Instance
serviceInstance environment ep = Instance environment (Endpoint ep) [] [] []

-- |A service is a row in Mattrai. Each service has instances deployed across environments.
data Service = Service {
  -- |A friendly name that describes a service
  _serName      :: ServiceName
  -- |All instances of belonging to the service.
, _serInstances :: [Instance]
}

-- |Create a service with the minimum mandatory information.
-- Config helpers can then be used to add information
service :: Text -> [Instance] -> Service
service = Service . ServiceName

-- * Endpoint Constructors
-- $construct
-- Convenience functions for constructing the appropriate endpoints

-- |Constructor to create an endpoint that documents a service
docsEndpoint :: Text -> MiscEndpoint
docsEndpoint = DocsEndpoint . Endpoint

-- |Constructor to create a healthcheck endpoint for a service.
-- Healthchecks inform on what is working within a service instance
-- Java Spring healthchecks are supported.
healthCheckEndpoint :: Text -> MiscEndpoint
healthCheckEndpoint = HealthCheckEndpoint . Endpoint

-- * Lenses and Prisms
-- $lenses
-- It's normal to have lots of duplication between instances.
-- Lenses can be extremely useful for writing functions to make changes across multiple instances or endpoints

makeLenses ''EnvironmentName
makeLenses ''Instance
makeLenses ''Service
makeLenses ''DynamicProperty

-- * Miscellaneous

-- |An alias to make the Mattrai config files read a bit nicer
(-->) = (,)
