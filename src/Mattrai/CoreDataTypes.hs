{-# LANGUAGE RankNTypes, TemplateHaskell #-}
module Mattrai.CoreDataTypes where

import Control.Lens
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics

newtype Endpoint = Endpoint {_endpointUrl :: Text} deriving (Show, Eq, Generic)
instance Hashable Endpoint
makeLenses ''Endpoint

endpointToString (Endpoint s) = s

data HealthCheckResult = HealthCheckResult {
  healthCheckResultItemName   :: HealthCheckItem
, healthCheckResultItemStatus :: HealthCheckItemStatus
} deriving (Show)

newtype HealthCheckItem = HealthCheckItem Text deriving (Show, Generic)

healthCheckItemName :: HealthCheckItem -> Text
healthCheckItemName (HealthCheckItem i) = i


data HealthCheckItemStatus = Up
                           | Down
                           deriving (Generic, Show)
makePrisms ''HealthCheckItemStatus

data PingResult = Timeout
                | CannotConnect
                | DnsFailure
                | UnknownFailure
                | UnknownFailure2
                | OtherFailure Text
                | HttpCode Int
                deriving (Show, Eq)
makeLenses ''PingResult


newtype EnvironmentName = Environment Text deriving Show
makeLenses ''EnvironmentName

environmentNameAsText :: EnvironmentName -> Text
environmentNameAsText (Environment str) = str

newtype ServiceName = ServiceName {
  serviceName :: Text
} deriving Show


data EndpointWithContext = EndpointWithContext {
  endpoint                               :: Endpoint
, endpointWithContextEndpointType        :: EndpointType
, endpointWithContextService             :: ServiceName
, endpointWithContextEnvironment         :: EnvironmentName
} deriving Show

data EndpointType = Ping | HealthCheck deriving (Show, Eq)

data MiscEndpoint = MiscEndpoint Text Endpoint
                  | LogsEndpoint Endpoint
                  | DocsEndpoint Endpoint
                  | HealthCheckEndpoint Endpoint
makePrisms ''MiscEndpoint

data Instance = Instance {
  _instEnvironmentName      :: EnvironmentName
, _instPingEndpoint         :: Endpoint
, _instMiscEndpoints        :: [MiscEndpoint]
, _instStaticInfo           :: [(Text, Text)]
}
makeLenses ''Instance

data MatTraiConfig = MatTraiConfig {
  servicesToMonitor :: [Service'']
, footer            :: Text
}

defaultConfig :: MatTraiConfig
defaultConfig = MatTraiConfig {
  servicesToMonitor = []
, footer            = ""
}

data Service'' = Service {
  _serName      :: ServiceName
, _serInstances :: [Instance]
}
makeLenses ''Service''

docsEndpoint :: Text -> MiscEndpoint
docsEndpoint = DocsEndpoint . Endpoint

healthCheckEndpoint :: Text -> MiscEndpoint
healthCheckEndpoint = HealthCheckEndpoint . Endpoint

isDoc (DocsEndpoint _) = True
isDoc _                = False

isHealthcheck (HealthCheckEndpoint _) = True
isHealthcheck _                       = False

isLog (LogsEndpoint _) = True
isLog _                = False

isMisc (MiscEndpoint _ _) = True
isMisc _                  = False

getEndpoint (DocsEndpoint ep) = ep
getEndpoint (HealthCheckEndpoint ep) = ep
getEndpoint (LogsEndpoint ep) = ep
getEndpoint (MiscEndpoint _ ep) = ep

getMiscEndpointName (MiscEndpoint name _) = name

(-->) = (,)
