{-# LANGUAGE RankNTypes, TemplateHaskell #-}
module CoreDataTypes where

import Control.Lens
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics

newtype Endpoint = Endpoint Text deriving (Show, Eq, Generic)
instance Hashable Endpoint

endpointToString (Endpoint s) = s

data HealthCheckResult = HealthCheckResult {
  healthCheckResultItemName   :: HealthCheckItem
, healthCheckResultItemStatus :: HealthCheckItemStatus
} deriving (Show)
--instance ToJSON HealthCheckResult where
--  toJSON (HealthCheckResult name status) =
--    object [
--             "name"   .= name
--           , "status" .= status
--           ]

newtype HealthCheckItem = HealthCheckItem Text deriving (Show, Generic)
--instance ToJSON HealthCheckItem

healthCheckItemName :: HealthCheckItem -> Text
healthCheckItemName (HealthCheckItem i) = i


data HealthCheckItemStatus = Up
                           | Down
                           deriving (Generic, Show)
--instance ToJSON HealthCheckItemStatus

data PingResult = Timeout
                | CannotConnect
                | DnsFailure
                | UnknownFailure
                | UnknownFailure2
                | OtherFailure Text
                | HttpCode Int
                deriving (Show, Eq)
makeLenses ''PingResult
-- instance ToJSON PingResult where
--   toJSON stat =
--     let base = [ "status" .= if success then "Up" :: Text else "Down" ]
--         success = case stat of
--                     HttpCode n | n >= 200 && n < 300 -> True
--                     _                                -> False
--     in object $ if success then base else "reason" .= show stat : base


--
newtype EnvironmentName = Environment Text deriving Show

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
makeLenses ''MiscEndpoint

data Instance = Instance {
  _instEnvironmentName      :: EnvironmentName
, _instPingEndpoint         :: Endpoint
, _instMiscEndpoints        :: [MiscEndpoint]
, _instStaticInfo           :: [(Text, Text)]
}
makeLenses ''Instance

data GlobalLink = GlobalLink Text Endpoint
data GlobalLinks = GlobalLinks {
  globalLink :: GlobalLink
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
