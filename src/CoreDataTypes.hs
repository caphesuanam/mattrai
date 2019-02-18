module CoreDataTypes where

import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Text (Text)
import GHC.Generics

-- Endpoint
newtype Endpoint = Endpoint String deriving (Show, Eq, Generic)
instance ToJSON Endpoint

endpointToString (Endpoint s) = s



-- Results

data HealthCheckResult = HealthCheckResult {
  healthCheckResultItemName   :: HealthCheckItem
, healthCheckResultItemStatus :: HealthCheckItemStatus
} deriving (Show)
instance ToJSON HealthCheckResult where
  toJSON (HealthCheckResult name status) =
    object [
             "name"   .= name
           , "status" .= status
           ]

newtype HealthCheckItem = HealthCheckItem Text deriving (Show, Generic)
instance ToJSON HealthCheckItem

healthCheckItemName :: HealthCheckItem -> Text
healthCheckItemName (HealthCheckItem i) = i


data HealthCheckItemStatus = Up
                           | Down
                           deriving (Generic, Show)
instance ToJSON HealthCheckItemStatus

data PingResult = Timeout
                | CannotConnect
                | DnsFailure
                | UnknownFailure
                | UnknownFailure2
                | HttpCode Int
                deriving (Show, Eq)
instance ToJSON PingResult where
  toJSON stat =
    let base = [ "status" .= if success then ("Up" :: Text) else "Down" ]
        success = case stat of
                    HttpCode n | n >= 200 && n < 300 -> True
                    _                                -> False
    in object $ if success then base else "reason" .= show stat : base


--
newtype EnvironmentName = Environment Text deriving Show

environmentNameAsText :: EnvironmentName -> Text
environmentNameAsText (Environment str) = str

newtype Service' = Service' {
  serviceName :: Text
} deriving Show


data EndpointWithContext = EndpointWithContext {
  endpoint                               :: Endpoint
, endpointWithContextEndpointType        :: EndpointType
, endpointWithContextService             :: Service'
, endpointWithContextEnvironment         :: EnvironmentName
} deriving Show

data EndpointType = Ping | HealthCheck deriving (Show, Eq)
