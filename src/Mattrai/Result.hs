{-# LANGUAGE TemplateHaskell #-}
module Mattrai.Result where

import Control.Lens
import Data.Hashable (hash)
import Data.Text (Text, append, pack)
import Data.Vector as V
import GHC.Generics (Generic)

import Mattrai.Endpoint
import Mattrai.Service

newtype HealthCheckItem = HealthCheckItem {_healthCheckItemName :: Text} deriving (Show)
makeLenses ''HealthCheckItem

data HealthCheckItemStatus = Up
                           | Down
                           deriving (Generic, Show)
makePrisms ''HealthCheckItemStatus

data HealthCheckResult = HealthCheckResult {
  _healthCheckResultItemName   :: HealthCheckItem
, _healthCheckResultItemStatus :: HealthCheckItemStatus
} deriving (Show)

makeLenses ''HealthCheckResult

data PingResult = Timeout
                | CannotConnect
                | DnsFailure
                | UnknownFailure
                | UnknownFailure2
                | OtherFailure Text
                | HttpCode Int
                deriving (Show, Eq)

newtype ResultServices = ResultServices[ResultService]
-- instance ToJSON ResultServices

data ResultService = ResultService {
  resServiceName :: Text
, resServiceEnvironments :: [ResultEnvironment]
}

data ResultEnvironment = ResultEnvironment {
  resultEnvironmentName   :: Text
, resultInstances         :: [ResultInstance]
}

data ResultInstance = ResultInstance {
  resultInstanceEnvironmentName      :: Text
, resultInstancePingEndpoint         :: Endpoint
, resultInstancePingResult           :: PingResult
, resultInstanceDocumentation        :: [Endpoint]
, resultInstanceLogs                 :: [Endpoint]
, resultInstanceMiscEndpoints        :: [(Text,Endpoint)]
, resultInstanceHealthCheckResults   :: [ResultHealthCheck]
, information                        :: [(Text,Text)]
}

instanceId :: ResultInstance -> Text
instanceId = append "instance-" . pack . show . hash . resultInstancePingEndpoint

data ResultHealthCheck = ResultHealthCheck {
  healthCheckEndpoint    :: Endpoint
, healthCheckResultItems  :: [HealthCheckResult]
} deriving Show

