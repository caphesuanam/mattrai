module ResultJson where

import Data.Aeson (ToJSON, toJSON, object, (.=), Value(..))
import Data.Text (Text)
import Data.Vector as V
import GHC.Generics

import CoreDataTypes

newtype ResultServices = ResultServices[ResultService] deriving Generic
-- instance ToJSON ResultServices

data ResultService = ResultService {
  resServiceName :: Text
, resServiceEnvironments :: [ResultEnvironment] -- transition from instances to envs
--, resInstances   :: [ResultInstance]
}
-- instance ToJSON ResultService where
--   toJSON (ResultService name envs inst) =
--     object [ "name"      .= name
--            , "instances" .= inst
--            ]

data ResultEnvironment = ResultEnvironment {
  resultEnvironmentName   :: Text
, resultInstances         :: [ResultInstance]
}

data ResultInstance = ResultInstance {
  resultInstanceEnvironmentName      :: Text
, resultInstancePingEndpoint         :: Endpoint
, resultInstancePingResult           :: PingResult
, resultInstanceDocumentation        :: Endpoint
, resultInstanceHealthCheckResults   :: [ResultHealthCheck]
}
instance ToJSON ResultInstance where
  toJSON (ResultInstance name pingEndpoint pingResult _ healthCheckResults) =
    object [ "name"    .= name
           , "pingUrl" .= pingEndpoint
           , "status"  .=
             object [
               "pingStatus"        .= pingResult
             , "healthCheckStatus" .= (Array . V.concat . Prelude.map fii $ healthCheckResults)
             ]
           ]

data ResultHealthCheck = ResultHealthCheck {
  healthCheckEndpoint    :: Endpoint
, healthCheckResultItems  :: [HealthCheckResult]
} deriving Show
instance ToJSON ResultHealthCheck where
  toJSON = Array . fii

fii (ResultHealthCheck endpoint items) =
      V.map (\item -> object [ "healthCheckUrl" .= endpoint
                             , "result"         .= item
                     ])
            $ V.fromList items
