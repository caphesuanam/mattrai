module ResultJson where

import Data.Aeson (ToJSON, toJSON, object, (.=), Value(..))
import Data.Text (Text)
import Data.Vector as V
import GHC.Generics

import CoreDatatypes

newtype ResultServices = ResultServices[ResultService] deriving Generic
instance ToJSON ResultServices

data ResultService = ResultService {
  resServiceName :: Text
, resInstances :: [ResultInstance]
}
instance ToJSON ResultService where
  toJSON (ResultService name inst) =
    object [ "name"      .= name
           , "instances" .= inst
           ]

data ResultEnvironment = ResultEnvironment {
  resultInstanceName      :: Text
, resultInstances         :: [ResultInstance]
}

data ResultInstance = ResultInstance {
  resultInstanceEnvironmentName      :: Text
, resultInstancePingEndpoint         :: Endpoint
, resultInstancePingResult           :: PingResult
, resultInstanceDocumetation         :: Endpoint
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
  --toJSON (ResultHealthCheck endpoint items) =
  --  Array $
  --    V.map (\item -> object [ "healthCheckUrl" .= endpoint
  --                           , "result"         .= item
  --                   ])
  --          $ V.fromList items

fii (ResultHealthCheck endpoint items) =
      V.map (\item -> object [ "healthCheckUrl" .= endpoint
                             , "result"         .= item
                     ])
            $ V.fromList items
