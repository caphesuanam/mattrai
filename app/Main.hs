module Main where

import Happstack.Server ( simpleHTTP, nullConf, ok, toResponse )
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Monad (liftM)

import Config
import CoreDatatypes
import Render (topLevelPage)
import ResultJson
import StatusCheck (healthCheckStatus, ping)


newtype HealthCheckEndpoint = HealthCheckEndpoint String

pingEndpoints :: [EndpointWithContext]
pingEndpoints = concatMap
                  (\service ->
                     map (\inst ->
                          EndpointWithContext{
                              endpointWithContextEndpointType = Ping,
                              endpoint                        = instPingEndpoint inst,
                              endpointWithContextService      = serName service,
                              endpointWithContextEnvironment  = instEnvironmentName inst
                          }) (serInstances service))
                  services

healthCheckEndpoints :: [EndpointWithContext]
healthCheckEndpoints = concat $ concatMap
                          (\service ->
                             map (\inst ->
                               map (\ep ->
                                 EndpointWithContext{
                                     endpointWithContextEndpointType = HealthCheck,
                                     endpoint                        = ep,
                                     endpointWithContextService      = serName service,
                                     endpointWithContextEnvironment  = instEnvironmentName inst
                                 }) (instHealthCheckEndpoints inst)) $ serInstances service)
                          services


mapServicesToJSON :: IO ResultServices
mapServicesToJSON =
  ResultServices <$> mapM mapServiceToResultService services

mapServiceToResultService :: Service'' -> IO ResultService
mapServiceToResultService service =
  do r <- mapM mapInstancesToResultInstance $ serInstances service
     return $ ResultService {
       resServiceName = serviceName $ serName service
     , resInstances   = r
     }

mapInstancesToResultInstance :: Instance -> IO ResultInstance
mapInstancesToResultInstance inst =
  do pingResult <- ping $ instPingEndpoint inst
     aHealthCheckResult <- mapM mapHealthCheckEndpointToResult $ instHealthCheckEndpoints inst
     putStrLn $ show aHealthCheckResult
     return $ ResultInstance {
       resultInstanceEnvironmentName     = environmentNameAsText $ instEnvironmentName inst
     , resultInstancePingEndpoint        = instPingEndpoint inst
     , resultInstancePingResult          = pingResult
     , resultInstanceDocumetation        = docs inst
     , resultInstanceHealthCheckResults  = aHealthCheckResult
     }

mapHealthCheckEndpointToResult :: Endpoint -> IO ResultHealthCheck
mapHealthCheckEndpointToResult endpoint =
  do healthCheckResult <- healthCheckStatus endpoint
     return $ ResultHealthCheck {
       healthCheckEndpoint    = endpoint
     , healthCheckResultItems = healthCheckResult
     }


main :: IO ()
main = mapServicesToJSON >>= simpleHTTP nullConf . ok . toResponse . topLevelPage   --("Hello World!" :: String)

-- http://vtp-la-int.dev.cosmic.sky/legacy-adapter-app/health