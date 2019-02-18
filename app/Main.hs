module Main where

import Happstack.Server ( simpleHTTP, nullConf, ok, toResponse )
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Monad (liftM)
import Data.List (sort)
import Data.Set as Set (toList, fromList)
import Data.Text (Text(..))

import Config
import CoreDataTypes
import Render (topLevelPage)
import ResultJson
import StatusCheck (healthCheckStatus, ping)

mkUniq :: Ord a => [a] -> [a]
mkUniq = Set.toList . Set.fromList

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
     let envNames = allEnvironments services
     in ResultServices <$> mapM (mapServiceToResultService envNames) services

allEnvironments :: [Service''] -> [Text]
allEnvironments = sort . mkUniq . map (environmentNameAsText . instEnvironmentName ) . concatMap serInstances

mapServiceToResultService :: [Text] -> Service'' -> IO ResultService
mapServiceToResultService envNames service =
  do s <- mapM (\envName -> mapInstancesToEnvironment envName (filter ((==  envName) . environmentNameAsText . instEnvironmentName) (serInstances service))) envNames
     return $ ResultService {
       resServiceName = serviceName $ serName service
     , resServiceEnvironments = s
     }

mapInstancesToEnvironment :: Text -> [Instance] -> IO ResultEnvironment
mapInstancesToEnvironment envName insts =
  do newInstances <- mapM mapInstanceToResultInstance insts
     return $ ResultEnvironment {
       resultEnvironmentName = envName
     , resultInstances       = newInstances
     }

mapInstanceToResultInstance :: Instance -> IO ResultInstance
mapInstanceToResultInstance inst =
  do pingResult <- ping $ instPingEndpoint inst
     aHealthCheckResult <- mapM mapHealthCheckEndpointToResult $ instHealthCheckEndpoints inst
     print aHealthCheckResult
     return $ ResultInstance {
       resultInstanceEnvironmentName     = environmentNameAsText $ instEnvironmentName inst
     , resultInstancePingEndpoint        = instPingEndpoint inst
     , resultInstancePingResult          = pingResult
     , resultInstanceDocumentation       = docs inst
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
main = mapServicesToJSON >>= simpleHTTP nullConf . ok . toResponse . topLevelPage (allEnvironments services)

-- http://vtp-la-int.dev.cosmic.sky/legacy-adapter-app/health