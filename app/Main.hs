{-# LANGUAGE FlexibleContexts #-}
module Main where


import           Control.Concurrent         (forkIO, threadDelay)
import qualified Control.Monad.Parallel as Par (mapM)
import Happstack.Server ( simpleHTTP, nullConf, ok, toResponse, serveDirectory, dir)
import Happstack.Server.FileServe (Browsing(DisableBrowsing))
import Happstack.Server.Routing (nullDir)
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.AutoUpdate (mkAutoUpdate, defaultUpdateSettings, UpdateSettings(updateAction, updateFreq))
import Control.Monad (liftM, msum, forever)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List (sort)
import Data.Set as Set (toList, fromList)
import Data.Text (Text(..))
import System.Log.Logger ( updateGlobalLogger
                         , rootLoggerName
                         , setLevel
                         , Priority(..)
                         , infoM
                         )

import Config
import CoreDataTypes
import Render (topLevelPage, report)
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

emptyResultServices :: IO (IORef ResultServices)
emptyResultServices = newIORef $ ResultServices []


mapServicesToJSON :: IO ResultServices
mapServicesToJSON =
     let envNames = allEnvironments' -- services
     in ResultServices <$> mapM (mapServiceToResultService envNames) services

mapServicesToJSON' :: IORef ResultServices -> IO ResultServices
mapServicesToJSON' ref = do infoM "FOO.BAR" "Retriving statuses"
                            t <- mapServicesToJSON
                            writeIORef ref t
                            readIORef ref
allEnvironments' :: [Text]
allEnvironments' = map environmentNameAsText allEnvironments

-- allEnvironments :: [Service''] -> [Text]
-- allEnvironments = sort . mkUniq . map (environmentNameAsText . instEnvironmentName ) . concatMap serInstances

mapServiceToResultService :: [Text] -> Service'' -> IO ResultService
mapServiceToResultService envNames service =
  ResultService (serviceName $ serName service)
                <$> Par.mapM (
                      \envName -> mapInstancesToEnvironment envName (filter ((==  envName) . environmentNameAsText . instEnvironmentName) (serInstances service))) envNames

mapInstancesToEnvironment :: Text -> [Instance] -> IO ResultEnvironment
mapInstancesToEnvironment envName insts =
  do newInstances <- mapM mapInstanceToResultInstance insts
     return $ ResultEnvironment {
       resultEnvironmentName = envName
     , resultInstances       = newInstances
     }

mapInstanceToResultInstance :: Instance -> IO ResultInstance
mapInstanceToResultInstance inst = do
  pingResult <- ping $ instPingEndpoint inst
  bHealthCheckResult <- mapM (mapHealthCheckEndpointToResult . getEndpoint) (filter isHealthcheck $ miscEndpoints inst)
  print bHealthCheckResult
  return $
    ResultInstance
      { resultInstanceEnvironmentName = environmentNameAsText $ instEnvironmentName inst
      , resultInstancePingEndpoint = instPingEndpoint inst
      , resultInstancePingResult = pingResult
      , resultInstanceDocumentation = map getEndpoint $ filter isDoc $ miscEndpoints inst
      , resultInstanceLogs          = map getEndpoint $ filter isLog $ miscEndpoints inst
      , resultInstanceHealthCheckResults = bHealthCheckResult
      , resultInstanceMiscEndpoints = map (\misc -> (getMiscEndpointName misc, getEndpoint misc)) $ filter isMisc $ miscEndpoints inst
      , information = staticInfo inst
      }

mapHealthCheckEndpointToResult :: Endpoint -> IO ResultHealthCheck
mapHealthCheckEndpointToResult endpoint =
  do healthCheckResult <- healthCheckStatus endpoint
     return $ ResultHealthCheck {
       ResultJson.healthCheckEndpoint    = endpoint
     , healthCheckResultItems = healthCheckResult
     }


main :: IO ()
main = do let sixtySeconds = 60000000
          updateGlobalLogger rootLoggerName (setLevel INFO)

          ref <- emptyResultServices
          _ <- forkIO $
                  do _ <- mapServicesToJSON' ref
                     threadDelay sixtySeconds
                     return ()

          simpleHTTP nullConf $
            msum [
                   dir "static" $ serveDirectory DisableBrowsing [] "static"
                 , dir "report" $ displayResultPage ref report
                 , nullDir >> (displayResultPage ref $ topLevelPage allEnvironments')
                 ]
           where displayResultPage ref renderer = liftIO (readIORef ref) >>= ok . toResponse . renderer

