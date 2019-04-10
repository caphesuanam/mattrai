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
                              endpoint                        = _instPingEndpoint inst,
                              endpointWithContextService      = _serName service,
                              endpointWithContextEnvironment  = _instEnvironmentName inst
                          }) (_serInstances service))
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

mapServiceToResultService :: [Text] -> Service'' -> IO ResultService
mapServiceToResultService envNames service =
  ResultService (serviceName $ _serName service)
                <$> Par.mapM (
                      \envName -> mapInstancesToEnvironment envName (filter ((==  envName) . environmentNameAsText . _instEnvironmentName) (_serInstances service))) envNames

mapInstancesToEnvironment :: Text -> [Instance] -> IO ResultEnvironment
mapInstancesToEnvironment envName insts =
  do newInstances <- mapM mapInstanceToResultInstance insts
     return $ ResultEnvironment {
       resultEnvironmentName = envName
     , resultInstances       = newInstances
     }

mapInstanceToResultInstance :: Instance -> IO ResultInstance
mapInstanceToResultInstance inst = do
  pingResult <- ping $ _instPingEndpoint inst
  bHealthCheckResult <- mapM (mapHealthCheckEndpointToResult . getEndpoint) (filter isHealthcheck $ _instMiscEndpoints inst)
  print bHealthCheckResult
  return $
    ResultInstance
      { resultInstanceEnvironmentName = environmentNameAsText $ _instEnvironmentName inst
      , resultInstancePingEndpoint = _instPingEndpoint inst
      , resultInstancePingResult = pingResult
      , resultInstanceDocumentation = map getEndpoint $ filter isDoc $ _instMiscEndpoints inst
      , resultInstanceLogs          = map getEndpoint $ filter isLog $ _instMiscEndpoints inst
      , resultInstanceHealthCheckResults = bHealthCheckResult
      , resultInstanceMiscEndpoints = map (\misc -> (getMiscEndpointName misc, getEndpoint misc)) $ filter isMisc $ _instMiscEndpoints inst
      , information = _instStaticInfo inst
      }

mapHealthCheckEndpointToResult :: Endpoint -> IO ResultHealthCheck
mapHealthCheckEndpointToResult endpoint =
  do healthCheckResult <- healthCheckStatus endpoint
     return $ ResultHealthCheck {
       ResultJson.healthCheckEndpoint    = endpoint
     , healthCheckResultItems = healthCheckResult
     }

loop ref = do _ <- mapServicesToJSON' ref
              threadDelay sixtySeconds
              loop ref
       where sixtySeconds = 60000000

main :: IO ()
main = do updateGlobalLogger rootLoggerName (setLevel INFO)

          ref <- emptyResultServices
          _ <- forkIO $ loop ref

          simpleHTTP nullConf $
            msum [
                   dir "static" $ serveDirectory DisableBrowsing [] "static"
                 , dir "report" $ displayResultPage ref report
                 , nullDir >> (displayResultPage ref $ topLevelPage allEnvironments')
                 ]
           where displayResultPage ref renderer = liftIO (readIORef ref) >>= ok . toResponse . renderer

