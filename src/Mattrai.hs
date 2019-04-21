{-# LANGUAGE FlexibleContexts #-}
-- |A module for running a server to monitor services across environments
module Mattrai (runMattrai, defaultConfig, MattraiConfig(..))
where

import           Control.Concurrent         (forkIO, threadDelay)
import qualified Control.Monad.Parallel as Par (mapM)
import Happstack.Server ( simpleHTTP, nullConf, ok, toResponse, serveDirectory, dir)
import Happstack.Server.FileServe (Browsing(DisableBrowsing))
import Happstack.Server.Routing (nullDir)
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.AutoUpdate (mkAutoUpdate, defaultUpdateSettings, UpdateSettings(updateAction, updateFreq))
import Control.Lens (traverse, (^..), (^.))
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

import Mattrai.Service
import Mattrai.Render (topLevelPage, report)
import Mattrai.ResultJson
import Mattrai.StatusCheck (healthCheckStatus, ping)

-- |Default settings that should be overridden. Start by just overrriding `servicesToMonitor` and `environmentsToMonitor`.
defaultConfig :: MattraiConfig
defaultConfig = MattraiConfig {
  servicesToMonitor     = []
, environmentsToMonitor = []
, footer                = ""
}

data MattraiConfig = MattraiConfig {
  servicesToMonitor     :: [Service]
, environmentsToMonitor :: [EnvironmentName] -- |The ordering for the columns at the top of the status table.
, footer                :: Text
}

mkUniq :: Ord a => [a] -> [a]
mkUniq = Set.toList . Set.fromList

newtype HealthCheckEndpoint = HealthCheckEndpoint String

emptyResultServices :: IO (IORef ResultServices)
emptyResultServices = newIORef $ ResultServices []

mapServicesToJSON :: [EnvironmentName] -> [Service] -> IO ResultServices
mapServicesToJSON envs services =
     ResultServices <$> mapM (mapServiceToResultService envNames) services
     where envNames = allEnvironments' envs

mapServicesToJSON' :: [EnvironmentName] -> [Service] -> IORef ResultServices -> IO ResultServices
mapServicesToJSON' envs services ref = do infoM "FOO.BAR" "Retriving statuses"
                                          mapServicesToJSON envs services >>= writeIORef ref
                                          readIORef ref

allEnvironments' :: [EnvironmentName] -> [Text]
allEnvironments' = (^.. traverse . environmentNameAsString)

mapServiceToResultService :: [Text] -> Service -> IO ResultService
mapServiceToResultService envNames service =
  ResultService (serviceName $ _serName service)
                <$> Par.mapM (
                      \envName -> mapInstancesToEnvironment envName (filter ((==  envName) . (^. instEnvironmentName . environmentNameAsString) ) (_serInstances service))) envNames

mapInstancesToEnvironment :: Text -> [Instance] -> IO ResultEnvironment
mapInstancesToEnvironment envName insts =
  do newInstances <- mapM mapInstanceToResultInstance insts
     return $ ResultEnvironment {
       resultEnvironmentName = envName
     , resultInstances       = newInstances
     }

mapInstanceToResultInstance :: Instance -> IO ResultInstance
mapInstanceToResultInstance inst = do
  pingResult <- ping $ inst ^. instPingEndpoint
  bHealthCheckResult <- mapM mapHealthCheckEndpointToResult $ inst ^.. instMiscEndpoints . traverse . _HealthCheckEndpoint
  print bHealthCheckResult
  return $
    ResultInstance
      { resultInstanceEnvironmentName = inst ^. instEnvironmentName . environmentNameAsString
      , resultInstancePingEndpoint = inst ^. instPingEndpoint
      , resultInstancePingResult = pingResult
      , resultInstanceDocumentation = inst ^.. instMiscEndpoints . traverse . _DocsEndpoint
      , resultInstanceLogs          = inst ^.. instMiscEndpoints . traverse . _LogsEndpoint
      , resultInstanceHealthCheckResults = bHealthCheckResult
      , resultInstanceMiscEndpoints = inst ^.. instMiscEndpoints . traverse . _MiscEndpoint
      , information = inst ^. instStaticInfo
      }

mapHealthCheckEndpointToResult :: Endpoint -> IO ResultHealthCheck
mapHealthCheckEndpointToResult endpoint =
  do healthCheckResult <- healthCheckStatus endpoint
     return $ ResultHealthCheck {
       Mattrai.ResultJson.healthCheckEndpoint = endpoint
     , healthCheckResultItems = healthCheckResult
     }

loop envs services ref = do _ <- mapServicesToJSON' envs services ref
                            threadDelay sixtySeconds
                            loop envs services ref
       where sixtySeconds = 60000000

-- |Start a Mattrai server by simply calling `runMattrai` and passing `defaultConfig` with the necessary config overridden.
runMattrai :: MattraiConfig -> IO ()
runMattrai config =
          do updateGlobalLogger rootLoggerName (setLevel INFO)

             ref <- emptyResultServices
             _ <- forkIO $ loop allEnvironments services ref

             simpleHTTP nullConf $
               msum [
                      dir "static" $ serveDirectory DisableBrowsing [] "static"
                    , dir "report" $ displayResultPage ref report
                    , nullDir >> displayResultPage ref (topLevelPage pageFooter (allEnvironments' allEnvironments))
                    ]
              where displayResultPage ref renderer = liftIO (readIORef ref) >>= ok . toResponse . renderer
                    services = servicesToMonitor config
                    pageFooter = footer config
                    allEnvironments = environmentsToMonitor config

