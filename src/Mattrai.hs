{-# LANGUAGE FlexibleContexts, RankNTypes #-}
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
import Control.Lens (traverse, (^..), (^.), Lens')
import Control.Lens.Getter (Getting)
import Control.Monad (liftM, msum, forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Lens (key, _String, _Object, _Array)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.IORef
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Monoid (First)
import Data.Set as Set (toList, fromList)
import Data.Text (Text(..), pack)
import Network.Wreq (Response, responseBody)
import System.Log.Logger ( updateGlobalLogger
                         , rootLoggerName
                         , setLevel
                         , Priority(..)
                         , infoM
                         )

import Mattrai.Endpoint
import Mattrai.Service
import Mattrai.Render (topLevelPage, report)
import Mattrai.Result
import Mattrai.StatusCheck (healthCheckStatus, ping, getDynamicInformation)

infoLog = infoM "Matrai"

-- |Default settings that should be overridden. Start by just overrriding `servicesToMonitor` and `environmentsToMonitor`.
defaultConfig :: MattraiConfig
defaultConfig = MattraiConfig {
  servicesToMonitor     = []
, environmentsToMonitor = []
, footer                = ""
}

-- |Configuration for a Mattrai server.
-- servicesToMonitor and environmentsToMonitor should be provided as a minimum.
data MattraiConfig = MattraiConfig {
  -- |Services to be reported as rows on the dashboard
  servicesToMonitor     :: [Service]
  -- |The ordering for the columns at the top of the status table.
, environmentsToMonitor :: [EnvironmentName]
  -- |Footers allow HTML strings to be embedded directly under the dashboard.
  -- Use this to insert strings for example.
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

mapServicesToJSON' :: [EnvironmentName] -> [Service] -> IORef ResultServices -> IO ()
mapServicesToJSON' envs services ref = do infoLog "Retriving statuses"
                                          mapServicesToJSON envs services >>= writeIORef ref

allEnvironments' :: [EnvironmentName] -> [Text]
allEnvironments' = (^.. traverse . environmentNameAsString)

mapServiceToResultService :: [Text] -> Service -> IO ResultService
mapServiceToResultService envNames service =
  ResultService (serviceName $ _serName service)
                <$> Par.mapM (
                      \envName -> mapInstancesToEnvironment envName (filter ((==  envName) . (^. instEnvironmentName . environmentNameAsString) ) (_serInstances service))) envNames

mapInstancesToEnvironment :: Text -> [Instance] -> IO ResultEnvironment
mapInstancesToEnvironment envName insts =
     ResultEnvironment envName <$> mapM mapInstanceToResultInstance insts

getSingleDynamicValue :: DynamicProperty -> IO (Text, Text)
getSingleDynamicValue (DynamicProperty name url accessor) =
       (,) name . fromMaybe "<Missing value" <$> getDynamicInformation url accessor

mapInstanceToResultInstance :: Instance -> IO ResultInstance
mapInstanceToResultInstance inst = do
  pingResult <- ping $ inst ^. instPingEndpoint
  bHealthCheckResult <- mapM mapHealthCheckEndpointToResult $ inst ^.. instMiscEndpoints . traverse . _HealthCheckEndpoint
  dynamicValues <- mapM getSingleDynamicValue (inst ^. instDynamicInfo)
  infoLog $ "Got dynamic values: " ++ show dynamicValues
  print bHealthCheckResult
  return $
    ResultInstance
      { resultInstanceEnvironmentName    = inst ^. instEnvironmentName . environmentNameAsString
      , resultInstancePingEndpoint       = inst ^. instPingEndpoint
      , resultInstancePingResult         = pingResult
      , resultInstanceDocumentation      = inst ^.. instMiscEndpoints . traverse . _DocsEndpoint
      , resultInstanceLogs               = inst ^.. instMiscEndpoints . traverse . _LogsEndpoint
      , resultInstanceHealthCheckResults = bHealthCheckResult
      , resultInstanceMiscEndpoints      = inst ^.. instMiscEndpoints . traverse . _MiscEndpoint
      , information                      = dynamicValues ++ inst ^. instStaticInfo
      }

mapHealthCheckEndpointToResult :: Endpoint -> IO ResultHealthCheck
mapHealthCheckEndpointToResult endpoint =
   ResultHealthCheck endpoint <$> healthCheckStatus endpoint


loop envs services ref = do mapServicesToJSON' envs services ref
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

