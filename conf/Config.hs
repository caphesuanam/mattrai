module Config where

import Data.Text (Text)

import Mattrai.Endpoint
import Mattrai.Service
import Mattrai

integration :: EnvironmentName
integration = Environment "Integration"

production :: EnvironmentName
production = Environment "Production"

preProd :: EnvironmentName
preProd = Environment "Pre-prod"

description :: Text
description = "Description"

owner :: Text
owner = "owner"

allEnvironments :: [EnvironmentName]
allEnvironments = [integration, preProd, production]

config = defaultConfig {
  servicesToMonitor = testServices
, environmentsToMonitor = [integration, preProd, production]
, footer = "<a href=\"http://bing.com\">Bing</a>"
}

testServices =
  [
    Service {
      _serName = ServiceName "Google"
    , _serInstances = [
        Instance {
          _instEnvironmentName = production
        , _instPingEndpoint    = Endpoint "http://google.com"
        , _instMiscEndpoints = [
            DocsEndpoint $ Endpoint "https://about.google"
          , LogsEndpoint $ Endpoint "http://google.com?q=logs"
          , MiscEndpoint "Business Verification" (Endpoint "https://google.com/verifymybusiness")
          , HealthCheckEndpoint $ Endpoint "http://0.0.0.0:8080/healthcheck/happy"
          ]
        , _instStaticInfo    = [
            "description" --> "Use to find stuff"
          , "owner"       --> "Alphabet"
          ]
        }
      , Instance {
          _instEnvironmentName = production
        , _instPingEndpoint    = Endpoint "http://google.com"
        , _instMiscEndpoints = [
            DocsEndpoint $ Endpoint "https://about.google"
          , LogsEndpoint $ Endpoint "http://google.com?q=logs"
          , MiscEndpoint "Business Verification" (Endpoint "https://google.com/verifymybusiness")
          , HealthCheckEndpoint $ Endpoint "http://0.0.0.0:8080/healthcheck/happy2"
          ]
        , _instStaticInfo    = [
            "description" --> "Use to find stuff"
          , "owner"       --> "Alphabet"
          ]
        }
      ]
    }
  , Service {
      _serName = ServiceName "Yahoo"
    , _serInstances = [
        Instance {
          _instEnvironmentName = production
        , _instPingEndpoint    = Endpoint "http://yahoo.com"
        , _instMiscEndpoints = [
          ]
        , _instStaticInfo    = []
        }
      ]
    }
  ]




