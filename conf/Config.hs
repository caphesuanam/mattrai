module Config where

import Data.Text (Text)

import Data.Aeson.Lens (key, _String)
import Network.Wreq (responseBody)
import Mattrai.ConfigHelpers
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
    service "Google" [
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
        , _instDynamicInfo = [
            DynamicProperty "Dynamic information"
                            (Endpoint "http://0.0.0.0:8080/info/happy")
                            (responseBody . key "path" . key "to" . _String)
          ]
        }
      , Instance {
          _instEnvironmentName = production
        , _instPingEndpoint    = Endpoint "http://google.co.uk"
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
        , _instDynamicInfo = [ ]
        }
      ]
  , service "Yahoo" [
        serviceInstance production "http://yahoo.com"
    ]
  ]




