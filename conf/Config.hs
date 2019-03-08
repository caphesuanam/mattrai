module Config where

import CoreDataTypes
import Data.Text (Text)

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

services =
  [
    Service {
      serName = ServiceName "Google"
    , serInstances = [
        Instance {
          instEnvironmentName = production
        , instPingEndpoint    = Endpoint "http://google.com"
        , miscEndpoints = [
            DocsEndpoint $ Endpoint "https://about.google"
          ]
        , staticInfo    = [
            "description" --> "Use to find stuff"
          , "owner"       --> "Alphabet"
          ]
        }
      ]
    }
  , Service {
      serName = ServiceName "Yahoo"
    , serInstances = [
        Instance {
          instEnvironmentName = production
        , instPingEndpoint    = Endpoint "http://yahoo.com"
        , miscEndpoints = [
          ]
        , staticInfo    = []
        }
      ]
    }
  ]




