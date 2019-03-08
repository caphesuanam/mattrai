module Config where

import CoreDataTypes

integration :: EnvironmentName
integration = Environment "Integration"

production :: EnvironmentName
production = Environment "Production"

preProd :: EnvironmentName
preProd = Environment "Pre-prod"

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
        , miscEndpoints = []
        , staticInfo    = []
        }
      ]
    }
  ]




