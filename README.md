# Mattrai

An eye on all your services. Mattrai is a dashboard that shows services deployed accross environements in one place, checks they are up, confirms healthcheck status, and shows meta information about the services. This includes:
* Documentation links
* Logging links
* Ping Endpoints to confirm a service is up
* Any Meta information yuo desire, e.g. team names, identifiers, descriptions etc.

## Getting Started
1. `mkdir conf`
1. `touch /conf/Conf.hs`
1. Enter the following in `/conf/Conf.hs`:
```module Config where

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
```
1. Then run
```docker run -p 8080:8000 -t mattrai caphesuanam/mattrai```

1. Open `0.0.0.0:8080` in your browser

## Quick reference

* Where to file issues:
https://github.com/caphesuanam/mattrai/issues


