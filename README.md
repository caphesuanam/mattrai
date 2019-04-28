# Mattrai

An eye on all your services. Mattrai is a dashboard that shows services deployed across environments in one place, checks they are up, confirms health check status, and shows meta information about the services. This includes:
* Documentation links
* Logging links
* Ping Endpoints to confirm a service is up
* Any Meta information yuo desire, e.g. team names, identifiers, descriptions etc.

![Pings occur periodically regardless of calls to the service](https://raw.githubusercontent.com/caphesuanong/mattrai/master/docs/mattrai.png)


## Getting Started
1. `mkdir conf`
1. `touch conf/Config.hs`
1. Enter the following in `conf/Main.hs`:

```
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

main :: IO ()
main = runMattrai defaultConfig {
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
```

1. Then run

```docker run -d -p 8080:8000 -v `pwd`/conf:/service/conf --name mattrai caphesuanong/mattrai```

1. Wait about 10 seconds.

1. Open `http://0.0.0.0:8080` in your browser

1. You can monitor progress with `docker logs mattrai` if things are a bit slow or if something went wrong.

## Refactoring and maintaining

All Mattrai modules are documented with [Haddock](https://www.haskell.org/haddock/).
Mattrai can be built as a separate library and imported into your applications as required.

## Quick reference

* Where to file issues:
<https://github.com/caphesuanong/mattrai/issues>


