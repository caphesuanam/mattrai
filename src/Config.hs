 module Config where

import CoreDataTypes

functional :: EnvironmentName
functional = Environment "Functional"

integration :: EnvironmentName
integration = Environment "Integration"

production :: EnvironmentName
production = Environment "Production"

showcase :: EnvironmentName
showcase = Environment "Showcase"



data Service'' = Service {
  serName      :: Service'
, serInstances :: [Instance]
}

data Instance = Instance {
  instEnvironmentName      :: EnvironmentName
, instPingEndpoint         :: Endpoint
, docs                     :: Endpoint
, instHealthCheckEndpoints :: [Endpoint]
}

services :: [Service'']
services = [
  Service {
    serName = Service' "Service1"
,   serInstances = [
      Instance {
        instEnvironmentName      = integration
      , instPingEndpoint         = Endpoint "http://google.com"
      , instHealthCheckEndpoints = [Endpoint "http://localhost:8080/healthcheck/happy"]
      , docs                     = Endpoint "http://news.bbc.co.uk"
      }
    ,
      Instance {
        instEnvironmentName      = production
      , instPingEndpoint         = Endpoint "http://dnsprodafailure"
      , instHealthCheckEndpoints = [Endpoint "http://dnshealthcheckfailure:8080/foo/bar"]
      , docs                     = Endpoint "http://news.bbc.co.uk"
      }
    ]
  },
  Service {
    serName = Service' "Service2"
  , serInstances = [
      Instance {
        instEnvironmentName      = production
      , instPingEndpoint         = Endpoint "http://localhost:8080/fourOfourresponse"
      , instHealthCheckEndpoints = [] -- [Endpoint "http://dnshealthcheckfailure:8080/foo/bar"]
      , docs                     = Endpoint "http://news.bbc.co.uk"
      }
    ]
}]

