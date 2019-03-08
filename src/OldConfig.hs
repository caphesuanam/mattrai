 module OldConfig where

import CoreDataTypes
import Config

services :: [Service'']
services = [
  Service {
    serName = ServiceName "Google"
  , serInstances = [
      Instance {
        instEnvironmentName   = integration
      , instPingEndpoint      = Endpoint "http://google.com"
      , miscEndpoints = []
      , staticInfo = []
      }
    ]
  }
  , Service {
    serName = ServiceName "Service1"
,   serInstances = [
      Instance {
        instEnvironmentName      = integration
      , instPingEndpoint         = Endpoint "http://localhost:8080/healthcheck/happy"
      , miscEndpoints            = [
          LogsEndpoint $ Endpoint "http://not/logs"
        , DocsEndpoint $ Endpoint "http://news.bbc.co.uk"
        , HealthCheckEndpoint $ Endpoint "http://localhost:8080/healthcheck/happy"
        ]
      , staticInfo             = [("Server CI", "Something")]
      }
    ,
      Instance {
        instEnvironmentName      = production
      , instPingEndpoint         = Endpoint "http://dnsprodafailure"
      , miscEndpoints            = [
          DocsEndpoint $ Endpoint "http://news.bbc.co.uk"
        , HealthCheckEndpoint $ Endpoint "http://dnshealthcheckfailure:8080/foo/bar"
        ]
      , staticInfo             = []
      }
    ]
  },
  Service {
    serName = ServiceName "Service2"
  , serInstances = [
      Instance {
        instEnvironmentName      = production
      , instPingEndpoint         = Endpoint "http://localhost:8080/fourOfourresponse"
      -- [Endpoint "http://dnshealthcheckfailure:8080/foo/bar"]
      , miscEndpoints            = [
        DocsEndpoint $ Endpoint "http://news.bbc.co.uk"
      ]
      , staticInfo             = []
      }
    ]
}]

