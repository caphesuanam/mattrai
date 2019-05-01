-- |Helpers for easily making changes to Mattrai services and instances in a readable way
module Mattrai.ConfigHelpers where

import Control.Lens
import Data.Text (Text)

import Mattrai.Endpoint
import Mattrai.Service

-- |Given a service and a key/value tuple, add the key and value as meta information for all instances.
-- This works well when called as an infix function and allows the with* functions to be stacked one after another
withAttribute :: Service -> (Text, Text) -> Service
withAttribute service (name, value) = over (serInstances . traverse . instStaticInfo)
                                           ((name,value) :)
                                           service

withDynamicProperty :: Service -> (Instance -> DynamicProperty) -> Service
withDynamicProperty service propertyGenerator =
      over (serInstances . traverse)
           (\inst -> over instDynamicInfo
                          (propertyGenerator inst :)
                          inst)
           service

-- | Given a service and and endpoint add the endpoint to all instances
-- This works well when called as an infix function and allows the with* functions to be stacked one after another
withMiscEndpoint :: Service ->  MiscEndpoint -> Service
withMiscEndpoint service ep = over (serInstances . traverse . instMiscEndpoints)
                                   (ep :)
                                   service


