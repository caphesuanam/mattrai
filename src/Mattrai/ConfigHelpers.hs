module Mattrai.ConfigHelpers where

import Control.Lens
import Data.Text (Text)

import Mattrai.Service

-- |Given a service and a
withAttribute :: Service -> (Text, Text) -> Service
withAttribute service (name, value) = over (serInstances . traverse . instStaticInfo)
                                           ((name,value) :)
                                           service

withMiscEndpoint :: Service ->  MiscEndpoint -> Service
withMiscEndpoint service ep = over (serInstances . traverse . instMiscEndpoints)
                                   (ep :)
                                   service


