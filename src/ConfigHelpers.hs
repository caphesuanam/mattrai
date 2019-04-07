module ConfigHelpers where

import Control.Lens
import Data.Text (Text)

import CoreDataTypes

withAttribute :: Service'' -> (Text, Text) -> Service''
withAttribute service (name, value) = over (serInstances . traverse . instStaticInfo)
                                           ((name,value) :)
                                           service

withMiscEndpoint :: Service'' ->  (Text, MiscEndpoint) -> Service''
withMiscEndpoint service (name, ep) = over (serInstances . traverse . instMiscEndpoints)
                                          (ep :)
                                          service


