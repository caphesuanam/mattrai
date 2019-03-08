module StatusCheck where

import Prelude hiding (lookup)

import Control.Exception (catch, displayException)
import Control.Lens ((^.),(.~),(&))
import Data.Aeson (decode, Value(..), fromJSON)
import Data.Aeson.Lens (key, asText)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Either (fromRight)
import Data.HashMap.Strict (keys, HashMap(..), empty, lookup, fromList)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, fromJust)
import Data.Text (Text, pack, unpack, append)
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..), defaultManagerSettings, managerResponseTimeout, responseTimeoutMicro)
import Network.Wreq
import System.Log.Logger (infoM)

import CoreDataTypes

info :: Text -> IO()
info = infoM "Mattrai" . unpack

getter :: Text -> IO (Response ByteString)
getter = let opts = defaults & manager .~ Left (defaultManagerSettings {managerResponseTimeout = responseTimeoutMicro 1000000})
         in getWith opts . unpack

httpErrorHandler (HttpExceptionRequest _ (ConnectionFailure cf))    = if "does not exist" `isInfixOf` displayException cf
                                                                      then DnsFailure
                                                                      else UnknownFailure
httpErrorHandler (HttpExceptionRequest _ ConnectionTimeout)         = Timeout
httpErrorHandler (HttpExceptionRequest _ (StatusCodeException r _)) = HttpCode $ r ^. responseStatus . statusCode
httpErrorHandler (HttpExceptionRequest _ failure)                   = OtherFailure $ pack (show failure)
httpErrorHandler _                                                  = UnknownFailure2


healthCheckStatus :: Endpoint -> IO [HealthCheckResult]
healthCheckStatus (Endpoint url) = do
  payload <- wrapLog (append "Healthcheck check: " url) $
      catch (Just <$> getter url)
            $ \(_ :: HttpException) -> return Nothing
  let kys = maybe []
                  (filter (/= "status") .
                  keys .
                  (\(Object o) -> o) .
                  fromJust .
                  lookup ("details" :: Text).
                  fromMaybe empty .
                  decode .
                  (^. responseBody))
                     payload
  let getServiceUpStatus ky =
        case payload of
          Just t
            | Just "UP" == decode (t ^. responseBody) ^. key "details" . key ky . key "status" . asText -> Up
          _ -> Down
  return $ map (\t -> HealthCheckResult (HealthCheckItem t) (getServiceUpStatus t)) kys

ping :: Endpoint -> IO PingResult
ping (Endpoint url) =
    wrapLog (append "Ping check " url)
            $ catch (HttpCode . (^. responseStatus . statusCode) <$> getter url) $ return . httpErrorHandler

wrapLog :: Text -> IO a -> IO a
wrapLog description action =
    do info $ append "Start " description
       result <- action
       info $ append "Done " description
       return result
