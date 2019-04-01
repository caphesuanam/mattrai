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
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client (Manager, HttpException(..), HttpExceptionContent(..), defaultManagerSettings, managerResponseTimeout, responseTimeoutMicro, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings, mkManagerSettings, newTlsManagerWith,mkManagerSettingsContext)
import Network.Wreq
import Network.TLS (ClientParams(ClientParams))
import System.Log.Logger (infoM)

import CoreDataTypes

info :: Text -> IO()
info = infoM "Mattrai" . unpack

getter :: Text -> IO (Response ByteString)
getter = let opts = defaults & manager .~ Left (tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro 1000000
                                                                   {-, settingDisableCertificateValidation = True-}})
                             & manager .~ Left (mkManagerSettingsContext Nothing
                                                                         (TLSSettingsSimple {
                                                                           settingDisableCertificateValidation = True,
                                                                           settingDisableSession=False,
                                                                           settingUseServerName=True})
                                                                         Nothing)
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
  let kys1 = maybe []
                   (filter (/= "status") .
                   keys .
                   (\(Object o) -> o) .
                   fromMaybe (Object empty) .
                   lookup ("details" :: Text).
                   fromMaybe empty .
                   decode .
                   (^. responseBody))
                      payload
  let kys2 = maybe []
                   (filter (\x -> (x /= "details") && (x /= "status")) .
                   keys .
                   (\(Object o) -> o) .
                   fromMaybe (Object empty) .
                   decode .
                   (^. responseBody))
                      payload
  let getServiceUpStatus ky =
        case payload of
          Just t
            | Just "UP" == decode (t ^. responseBody) ^. key "details" . key ky . key "status" . asText -> Up
          _ -> Down
  let getServiceUpStatus2 ky =
        case payload of
          Just t
            | Just "UP" == decode (t ^. responseBody) ^. key ky . key "status" . asText -> Up
          _ -> Down
  let z1 = map (\t -> HealthCheckResult (HealthCheckItem t) (getServiceUpStatus t)) kys1
  let z2 = map (\t -> HealthCheckResult (HealthCheckItem t) (getServiceUpStatus2 t)) kys2
  return $ z1 ++ z2

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
