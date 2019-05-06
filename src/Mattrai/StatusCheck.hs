{-# LANGUAGE RankNTypes #-}
module Mattrai.StatusCheck where

import Prelude hiding (lookup)

import Control.Exception (catch, displayException)
import Control.Lens ((^.),(.~),(&),(^?),(^..),Getter)
import Control.Lens.Prism (_Just)
import Control.Lens.Getter (Getting)
import Data.Aeson (decode, Value(..), fromJSON)
import Data.Aeson.Lens (key, _String, _Object, _Array)
import Data.Monoid (First)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Either (fromRight)
import Data.HashMap.Strict as M (keys, HashMap(..), empty, lookup, fromList)
import Data.List (isInfixOf)
import Data.Maybe (maybe, fromMaybe, fromJust)
import Data.Text (Text, pack, unpack, append)
import qualified Data.Vector as V (toList, empty)
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client (Manager, HttpException(..), HttpExceptionContent(..), defaultManagerSettings, managerResponseTimeout, responseTimeoutMicro, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings, mkManagerSettings, newTlsManagerWith,mkManagerSettingsContext)
import Network.Wreq
import Network.TLS (ClientParams(ClientParams))
import System.Log.Logger (infoM)

import Mattrai.Endpoint
import Mattrai.Service
import Mattrai.Result

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

getKeys blackList locator payload =
     maybe []
           (filter (not . (`elem` blackList)) . keys)
           $ payload ^? _Just . responseBody . locator . _Object

getServiceUpStatus locator payload ky =
  if Just "UP" == (payload ^? _Just . responseBody . locator . key ky . key "status" . _String) then
    Up
  else
    Down

healthCheckStatus :: Endpoint -> IO [HealthCheckResult]
healthCheckStatus (Endpoint url) = do
  payload <- wrapLog (append "Healthcheck check: " url) $
      catch (Just <$> getter url)
            $ \(_ :: HttpException) -> return Nothing

  let kys1 = getKeys ["status"] (key "details") payload
  let kys2 = getKeys ["details", "status"] id payload
  let getServiceUpStatus1 = getServiceUpStatus (key "details") payload
  let getServiceUpStatus2 = getServiceUpStatus id payload
  let z1 = map (\t -> HealthCheckResult (HealthCheckItem t) (getServiceUpStatus1 t)) kys1
  let z2 = map (\t -> HealthCheckResult (HealthCheckItem t) (getServiceUpStatus2 t)) kys2
  return $ z1 ++ z2

getDynamicInformation :: Endpoint -> Getting (First Text) (Response ByteString) Text -> IO (Maybe Text)
getDynamicInformation (Endpoint url) accessor = (^? accessor) <$> getter url

getDynamicInformation' :: Endpoint -> IO (Maybe Text)
getDynamicInformation' (Endpoint url) = (^? accessor) <$> getter url
                                         where accessor :: Getting (First Text) (Response ByteString) Text = responseBody . key "path" . key "to" . _String

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
