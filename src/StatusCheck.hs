module StatusCheck where

import Control.Exception (catch, displayException)
import Control.Lens ((^.),(^..))
import Data.Aeson (decode, Value)
import Data.Aeson.Lens (key, asText)
import Data.HashMap.Strict (keys, HashMap(..), empty)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..))
import Network.Wreq -- (Response(..))

import CoreDatatypes


httpErrorHandler e =
         case e of
            HttpExceptionRequest _ (ConnectionFailure cf)    -> if "does not exist" `isInfixOf` displayException cf
                                                                then DnsFailure
                                                                else UnknownFailure
            HttpExceptionRequest _ ConnectionTimeout         -> Timeout
            HttpExceptionRequest _ (StatusCodeException r _) -> HttpCode $ r ^. (responseStatus . statusCode)
            _                                                -> UnknownFailure2



healthCheckStatus :: Endpoint -> IO [HealthCheckResult]
healthCheckStatus (Endpoint url) = do
  payload <- catch (Right <$> get url)
             $ return . Left . httpErrorHandler
  let kys :: [Text] = case payload of
                        Right t -> filter (/= "status") . keys . fromMaybe empty $ (decode (t ^. responseBody) :: Maybe (HashMap Text Value))
                        Left _  -> []
  let getServiceUpStatus ky = case payload of
                        Right t | [Just "UP"] == decode (t ^. responseBody) ^.. key ky . key "status" . asText -> Up
                        _                                                                                      -> Down
  return $
    map
      (\t ->
         HealthCheckResult
           {healthCheckResultItemName = HealthCheckItem t, healthCheckResultItemStatus = getServiceUpStatus t})
      kys

ping :: Endpoint -> IO PingResult
ping (Endpoint url) = catch (HttpCode . (^. responseStatus . statusCode) <$> get url) $ return . httpErrorHandler
