module Render where

import Data.Text (Text(..))
import Text.Blaze (toValue,(!))
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as A

import CoreDataTypes
import ResultJson

topLevelPage :: [Text] -> ResultServices -> H.Html
topLevelPage envKey result =
    H.html $ do
      H.head $ do
        H.title "The all seeing eye"
        H.style $ toHtml pageStyle
        H.meta ! charset "utf-8"
        H.meta ! name "viewport"
               ! content "width=device-width, initial-scale=1, shrink-to-fit=no"
        H.link ! rel "stylesheet"
               ! type_ "text/css"
               ! href "https://stackpath.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
        H.script mempty ! src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
        H.script "$(function () { $('[data-toggle=\"tooltip\"]').tooltip() })"


        -- Bootstrap Tooltips
        -- https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js
        H.script mempty ! src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js"

      H.body $
        statusTable envKey result

statusHeaderRow :: [Text] -> H.Html
statusHeaderRow =
     (H.div ! class_ "statusTableRow") .
       mappend (statusHeaderCell "Service") .
               mconcat . map statusHeaderCell

statusHeaderCell :: Text -> H.Html
statusHeaderCell = (H.div ! class_ "statusTableCell") . H.toHtml

statusTable :: [Text] -> ResultServices -> H.Html
statusTable envKey (ResultServices services) =
             H.div ! class_ "statusTable" $ do
                statusHeaderRow envKey
                H.div ! class_ "statusTableBody" $
                  mconcat $ map statusService services

pageStyle :: Text
pageStyle =
  ".statusTable {\
  \  display: table; \
  \  width: 100% \
  \} \
  \.statusTableBody{\
  \  display: table-row-group; \
  \} \
  \.statusTableRow {\
  \  display: table-row; \
  \} \
  \.statusTableHeading {\
  \  display: table-header-group \
  \} \
  \.statusTableCell, .statusTableHead {\
  \  border: 1px solid #999999;\
  \  display: table-cell;\
  \  padding: 3px 10px;\
  \  vertical-align: middle;\
  \}"

statusService :: ResultService -> H.Html
statusService service =
                  H.div ! class_ "statusTableRow" $ do
                    H.div ! class_ "statusTableCell" $
                      H.toHtml $ resServiceName service
                    mconcat $ map envInst $ resServiceEnvironments service

envInst :: ResultEnvironment -> H.Html
envInst = (H.div ! class_ "statusTableCell") . mconcat . map statusInstance . resultInstances

statusInstance :: ResultInstance -> H.Html
statusInstance inst = let backgroundColor = if pingSuccessful inst then "LightGreen" else "red"
                      in H.div ! A.style (toValue ("display: inline-block; width: 105px; height: 40px; background-color: " ++ backgroundColor)) $ do
                        H.div ! A.style "display: block; width: 16px; float: right" $
                          documentationInstance $ resultInstanceDocumentation inst
                        statusPingResult $ resultInstancePingResult inst
                        statusHealthChecks $ resultInstanceHealthCheckResults inst

pingSuccessful :: ResultInstance -> Bool
pingSuccessful inst = case resultInstancePingResult inst of
                        HttpCode t | t >= 200  && t <300 -> True
                        _                                -> False

documentationInstance :: Endpoint -> H.Html
documentationInstance (Endpoint url) = H.a ! href (toValue url) $ H.span mempty ! class_ "glyphicon glyphicon-question-sign"

statusPingResult :: PingResult -> H.Html
statusPingResult = H.div . H.toHtml . show

statusHealthChecks :: [ResultHealthCheck] -> H.Html
statusHealthChecks = H.div . mconcat . map statusHealthCheck

statusHealthCheck :: ResultHealthCheck -> H.Html
statusHealthCheck healthCheck = mconcat . map (statusHealthCheckItem (endpointToString $ healthCheckEndpoint healthCheck)) . healthCheckResultItems $ healthCheck

red :: Text
red = "color: rgb(255,0,0)"

green :: Text
green = "color: rgb(68,157,68)"

statusHealthCheckItem :: String -> HealthCheckResult -> H.Html
statusHealthCheckItem url result = H.a ! href (toValue url)
                                       ! A.title (toValue $ healthCheckItemName $ healthCheckResultItemName result)
                                       $ H.span mempty
                                         ! case healthCheckResultItemStatus result of
                                             Down -> class_ "glyphicon glyphicon-minus-sign" `mappend` A.style (toValue red)
                                             Up   -> class_ "glyphicon glyphicon-plus" `mappend` A.style (toValue green)

--statusHealthCheckItem url result = H.span ! class_ "glyphicon glyphicon-minus-sign" $ H.a "?" ! href (toValue url)
--statusHealthCheckItem :: String -> HealthCheckResult -> H.Html
-- statusHealthCheckItem url result =  H.a (H.span "" ! class_ "glyphicon glyphicon-minus-sign") ! href (toValue url)

