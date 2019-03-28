module Render (topLevelPage, report)
where

import Prelude hiding (span, id, div, head)
import Data.Text (Text, append, pack)
import Data.Tuple(fst,snd)
import Text.Blaze (toValue, (!), preEscapedText)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as A hiding (span)

import BlazeUtils (addScript, addStyleSheet, anchor, divClass)
import CoreDataTypes
import ResultJson


topLevelPage :: [Text] -> ResultServices -> Html
topLevelPage envKey result =
    htmlFrameWork $ do
      do div ! id "bg" $
           if isLoading result then
              loadingMessage
           else
             statusTable (map toHtml envKey) result

commonHeader :: Html
commonHeader = head $ do
           H.title "Mattrai"
           addStyleSheet "static/style.css"
           meta ! charset "utf-8"
           meta ! name "viewport"
                ! content "width=device-width, initial-scale=1, shrink-to-fit=no"
           addStyleSheet "https://stackpath.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.css"
           addScript "http://code.jquery.com/jquery-1.9.1.js"
           addScript "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.js"
           script "$(function () { $('[data-toggle=\"tooltip\"]').tooltip() })"
           addScript "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js" -- Bootstrap Tooltips
           addScript "/static/eng.js"

isLoading :: ResultServices -> Bool
isLoading (ResultServices []) = True
isLoading (ResultServices _)  = False

loadingMessage :: Html
loadingMessage = h1 "Loading..."

statusHeaderRow :: [Html] -> Html
statusHeaderRow =
     divClass "statusTableRow" .
       mappend (statusHeaderCell "Service") .
               mconcat . map statusHeaderCell

statusHeaderCell :: Html -> Html
statusHeaderCell = divClass "statusTableHead"

statusTable :: [Html] -> ResultServices -> Html
statusTable envKey (ResultServices services) =
             divClass "statusTable" $ do
                statusHeaderRow envKey
                divClass "statusTableBody" $
                  mconcat $ map statusService services

instanceModal :: Text -> ResultInstance -> Html
instanceModal serviceName inst =
  divClass "modal fade instanceModal"
      ! id (toValue $ instanceId inst)
      ! tabindex "-1"
      ! role "dialog" $
    divClass "modal-dialog" $
      divClass "modal-content" $ do
        divClass "modal-header" $ do
          closeButton ! dataAttribute "aria-label" "Close" $
            span ! dataAttribute "aria-hidden" "true" $ preEscapedText ("&times;" :: Text)
          h4 ! class_ "modal-title" $ toHtml $ append serviceName " Service Information"
        divClass "modal-body" $
          instanceInformation inst
        divClass "modal-footer" $
          closeButton "Close"
  where closeButton = button ! type_ "button"
                             ! class_ "close"
                             ! dataAttribute "dismiss" "modal"

optionalSection inst name formatter accessor = if not $ null $ accessor inst then
                                                 do h3 name
                                                    formatter $ accessor inst
                                               else
                                                 mempty

instanceInformation :: ResultInstance -> Html
instanceInformation inst =
    let optional = optionalSection inst
    in do h3 "Service Status"
          statusPingResult $ resultInstancePingResult inst
          h3 "Status Endpoint"
          div . urlToAnchor $ resultInstancePingEndpoint inst
          optional "Documentation" (mapM_ (div . urlToAnchor)) resultInstanceDocumentation
          optional "Logs" (mapM_ (div . urlToAnchor)) resultInstanceLogs
          optional "Healthcheck Status" statusHealthCheckTable resultInstanceHealthCheckResults
          optional "Other Endpoints" (mapM_ (\(name, endpoint) -> miscEndpointEntry name endpoint)) resultInstanceMiscEndpoints
          optional "Information" informationTable information

miscEndpointEntry :: Text -> Endpoint -> Html
miscEndpointEntry name val = div $ do (toHtml (name `mappend` ": "))
                                      urlToAnchor val

informationTable :: [(Text,Text)] -> Html
informationTable entries =
    table $ mconcat $ map informationRow entries
    where informationRow (key,value) = tr $ do td ! A.style "padding: 5px" $ toHtml key
                                               td ! A.style "padding: 5px" $ toHtml value

statusService :: ResultService -> Html
statusService service =
                  divClass "statusTableRow" $ do
                    divClass "statusTableCell serviceHeader" $
                      toHtml $ resServiceName service
                    mconcat $ map (envInst $ resServiceName service) $ resServiceEnvironments service

envInst :: Text -> ResultEnvironment -> Html
envInst serviceName = divClass "statusTableCell" . mconcat . map (statusInstance serviceName) . resultInstances

statusInstance :: Text -> ResultInstance -> Html
statusInstance serviceName inst =
                      do let boxColour :: Text = if pingSuccessful inst then "greenBackground" else "redBackground"
                         instanceModal serviceName inst
                         divClass (toValue (append "instanceBox " boxColour)) $ do
                           info inst
                           docs inst
                           statusHealthChecks $ resultInstanceHealthCheckResults inst

docs = mapM_ (divClass "docsButton" . documentationInstance) . resultInstanceDocumentation

pingSuccessful :: ResultInstance -> Bool
pingSuccessful inst =
      case resultInstancePingResult inst of
                        HttpCode t | t >= 200  && t <300 -> True
                        _                                -> False

info :: ResultInstance -> Html
info inst =
  a ! target "_blank" $
   span ! dataAttribute "toggle" "modal"
        ! dataAttribute "target" (toValue $ append "#" (instanceId inst))
        ! class_ "infoButton glyphicon glyphicon-info-sign" $
     ""

documentationInstance :: Endpoint -> Html
documentationInstance (Endpoint url) = anchor url $
                                         span mempty ! class_ "glyphicon glyphicon-question-sign"

urlToAnchor :: Endpoint -> Html
urlToAnchor (Endpoint url) = anchor url $ text url

statusPingResult :: PingResult -> Html
statusPingResult = span . small . toHtml . show

statusHealthChecks :: [ResultHealthCheck] -> Html
statusHealthChecks = divClass "healthChecks" . mconcat . map statusHealthCheck

statusHealthCheckTable :: [ResultHealthCheck] -> Html
statusHealthCheckTable = mapM_ (\h -> do div . urlToAnchor . ResultJson.healthCheckEndpoint $ h
                                         statusHealthCheckRows $ healthCheckResultItems h)


statusHealthCheckRows :: [HealthCheckResult] -> Html
statusHealthCheckRows = divClass "healthcheckTable" . mapM_ statusHealthCheckRow

statusHealthCheckRow :: HealthCheckResult -> Html
statusHealthCheckRow (HealthCheckResult (HealthCheckItem name) status) =
    divClass "healthcheckRow" $ do
      divClass "healthcheckCell" $ divClass "healthCheckItemName" $ toHtml name
      divClass "healthcheckCell" $ toHtml $ pack $ show status


statusHealthCheck :: ResultHealthCheck -> Html
statusHealthCheck healthCheck = mconcat . map (statusHealthCheckItem $ endpointToString $ ResultJson.healthCheckEndpoint healthCheck) . healthCheckResultItems $ healthCheck

statusHealthCheckItem :: Text -> HealthCheckResult -> Html
statusHealthCheckItem url result = span $ anchor url ! A.title (toValue $ healthCheckItemName $ healthCheckResultItemName result)
                                       $ span mempty
                                         ! class_ (case healthCheckResultItemStatus result of
                                                     Down -> "glyphicon glyphicon-minus-sign red"
                                                     Up   -> "glyphicon glyphicon-ok green")

report :: ResultServices -> Html
report (ResultServices services) = htmlFrameWork $ mapM_ reportService services

htmlFrameWork :: Html -> Html
htmlFrameWork theBody = html $ do commonHeader
                                  body $
                                    do divClass "back" $ mempty
                                       theBody
                                       pageFooter

reportService :: ResultService -> Html
reportService service = do h2 $ toHtml $ resServiceName service
                           mapM_ reportEnvironment (resServiceEnvironments service)

reportEnvironment :: ResultEnvironment -> Html
reportEnvironment env = optionalSection env (toHtml $ resultEnvironmentName env) (mapM_ instanceInformation) resultInstances

pageFooter :: Html
pageFooter = divClass "footer" $ do span $ anchor "https://github.com/caphesuanong/mattrai" "Mattrai"
                                    span $ toHtml (" | " :: Text)
                                    span $ anchor "/report" "report"

