module Render where

import           Text.Blaze (toValue,(!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import CoreDatatypes
import ResultJson

topLevelPage :: ResultServices -> H.Html
topLevelPage result =
    H.html $ do
      H.head $ do
        H.title "The all seeing eye"
        H.style  ".statusTable {\
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
                 \} "
                 -- \.glyphicon {
                 -- \   position: relative;
                 -- \   top: 1px;
                 -- \   display: inline-block;
                 -- \   font-family: 'Glyphicons Halflings'
                 -- \   font-style: normal;
                 -- \   font-weight: normal;
                 -- \   line-height: 1;
                 -- \   -webkit-font-smoothing: antialiased;
                 -- \   -moz-osx-font-smoothing: grayscale;
                 -- \ }
                 -- \.glyphicon-minus-sign             { &:before { content: \"\\e082\"; } }
                 -- \.glyphicon-plus                   { &:before { content: \"\\2b\"; } } "
        H.meta ! A.charset "utf-8"
        H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1, shrink-to-fit=no"
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "https://stackpath.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
        H.script mempty ! A.src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
        -- <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"

        -- H.script mempty ! A.src "https://code.jquery.com/jquery-3.3.1.slim.min.js"
        -- H.script mempty ! A.src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.6/umd/popper.min.js"
        -- H.script mempty ! A.src "https://stackpath.bootstrapcdn.com/bootstrap/4.2.1/js/bootstrap.min.js"
        -- <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.2.1/css/bootstrap.min.css" integrity="sha384-GJzZqFGwb1QTTN6wy59ffF1BuGJpLSa9DkKMp0DgiMDm4iYMj70gZWKYbI706tWS" crossorigin="anonymous">


        -- <script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
        -- <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.6/umd/popper.min.js" integrity="sha384-wHAiFfRlMFy6i5SRaxvfOCifBUQy1xHdJ/yoi7FRNXMRBu5WHdZYu1hA6ZOblgut" crossorigin="anonymous"></script>
        -- <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.2.1/js/bootstrap.min.js" integrity="sha384-B0UglyR+jN6CkvvICOB2joaf5I4l3gm9GU6Hc1og6Ls7i6U/mkkaduKaBhlAXv9k" crossorigin="anonymous"></script>
      H.body $
        statusTable result

statusTable :: ResultServices -> H.Html
statusTable (ResultServices services) =
             H.div ! A.class_ "statusTable" $
                H.div ! A.class_ "statusTableBody" $
                  mconcat $ map statusService services


statusService :: ResultService -> H.Html
statusService service =
                  H.div ! A.class_ "statusTableRow" $
                    mconcat $ map statusInstance $ resInstances service

statusInstance :: ResultInstance -> H.Html
statusInstance inst =
                    H.div ! A.class_ "statusTableCell" $ do
                      documentationInstance $ resultInstanceDocumetation inst
                      statusPingResult $ resultInstancePingResult inst
                      statusHealthChecks $ resultInstanceHealthCheckResults inst

documentationInstance :: Endpoint -> H.Html
documentationInstance (Endpoint url) = H.a ( H.span mempty ! A.class_ "glyphicon glyphicon-question-sign") ! A.href (toValue url)

statusPingResult :: PingResult -> H.Html
statusPingResult pingResult = H.p $ H.toHtml $ "Ping: " ++ show pingResult

statusHealthChecks :: [ResultHealthCheck] -> H.Html
statusHealthChecks healthChecks = H.div $ do
 H.p "HealthChecks"
 mconcat . map statusHealthCheck $ healthChecks

statusHealthCheck :: ResultHealthCheck -> H.Html
statusHealthCheck healthCheck = mconcat . map (statusHealthCheckItem (endpointToString $ healthCheckEndpoint healthCheck)) . healthCheckResultItems $ healthCheck

statusHealthCheckItem :: String -> HealthCheckResult -> H.Html
statusHealthCheckItem url result = H.a (H.span mempty
                                    ! case healthCheckResultItemStatus result of
                                                    Down -> A.class_ "glyphicon glyphicon-minus-sign" `mappend` A.style "color: rgb(255,0,0)"
                                                    Up   -> A.class_ "glyphicon glyphicon-plus" `mappend` A.style "color: rgb(68,157,68)")
                                       ! A.href (toValue url)


--statusHealthCheckItem url result = H.span ! A.class_ "glyphicon glyphicon-minus-sign" $ H.a "?" ! A.href (toValue url)
--statusHealthCheckItem :: String -> HealthCheckResult -> H.Html
-- statusHealthCheckItem url result =  H.a (H.span "" ! A.class_ "glyphicon glyphicon-minus-sign") ! A.href (toValue url)

