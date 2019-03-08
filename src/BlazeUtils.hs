module BlazeUtils where

import Data.Text (Text)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

addScript :: AttributeValue -> Html
addScript name = script mempty ! src name

addStyleSheet :: AttributeValue -> Html
addStyleSheet name = link ! rel "stylesheet"
                          ! type_ "text/css"
                          ! href name

anchor :: Text -> Html -> Html
anchor url = a ! href (toValue url)

