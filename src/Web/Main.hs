{-# LANGUAGE OverloadedStrings #-}
module Main where

import CallOption
import Contract hiding (i)
import Pricing
import Utils
import DataProviders.Csv

import Data.Time
import Web.Scotty hiding (body)
import Network.Wai.Middleware.Static
import CSS
import Data.Aeson (object, (.=), FromJSON(..), decode, eitherDecode, Value (..))
import Data.Text.Lazy (toStrict)
import Prelude hiding (div, head, id, span)
import Text.Blaze.Html5 (Html, a, body, button,
                         dataAttribute, div, docTypeHtml,
                         form, h1, h2, head, input, li,
                         link, meta, p, script, style,
                         title, ul, (!), form, input, label, button, span, i)
import Text.Blaze.Html5.Attributes (charset, class_, content, href,
                                    httpEquiv, id, media, name,
                                    placeholder, rel, src, type_)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Internal (preEscapedText, preEscapedString)
import Text.Blaze (stringValue)

import Data.Monoid (mconcat, mempty)
import Control.Monad.Trans
import Control.Monad (msum)
import qualified Data.Map as M
import Control.Applicative ((<$>), (<*>))
import qualified Data.Text as T
import Data.Typeable
import Types
import GHC.Generics

instance FromJSON OptionData
instance FromJSON Day where
    parseJSON (String s) = return $ parseDate $ T.unpack s

pet = preEscapedText
pes = preEscapedString

blaze :: Html -> ActionM ()
blaze = html . renderHtml

layout :: Html -> Html -> Html
layout t pageBody = docTypeHtml $ do
               head $ do
                 title t
                 meta ! charset "utf-8"
                 meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge,chrome=1"
                 meta ! name "description" ! content "Contract pricing"
                 meta ! name "viewport" ! content "width=device-width"
                 link ! href "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css" ! rel "stylesheet" ! media "screen"
                 link ! href "//cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.3.1/css/datepicker3.min.css" ! rel "stylesheet" ! media "screen"
                 style $ pet $ toStrict layoutCss
                 body $ do
                        script ! src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js" $ mempty
                        script ! src "//cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.3.1/js/bootstrap-datepicker.min.js" $ mempty
                        script ! src "/js/main.js" $ mempty
                        script ! src "//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js" $ mempty
                        pageBody
                        
                        

homeView :: ActionM ()
homeView = blaze $ layout "Main" $ do
             div ! class_ "container" $
                 do
                   h1 "Option pricing"
                   p $ do form ! id "mainForm" $ do
                                     mainForm
                                     a ! class_ "btn btn-lg btn-primary" ! id "run" ! href "#run" $ "Run pricing"
                   div ! class_ "alert alert-success" ! id "result" $ ""
                   div ! class_ "alert alert-danger" ! id "error" $ ""


mainForm :: Html
mainForm = mconcat $ map field formData
    where
      formData = gtoForm (Proxy :: Proxy (Rep OptionData))

field :: (String, TypeRep) -> Html
field (name, tr) | tr == typeOf (undefined :: Double) = numField name name
                 | tr == typeOf (undefined :: Day) = dateField name name

numField :: String -> String -> Html
numField fName fLabel = div ! class_ "form-group" $ do
                              label $ pes fLabel
                              input ! type_ "text" ! class_ "form-control" ! name (stringValue fName) 
                                    ! dataAttribute "datatype" "Double"

dateField :: String -> String -> Html
dateField fName fLabel = div ! class_ "form-group" $ do
                           label $ pes fLabel
                           div ! class_ "input-group date" ! id (stringValue (fName ++ "Picker")) $ 
                               do
                                 input ! type_ "text" ! class_ "form-control" ! name (stringValue fName)
                                 span ! class_ "input-group-addon" $ 
                                      i ! class_ "glyphicon glyphicon-calendar" $ ""


price :: MContract -> OptionData -> IO Double
price contr optData = 
    do
      rawMarketData <- getRawData "UNDERLYING" (startDate optData) (endDate optData)
      [price] <- runPricing pConf [(discModel, modelData, toMarketData rawMarketData)] contr
      return price
    where
      (discModel, modelData) = makeInput optData

main = scotty 3000 $ do
    get "/" homeView
    post "/api/" $ do
      optData <- jsonData :: ActionM OptionData
      res <- liftIO $ price (day2ContrDate $ startDate optData, makeContract optData) optData
      json $ object ["price" .= res]
    middleware $ staticPolicy (addBase "src/Web/static")
