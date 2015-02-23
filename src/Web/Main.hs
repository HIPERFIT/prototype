{-# LANGUAGE OverloadedStrings #-}
module Main where

import CallOption
import Contract hiding (i)
import Pricing
import Utils
import DataProviders.Csv
import DataProviders.Common
import DataProviders.Data

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
                         title, ul, (!), form, input, label, 
                         option, button, span, i, select, 
                         table, tr, td, th)
import Text.Blaze.Html5.Attributes (charset, class_, content, href,
                                    httpEquiv, id, media, name,
                                    placeholder, rel, src, type_)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Internal (preEscapedText, string)
import Text.Blaze (stringValue)

import Data.Monoid (mconcat, mempty)
import Control.Monad.Trans
import Control.Monad (forM_)
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

blaze :: Html -> ActionM ()
blaze = html . renderHtml

layout :: Html -> Html -> Html
layout t pageContent = docTypeHtml $ do
               head $ do
                 title t
                 meta ! charset "utf-8"
                 meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge,chrome=1"
                 meta ! name "description" ! content "Contract pricing"
                 meta ! name "viewport" ! content "width=device-width"
                 link ! href "//cdnjs.cloudflare.com/ajax/libs/bootstrap-select/1.6.3/css/bootstrap-select.min.css" ! rel "stylesheet" ! media "screen"
                 link ! href "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css" ! rel "stylesheet" ! media "screen"
                 link ! href "//cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.3.1/css/datepicker3.min.css" ! rel "stylesheet" ! media "screen"
                 style $ pet $ toStrict layoutCss
                 body $ do
                        script ! src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js" $ mempty
                        script ! src "//cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.3.1/js/bootstrap-datepicker.min.js" $ mempty
                        script ! src "/js/main.js" $ mempty
                        script ! src "//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js" $ mempty
                        script ! src "//cdnjs.cloudflare.com/ajax/libs/bootstrap-select/1.6.3/js/bootstrap-select.min.js" $ mempty
                        div ! class_ "container" $ do
                          navBar
                          pageContent

homeView :: ActionM ()
homeView = blaze $ layout "Main" $ do
             div ! class_ "row" $ do
               div ! class_ "col-xs-4" $ leftPanel
               div ! class_ "col-xs-8" $ rightPanel

leftPanel = ""

rightPanel = do
  div ! class_ "right-container" $
      do
        h1 "Option pricing"
        p $ do form ! id "mainForm" $ do
                 mainForm
                 a ! class_ "btn btn-lg btn-primary" ! id "run" ! href "#run" $ "Run pricing"
                 div ! class_ "alert alert-success" ! id "result" $ ""
                 div ! class_ "alert alert-danger" ! id "error" $ ""

navBar :: Html
navBar = div ! class_ "navbar navbar-default" $ do
           div ! class_ "navbar-header" $ do
             button ! type_ "button"
                    ! class_ "navbar-toggle" ! dataAttribute "toggle" "collapse" 
                    ! dataAttribute "target" ".navbar-collapse" $ do
                                            a ! class_ "navbar-brand" ! href "#" $ "λ"
             div ! class_ "navbar-collapse collapse" $ ul ! class_ "nav navbar-nav" $ 
                 do
                   li ! class_ "active" $ a ! href "#" $ "Home"
                   li $ a ! href "/" $ "Pricing"
                   li $ a ! href "/marketData/view/" $ "Market Data"

mainForm :: Html
mainForm = mconcat $ map field formData
    where
      formData = gtoForm (Proxy :: Proxy (Rep OptionData))

marketDataView :: [RawQuotes] -> ActionM ()
marketDataView quotes = blaze $ layout "Market data" $ do
                   table ! class_ "table table-stripped" $ do
                                                         headerRow
                                                         forM_ quotes toRow 
    where
      headerRow = tr $ do
                th "Underlying"
                th "Date"
                th "Price"
      toRow (und, date, price) = tr $ do
                td $ string und
                td $ string $ formatDate date
                td $ string $ show price

field :: (String, TypeRep) -> Html
field (name, tr) | tr == typeOf (undefined :: Double) = numField name name
                 | tr == typeOf (undefined :: Day) = dateField name name
                 | tr == typeOf (undefined :: Underlying) = selectField name name

numField :: String -> String -> Html
numField fName fLabel = div ! class_ "form-group" $ do
                              label $ string fLabel
                              input ! type_ "text" ! class_ "form-control" ! name (stringValue fName) 
                                    ! dataAttribute "datatype" "Double"

dateField :: String -> String -> Html
dateField fName fLabel = div ! class_ "form-group" $ do
                           label $ string fLabel
                           div ! class_ "input-group date" ! id (stringValue (fName ++ "Picker")) $ 
                               do
                                 input ! type_ "text" ! class_ "form-control" ! name (stringValue fName)
                                 span ! class_ "input-group-addon" $ 
                                      i ! class_ "glyphicon glyphicon-calendar" $ ""

selectField :: String -> String -> Html
selectField fName fLabel = div ! class_ "form-group" $ do
                                  label $ string fLabel
                                  select ! class_ "form-control selectpicker" ! name (stringValue fName)
                                         ! dataAttribute "datatype" "Underlying" $ ""

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
    get "/marketData/underlyings/" $ do
                       availUnd <- liftIO availableUnderlyings
                       json availUnd
    get "/marketData/view/" $ do 
                       quotes <- liftIO getStoredQuotes
                       marketDataView quotes
    post "/api/" $ do
      optData <- jsonData :: ActionM OptionData
      res <- liftIO $ price (day2ContrDate $ startDate optData, makeContract optData) optData
      json $ object ["price" .= res]
    middleware $ staticPolicy (addBase "src/Web/static")
