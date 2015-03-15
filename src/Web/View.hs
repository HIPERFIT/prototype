{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module View where

import DataProviders.Data
import Utils
import Data
import TypeClass
import CodeGen.DataGen (ppDouble)
import PersistentData
import qualified DB

import Data.Time
import Web.Scotty hiding (body, params, text)
import CSS
import Data.Aeson (object, (.=), FromJSON(..), decode, eitherDecode, Value (..), encode)
import Data.Text.Lazy (toStrict)
import Prelude hiding (div, head, id, span)
import Text.Blaze.Html5 (Html, a, body, button,
                         dataAttribute, div, docTypeHtml,
                         form, h1, h2, h4, head, input, li,
                         link, meta, p, script, style,
                         title, ul, (!), form, input, label, 
                         option, button, span, i, select, 
                         table, tr, td, th, stringValue, tbody, 
                         thead, fieldset, legend, AttributeValue)
import Text.Blaze.Html5.Attributes (charset, class_, content, href,
                                    httpEquiv, id, media, name,
                                    placeholder, rel, src, type_)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Internal (preEscapedText, string, text)
import Text.Blaze (stringValue)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Data
import Data.Monoid (mempty, mconcat)
import GHC.Generics (Rep, Generic)
import Control.Monad (forM_)


menuItems = [instrumentsMenuItem, myPortfolioMenuItem, marketDataMenuItem, modelDataMenuItem]
instrumentsMenuItem = ("Instruments", "/")
myPortfolioMenuItem = ("My Portfolio", "/portfolio/")
marketDataMenuItem = ("Market Data", "/marketData/view/")
modelDataMenuItem = ("Model Data", "/modelData/")
contractsBaseUrl = "/contracts/"

instance FromJSON Day where
    parseJSON (String s) = return $ parseDate $ T.unpack s

pet = preEscapedText

blaze :: Html -> ActionM ()
blaze = html . renderHtml

layout :: Html  -> Maybe String -> Html -> Html
layout t activeMenuItem pageContent = docTypeHtml $ do
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
                        script ! src "//cdnjs.cloudflare.com/ajax/libs/spin.js/2.0.1/spin.min.js" $ mempty
                        script ! src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js" $ mempty
                        script ! src "//cdnjs.cloudflare.com/ajax/libs/spin.js/2.0.1/jquery.spin.min.js" $ mempty
                        script ! src "//cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.3.1/js/bootstrap-datepicker.min.js" $ mempty
                        script ! src "/js/main.js" $ mempty
                        script ! src "//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js" $ mempty
                        script ! src "//cdnjs.cloudflare.com/ajax/libs/bootstrap-select/1.6.3/js/bootstrap-select.min.js" $ mempty
                        div ! class_ "container" $ do
                          navBar activeMenuItem
                          div ! class_ "alert alert-success" ! id "result" $ ""
                          div ! class_ "alert alert-danger" ! id "error" $ ""
                          pageContent

contractView :: [ContractGUIRepr] -> ContractGUIRepr -> ActionM ()
contractView allContracts currentContract =
  blaze $ layout "Financial contracts" Nothing $ do
                                              div ! class_ "row" $ do
                                                div ! class_ "col-sm-4" $ leftPanel allContracts
                                                div ! class_ "col-sm-8" $ rightPanel currentContract

homeView :: [ContractGUIRepr] -> ActionM ()
homeView allContracts = blaze $ layout "Financial contracts" (Just (snd instrumentsMenuItem)) $ do
                          div ! class_ "row" $ do
                            div ! class_ "col-sm-4" $ leftPanel allContracts
                            div ! class_ "col-sm-8" $ div ! class_ "jumbotron" $ do
                                                                 h1 "Welcome!"
                                                                 p "To financial contract pricing prototype."

leftPanel cs = ul ! class_ "list-group" $ forM_ labelsAndUrls toHrefList
    where
      labelsAndUrls = map (\c -> (string $ guiLabel c, stringValue $ contractsBaseUrl ++ url c)) cs
      toHrefList (label, url)  = li ! class_ "list-group-item" $ a ! href url $ span label

rightPanel dataDescr = do
  div ! class_ "right-container" $
      do
        h2 $ string $ guiLabel dataDescr
        buildForm dataDescr
        a ! class_ "btn btn-lg btn-primary" ! id "add" ! href "#" $ "Add to portfolio"

navBar :: Maybe String -> Html
navBar activeMenuItem = div ! class_ "navbar navbar-default" $ do
           div ! class_ "navbar-header" $ do
             button ! type_ "button"
                    ! class_ "navbar-toggle" ! dataAttribute "toggle" "collapse" 
                    ! dataAttribute "target" ".navbar-collapse" $ do
                                            a ! class_ "navbar-brand" ! href "#" $ "λ"
             div ! class_ "navbar-collapse collapse" $ ul ! class_ "nav navbar-nav" $
                 mconcat $ map (buildMenuItem activeMenuItem) menuItems
           h4 ! class_ "navbar-text navbar-right user-info" $ span ! class_ "label label-default" $ "User: hiperfit"

buildMenuItem activeItem (label, url) = 
    case activeItem of
      Just activeUrl | activeUrl == url -> li ! class_ "active" $ link
                     | otherwise -> li $ link
      Nothing -> li $ link
    where
      link = a ! href (stringValue url) $ string label

buildForm dataDescr = form ! id "mainForm" ! dataAttribute "url" (stringValue $ url dataDescr) $ do
                        fieldset ! class_ "common-data" $ mconcat $ map labeledField commonFields
                        fieldset ! class_ "contract-data" $ mconcat $ map labeledField formData
    where
      formData = params dataDescr
      commonFields = gtoForm (Proxy :: Proxy (Rep CommonContractData))

marketDataView :: [RawQuotes] -> [RawCorr] -> ActionM ()
marketDataView quotes corrs = blaze $ layout "Market Data" (Just (snd marketDataMenuItem)) $ do
                 div ! id "data-tabs" $ do
                   ul ! class_ "nav nav-tabs" $ do
                     li ! class_ "active" $ a ! class_ "tab-link" ! dataAttribute "toggle" "tab" ! href "#quotes" $ "Quotes"
                     li  $ a ! class_ "tab-link" ! dataAttribute "toggle" "tab" ! href "#corrs" $ "Correlations"
                   div ! class_ "tab-content" $ do
                     div! id "quotes" ! class_ "tab-pane fade in active" $ 
                        quotesTable quotes
                     div ! id "corrs" ! class_ "tab-pane fade" $ 
                       corrsTable corrs
      

quotesTable quotes = buildTable (buildThead headerRow, buildTbody $ (fields ++ [addBtn]) : map toRow quotes)
    where
      headerRow = ["Underlying", "Date", "Price", ""]
      toRow (und, d, vol) = [ string und, string $ formatDate d, string $ ppDouble 3 vol
                            , dataDelLink (encode (und, formatDate d)) "/marketData/quotes/"]
      fields = map field $ gtoForm (Proxy :: Proxy (Rep DataForm))
      addBtn = a ! class_ "btn btn-lg btn-primary" ! id "add-data" ! href "/marketData/quotes/" $ "Add"

corrsTable corrs = buildTable (buildThead headerRow, buildTbody $ (fields ++ [addBtn]) : map toRow corrs)
    where
      headerRow = ["Underlying1", "Underlying2", "Date", "Correlation", ""]
      toRow (und1, und2, d, vol) = [ string und1, string und2, string $ formatDate d, string $ ppDouble 3 vol
                                   , dataDelLink (encode (und1, und2, formatDate d)) "/marketData/corrs/"]
      fields = map field $ gtoForm (Proxy :: Proxy (Rep CorrForm))
      addBtn = a ! class_ "btn btn-lg btn-primary" ! id "add-data-corrs" ! href "/marketData/corrs/" $ "Add"

portfolioView portfolio = blaze $ layout "My Portfolio" (Just (snd myPortfolioMenuItem)) $ do
                            div ! class_ "row" $ do
                                div ! class_ "col-sm-8" $ buildTable (buildThead headerRow, buildTbody $ (map pItemRow portfolio) ++ [totalRow])
                                div ! class_ "col-sm-4" $ do
                                                   pricingForm
                                                   a ! class_ "btn btn-lg btn-primary" ! id "run" ! href "#run" $ "Run valuation"
                                                   div ! class_ "alert alert-warning" ! id "pricing-form-alert" $ ""
    where
      headerRow = ["Nominal", "Contract", "Start", "Horizon", "Value", ""]
      pItemRow (k, p, hz) = [ string $ show $ pFItemNominal p
                            , text $ pFItemContractType p
                            , string $ formatDate $ pFItemStartDate p
                            , string $ formatDate $ hz
                            , h4 $ do
                                span ! class_ "price-output label label-info" $ ""
                                span ! class_ "processing-label label label-warning" $ "Not valuated"
                                span ! class_ "spinner" $ ""
                            , a ! dataAttribute "id" (stringValue k) ! href "#" ! class_ "del-pfitem" $
                              i ! class_ "glyphicon glyphicon-trash" $ "" ]
      totalRow = ["Total", "", "", "", h4 $ span ! class_ "total-output label label-success" $ "", ""]

modelDataView md = do
  blaze $ layout "Model Data" (Just (snd modelDataMenuItem)) $
        buildTable (buildThead headerRow, buildTbody $ (fields ++ [addBtn]) : map toRow md)
   where
     headerRow = ["Underlying", "Date", "Volatility", ""]
     toRow (und, d, vol) = [ string und, string $ formatDate d, string $ ppDouble 3 vol
                           , dataDelLink (encode (und, formatDate d)) "/modelData/"]
     fields = map field $ gtoForm (Proxy :: Proxy (Rep DataForm))
     addBtn = a ! class_ "btn btn-lg btn-primary" ! id "add-data" ! href "/modelData/" $ "Add"
     delLink und d = a ! dataAttribute "und" (stringValue und) ! dataAttribute "date" (stringValue (formatDate d))
                       ! href "/modelData/" ! class_ "del-item" $ i ! class_ "glyphicon glyphicon-trash" $ ""
          
buildTable (tblHead, tblBody) = table ! class_ "table table-striped" $ do
                                  tblHead
                                  tblBody

buildThead hs = tr $ mconcat $ map th hs

buildTbody rows = mconcat $ map tr $ map row rows
    where
      row xs = mconcat $ map (td ! class_ "vert-align fixed-height") xs

dataDelLink key url = a ! dataAttribute "key" (stringValue $ BL.unpack key)  
                        ! href url ! class_ "del-item" $ i ! class_ "glyphicon glyphicon-trash" $ ""

pricingForm = mconcat $ map labeledField $ gtoForm (Proxy :: Proxy (Rep PricingForm))

labeledField :: (String, TypeRep) -> Html
labeledField (name, tr) | tr == typeOf (undefined :: Bool) = div ! class_ "checkbox" $ label $ do
                                                               boolField name
                                                               string $ capFirst name
                        | otherwise = div ! class_ "form-group" $ do
                                        label $ string $ capFirst name
                                        field (name, tr)

field :: (String, TypeRep) -> Html
field (name, tr) | tr == typeOf (undefined :: Double) ||  tr == typeOf (undefined :: Int)  = numField name
                 -- possibly, we should add support for Maybe for other type of fields
                 | tr == typeOf (undefined :: Day) || tr == typeOf (undefined :: Maybe Day)  = dateField name 
                 | tr == typeOf (undefined :: Underlying) = selectField name
                 | tr == typeOf (undefined :: T.Text)     = textField name
                 | tr == typeOf (undefined :: PercentField) = percentField name
                 | tr == typeOf (undefined :: Bool) = boolField name

numField :: String -> Html
numField fName  = input ! type_ "text" ! class_ "form-control" ! name (stringValue fName) 
                              ! dataAttribute "datatype" "Double"

dateField :: String -> Html
dateField fName = div ! class_ "input-group date" ! id (stringValue (fName ++ "Picker")) $ 
                         do
                           input ! type_ "text" ! class_ "form-control" ! name (stringValue fName)
                           span ! class_ "input-group-addon" $ 
                                i ! class_ "glyphicon glyphicon-calendar" $ ""

selectField :: String -> Html
selectField fName = select ! class_ "form-control selectpicker" ! name (stringValue fName)
                                  ! dataAttribute "datatype" "Underlying" $ ""

percentField :: String -> Html
percentField fName = do
  div ! class_ "input-group" $ do
                       input ! type_ "text" ! class_ "form-control" ! name (stringValue fName) ! dataAttribute "datatype" "PercentField"
                       span ! class_ "input-group-addon" $ "%"

textField :: String -> Html
textField fName = input ! type_ "text" ! class_ "form-control" ! name (stringValue fName)

boolField :: String -> Html
boolField fName = input ! type_ "checkbox" ! name (stringValue fName) ! dataAttribute "datatype" "Bool"
