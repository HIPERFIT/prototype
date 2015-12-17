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
import qualified Fields as F

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
                         thead, fieldset, legend, AttributeValue,
                         pre, canvas)
import Text.Blaze.Html5.Attributes (charset, class_, content, href,
                                    httpEquiv, id, media, name,
                                    placeholder, rel, src, type_, 
                                    value, colspan)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Internal (preEscapedText, string, text)
import Text.Blaze (stringValue)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Data
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty, mconcat)
import GHC.Generics (Rep, Generic)
import Control.Monad (forM_)
import qualified Data.Map as M


menuItems = [instrumentsMenuItem, myPortfolioMenuItem, marketDataMenuItem, modelDataMenuItem, contractGraphMenuItem]
instrumentsMenuItem = ("Instruments", "/")
myPortfolioMenuItem = ("My Portfolio", "/portfolio/")
marketDataMenuItem = ("Market Data", "/marketData/view/")
modelDataMenuItem = ("Model Data", "/modelData/")
contractGraphMenuItem = ("Contract Graph", "/contractGraph/")
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
                        script ! src "/js/chart.js" $ mempty
                        script ! src "/js/main.js" $ mempty
                        script ! src "//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js" $ mempty
                        script ! src "//cdnjs.cloudflare.com/ajax/libs/bootstrap-select/1.6.3/js/bootstrap-select.min.js" $ mempty
                        div ! class_ "container" $ do
                          navBar activeMenuItem
                          div ! class_ "alert alert-success" ! id "result" $ ""
                          div ! class_ "alert alert-danger" ! id "error" $ ""
                          pageContent

contractView :: [ContractGUIRepr] -> ContractGUIRepr -> T.Text -> ActionM ()
contractView allContracts currentContract code =
  blaze $ layout "Financial contracts" Nothing $ do
    div ! class_ "row" $ do
      div ! class_ "col-sm-4" $ leftPanel allContracts
      div ! class_ "col-sm-8" $ rightPanel currentContract code

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

rightPanel dataDescr code = do
  div ! class_ "right-container" $
      do
        h2 $ string $ guiLabel dataDescr
        buildForm dataDescr
        div ! class_ "form-buttons" $ do
                              a ! class_ "btn btn-lg btn-primary" ! id "add" ! href "#" $ "Add to portfolio"
                              a ! class_ "btn btn-lg btn-default" ! dataAttribute "toggle" "collapse" ! href "#code-view" $ "View code"
  div ! class_ "collapse" ! id "code-view" $ pre $ text code

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
      formData = formFields dataDescr
      commonFields = gtoForm (Proxy :: Proxy (Rep CommonContractData))

marketDataView :: [RawQuotes] -> [RawCorr] -> ActionM ()
marketDataView quotes corrs = blaze $ layout "Market Data" (Just (snd marketDataMenuItem)) $ do
                 div ! id "data-tabs" $ do
                   ul ! class_ "nav nav-tabs" $ do
                     li ! class_ "active" $ a ! class_ "tab-link" ! dataAttribute "toggle" "tab" ! href "#quotes" $ "Quotes"
                     li  $ a ! class_ "tab-link" ! dataAttribute "toggle" "tab" ! href "#corrs" $ "Correlations"
                     li  $ a ! class_ "tab-link" ! dataAttribute "toggle" "tab" ! href "#stockgraphs" $ "Stock Graphs"
                   div ! class_ "tab-content" $ do
                     div! id "quotes" ! class_ "tab-pane fade in active" $
                        quotesTable quotes
                     div ! id "corrs" ! class_ "tab-pane fade" $
                       corrsTable corrs
                     div ! id "stockgraphs" ! class_ "tab-pane fade" $ do
                       stockgraphsPage
                       div ! id "stocklegend" $ ""
                       canvas ! id "stockChart" $ ""



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



stockgraphsPage = buildTable (buildThead headerRow, buildTbody $ [(fields ++ [addBtn])])
    where
      fields = map field $ gtoForm (Proxy :: Proxy (Rep StockGraphForm))
      addBtn = a ! class_ "btn btn-lg btn-primary" ! id "stockgraph-btn" ! href "#stockgraph" $ "Show"
      headerRow = ["Underlying 1", "Underlying 2", "Starting date", "End date", "Normalize?", ""]


contractgraphsPage = buildTable (buildThead headerRow, buildTbody $ [(fields ++ [addBtn])])
    where
      fields = map field $ gtoForm (Proxy :: Proxy (Rep ContractGraphForm))
      addBtn = a ! class_ "btn btn-lg btn-primary" ! id "contractgraph-btn" ! href "#contractgraph" $ "Show"
      headerRow = ["Underlying", "Starting date", "End date", "InterestRate", "Interations", ""]



portfolioView portfolio defaults = blaze $ layout "My Portfolio" (Just (snd myPortfolioMenuItem)) $ do
                            div ! class_ "row" $ do
                                div ! class_ "col-sm-8" $ buildTable (buildThead headerRow, (pfTableBody $ (map pItemRow portfolio)) >> totalRow)
                                div ! class_ "col-sm-4" $ do
                                                   pricingForm $ M.fromList defaults
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
      totalRow = tr $ row ["Total", "", "", "", h4 $ span ! class_ "total-output label label-success" $ "", ""]
      pfTableBody rows = mconcat $ map mkTr (zip3 [0 ..] (map row rows) portfolio)
      mkTr (count, r, (_,p,_)) = do
        tr ! class_ "pfitem-row"
           ! dataAttribute "toggle" "collapse" 
           ! dataAttribute "target" (stringValue ("#pfitem-details-" ++ show count))
           ! id (stringValue ("pfitem-" ++ show count)) $ r
        tr ! class_ "collapse"! id (stringValue ("pfitem-details-" ++ show count)) $ td ! colspan "6" $ text $ pFItemContractSpec p
      row xs = mconcat $ map (td ! class_ "vert-align fixed-height") xs

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


contractGraphView = blaze $ layout "Contract Graph" (Just (snd contractGraphMenuItem)) $ do
    contractgraphsPage
    canvas ! id "contractChart" $ ""




buildTable (tblHead, tblBody) = table ! class_ "table table-striped" $ do
                                  tblHead
                                  tblBody

buildThead hs = tr $ mconcat $ map th hs

buildTbody rows = mconcat $ map tr $ map row rows
    where
      row xs = mconcat $ map (td ! class_ "vert-align fixed-height") xs

dataDelLink key url = a ! dataAttribute "key" (stringValue $ BL.unpack key)  
                        ! href url ! class_ "del-item" $ i ! class_ "glyphicon glyphicon-trash" $ ""

pricingForm defaults = mconcat $ map f $ gtoForm (Proxy :: Proxy (Rep PricingForm))
    where
      f item@(name, typeRep) = labeledFieldWithDefault (defaults M.! name) item

labeledFieldWithDefault :: Maybe String -> (String, FormField Html) -> Html
labeledFieldWithDefault defaultVal (name, tr)  = div ! class_ "form-group" $ do
                                                   label $ string $ capFirst name
                                                   fieldWithDefault defaultVal (name, tr)
fieldWithDefault = F.renderField

labeledField = labeledFieldWithDefault Nothing
              
field = fieldWithDefault Nothing
