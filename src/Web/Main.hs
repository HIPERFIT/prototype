{-# LANGUAGE OverloadedStrings #-}
module Main where

import Contract hiding (i)
import Contract.Date
import Pricing
import CodeGen.DataGen hiding (startPrice, startDate)

import Web.Scotty hiding (body)
import CSS
import Data.Aeson (object, (.=))
import Data.Text.Lazy (toStrict)
import Prelude hiding (div, head, id, span)
--import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html5 (Html, a, body, button,
                         dataAttribute, div, docTypeHtml,
                         form, h1, h2, head, input, li,
                         link, meta, p, script, style,
                         title, ul, (!), form, input, label, button, span, i)
import Text.Blaze.Html5.Attributes (charset, class_, content, href,
                                    httpEquiv, id, media, name,
                                    placeholder, rel, src, type_)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Internal (preEscapedText)

import Data.Monoid (mconcat, mempty)
import qualified Data.Map as M
import Control.Monad.Trans
pet = preEscapedText

blaze :: Html -> ActionM ()
blaze = html . renderHtml

layout :: Html -> Html -> Html
layout t b = docTypeHtml $ do
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
--                        script ! src "//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js" $ mempty
                        script ! type_ "text/javascript" $ ajaxFn
                        b
                        
                        

homeView :: ActionM ()
homeView = blaze $ layout "Main" $ do
             div ! class_ "container" $
                 do
                   h1 "Option pricing"
                   p $ do form ! id "mainForm" $ do
                                     mainForm
                                     a ! class_ "btn btn-lg btn-primary" ! id "run" ! href "#run" $ "Run pricing"
                   div ! class_ "alert alert-success" ! id "result" $ ""

mainForm :: Html
mainForm = do
  div ! class_ "form-group" $ do
             label "Initial price"
             input ! type_ "text" ! class_ "form-control" ! name "initPrice"
  div ! class_ "form-group" $ do
             label "Strike price"
             input ! type_ "text" ! class_ "form-control" ! name "strike"
  div ! class_ "form-group" $ do
             label "Risk-free interest rate"
             input ! type_ "text" ! class_ "form-control" ! name "rate"
  div ! class_ "form-group" $ do
             label "Volatility"
             input ! type_ "text" ! class_ "form-control" ! name "vol"
  div ! class_ "form-group" $ do
             label "Start date"
             div ! class_ "input-group date" ! id "startDatePicker" $ 
                 do
                   input ! type_ "text" ! class_ "form-control" ! name "startDate"
                   span ! class_ "input-group-addon" $ 
                        i ! class_ "glyphicon glyphicon-calendar" $ ""
  div ! class_ "form-group" $ do
             label "End date"
             div ! class_ "input-group date" ! id "endDatePicker" $ 
                 do
                   input ! type_ "text" ! class_ "form-control" ! name "endDate"
                   span ! class_ "input-group-addon" $ 
                        i ! class_ "glyphicon glyphicon-calendar" $ ""
  
  script ! type_ "text/javascript" $ 
         arrayToObjFn >> "$(function(){$('#startDatePicker, #endDatePicker').datepicker({autoclose: true,todayHighlight:true, format: 'yyyy-mm-dd'});});" 

arrayToObjFn = "function arrayToObj(xs) {var res = {}; $.each(xs, function(i,x){res[x.name]=x.value}); return res}\n"

ajaxFn = 
    "$(document).ready(function() {$('#run').click(function(){var data = arrayToObj($('#mainForm').serializeArray()); $.post('/api/', JSON.stringify(data)).done(function(resp){$('#result').html(resp.price); $('#result').show();})})})"

main = scotty 3000 $ do
    get "/" homeView
    post "/api/" $ do
      p <- jsonData :: ActionM (M.Map String String)
      let optData = json2OptionData p
          strk = strike optData
          maturity = dateDiff (startDate optData) (endDate optData)
      res <- liftIO $ price (startDate optData, makeContract strk maturity) optData
      json $ object ["price" .= res]
      --json p

data OptionData = OD {
      startPrice :: Double
    , strike     :: Double
    , rate       :: Double
    , vol        :: Double
    , startDate  :: Date
    , endDate    :: Date
}

pConf = DataConf {monteCarloIter = 4000000}

price :: MContract -> OptionData -> IO Double
price contr optData= 
    do
      [price] <- runPricing pConf inputData contr
      return price
    where
      inputData = makeInput optData

makeInput :: OptionData -> [(DiscModel, ModelData, MarketData)]
makeInput od = [(ConstDisc (rate od),
                 [BS "UNDERLYING" [(startDate od, vol od, drift)]],
                 ([], [Quotes "UNDERLYING" [(startDate od, startPrice od)]]))
               ]
    where
      drift = ((rate od) - ((vol od)^2)/2) * years
      years = (fromIntegral (dateDiff (startDate od) (endDate od)) / 365)

makeContract strike maturity = 
    let
        theobs = obs ("UNDERLYING", 0)
    in transl maturity (scale (maxx (theobs - r strike) 0) (transfOne EUR "you" "me"))

json2OptionData :: (M.Map String String) -> OptionData
json2OptionData js = OD {
                       startPrice = read $ js M.! "initPrice"
                     , strike     = read $ js M.! "strike"
                     , rate       = read $ js M.! "rate"
                     , vol        = read $ js M.! "vol"
                     , startDate  = read $ js M.! "startDate"
                     , endDate    = read $ js M.! "endDate"
                     }
