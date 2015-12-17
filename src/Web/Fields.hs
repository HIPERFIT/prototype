{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Fields where

import Prelude hiding (div, id, span)
import Data.Proxy
import Data.Time
import Data.Text (Text)
import Data.Maybe
import Text.Blaze.Html5 (Html, div, input, span, (!), stringValue, i, dataAttribute, select)
import Text.Blaze.Html5.Attributes (class_, id, type_, name, value)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import TypeClass
import Data
import GHC.Generics (Rep)

instance Field Int Html where
    fieldHtml _ = numField

instance Field Double Html where
    fieldHtml _ = numField

instance Field Day Html where
    fieldHtml _ defaultVal fName = div ! class_ "input-group date" ! id (stringValue (fName ++ "Picker")) $ 
                   do
                     genericField defaultVal fName
                     span ! class_ "input-group-addon" $
                          i ! class_ "glyphicon glyphicon-calendar" $ ""

instance Field (Maybe Day) Html where
    fieldHtml _ defaultVal fName = div ! class_ "input-group date" ! id (stringValue (fName ++ "Picker")) $ 
                   do
                     genericField defaultVal fName
                     span ! class_ "input-group-addon" $
                          i ! class_ "glyphicon glyphicon-calendar" $ ""
-- TODO: add defaul value
instance Field String Html where
    fieldHtml _ defaultVal fName = select ! class_ "form-control selectpicker" ! name (stringValue fName)
                                   ! dataAttribute "datatype" "Underlying" $ ""


instance Field Text Html where
    fieldHtml _ =  genericField

-- TODO: add default value
instance Field Bool Html where
    fieldHtml _ _ fName = input ! type_ "checkbox" ! name (stringValue fName) ! dataAttribute "datatype" "Bool"

instance Field PercentField Html where
    fieldHtml _ defaultVal fName =
        do
          div ! class_ "input-group" $ do
                       input ! type_ "text" ! class_ "form-control" ! name (stringValue fName) ! dataAttribute "datatype" "PercentField"
                             ! value (stringValue (showDefaultVal defaultVal))
                       span ! class_ "input-group-addon" $ "%"                       

genericField :: Maybe String -> String -> Html
genericField defaultVal fName = input ! type_ "text" ! class_ "form-control" ! name (stringValue fName)
                                ! value (stringValue (showDefaultVal defaultVal))
numField :: Maybe String -> String -> Html
numField defaultVal fName  = input ! type_ "text" ! class_ "form-control" ! name (stringValue fName) 
                                   ! dataAttribute "datatype" "Double" ! value (stringValue (showDefaultVal defaultVal))

showDefaultVal = fromMaybe ""

renderField :: Maybe String -> (String, FormField Html) -> Html
renderField d (name, (MkField a)) = (fieldHtml a d name) :: Html
