{-# LANGUAGE OverloadedStrings #-}
module CSS  where

import Clay
import Data.Text.Lazy (Text)

layoutCss :: Text
layoutCss = render $ do
              element ".jumbotron" ? (textAlign $ alignSide sideCenter)
              element ".right-container" ? do 
                       minWidth $ px 400
                       paddingLeft $ pct 10
                       paddingRight $ pct 30
              element ".right-container" ? (maxWidth $ px 950)
              element ".spinner" ? (position relative)
              element "#result" ? (display none)
              element "#error" ? (display none)
              element "#pricing-form-alert" ? (display none)
              element ".table" |> tbody |> tr |> element "td.vert-align" ? verticalAlign middle 
              element ".table" |> tbody |> tr |> element "td.fixed-height" ? (height $ px 60)
              element ".user-info" ? paddingRight (px 5)
              element ".form-buttons" |> a ? margin (px 5) (px 5) (px 5) (px 5)
              element ".pfitem-row" ? cursor pointer
              td |> element "div.date" ? do 
                maxWidth (px 200)
                minWidth (px 150)
              element "#stockChart" ? do
                width (auto)
                height (px 500)
              element "#contractChart" ? do
                width (auto)
                height (px 500)
