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
              element "#result" ? (display none)
              element "#error" ? (display none)
