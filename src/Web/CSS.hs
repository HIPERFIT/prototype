{-# LANGUAGE OverloadedStrings #-}
module CSS  where

import Clay
import Data.Text.Lazy (Text)

layoutCss :: Text
layoutCss = render $ do
              element ".jumbotron" ? (textAlign $ alignSide sideCenter)
              element ".container" ? (width $ px 500)
              element "#result" ? (display none)
              element "#error" ? (display none)
