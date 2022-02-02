{-# LANGUAGE OverloadedStrings #-}

module Shortener where

import Web.Scotty
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5 ((!))

shortener :: IO ()
shortener = do
  scotty 3000 $
    get "/" $
      html $ renderHtml $
        H.docTypeHtml $ do
          H.head $
            H.title "Shortener"
          H.body $ do
            H.form ! A.method "post" ! A.action "/" $ do
              H.input ! A.type_ "text" ! A.name "url"
              H.input ! A.type_ "submit" ! A.value "Submit"
