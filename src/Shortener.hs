{-# LANGUAGE OverloadedStrings #-}

module Shortener where

import Web.Scotty
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H

shortener :: IO ()
shortener =
  scotty 3000 $
    get "/" $
      html $ renderHtml $
        H.docTypeHtml $ do
          H.head $ do
            H.title "Shortener"
          H.body $ do
            H.h1 "Shortener"
