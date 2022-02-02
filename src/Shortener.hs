{-# LANGUAGE OverloadedStrings #-}

module Shortener where

import Web.Scotty
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5 ((!))
import Data.IORef (newIORef, modifyIORef')
import Data.Map (Map, empty, insert)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO(liftIO))

shortener :: IO ()
shortener = do
  urlsRef <- newIORef (1 :: Int, empty :: Map Int Text)
  scotty 3000 $ do
    get "/" $
      html $ renderHtml $
        H.docTypeHtml $ do
          H.head $
            H.title "Shortener"
          H.body $ do
            H.form ! A.method "post" ! A.action "/" $ do
              H.input ! A.type_ "text" ! A.name "url"
              H.input ! A.type_ "submit" ! A.value "Submit"
    post "/" $ do
      url <- param "url"
      liftIO $ modifyIORef' urlsRef $
        \(i, urls) ->
          (i + 1, insert i url urls)
      redirect "/"
