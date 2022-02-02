{-# LANGUAGE OverloadedStrings #-}

module Shortener where

import Web.Scotty
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5 ((!))
import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.Map (Map, empty, insert, toList)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (for_)

shortener :: IO ()
shortener = do
  urlsRef <- newIORef (1 :: Int, empty :: Map Int Text)
  scotty 3000 $ do
    get "/" $ do
      (_, urls) <- liftIO $ readIORef urlsRef
      html $ renderHtml $
        H.docTypeHtml $ do
          H.head $
            H.title "Shortener"
          H.body $ do
            H.form ! A.method "post" ! A.action "/" $ do
              H.input ! A.type_ "text" ! A.name "url"
              H.input ! A.type_ "submit" ! A.value "Submit"
          H.table $ do
            for_ (toList urls) $ \(i, url) ->
              H.tr $ do
                H.td (H.toHtml i)
                H.td (H.toHtml url)
    post "/" $ do
      url <- param "url"
      liftIO $ modifyIORef' urlsRef $
        \(i, urls) ->
          (i + 1, insert i url urls)
      redirect "/"
