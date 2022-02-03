{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( shortener,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (for_)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types.Status (status404)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty

shortener :: IO ()
shortener = do
  urlsRef <- newIORef (1 :: Int, M.empty :: M.Map Int Text)
  scotty 3000 $ do
    -- Index page.
    get "/" $ do
      (_, urls) <- liftIO $ readIORef urlsRef
      html $
        renderHtml $
          H.docTypeHtml $ do
            H.head $
              H.title "Shortener"
            H.body $ do
              H.form H.! A.method "post" H.! A.action "/" $ do
                H.input H.! A.type_ "text" H.! A.name "url"
                H.input H.! A.type_ "submit" H.! A.value "Submit"
            H.table $ do
              for_ (M.toList urls) $ \(i, url) ->
                H.tr $ do
                  H.td (H.toHtml i)
                  H.td (H.toHtml url)

    -- Shorten url from post request.
    post "/" $ do
      url <- param "url"
      liftIO $
        modifyIORef' urlsRef $
          \(i, urls) ->
            (i + 1, M.insert i url urls)
      redirect "/"

    -- Redirect to shortened url
    get "/:shortened" $ do
      (_, urls) <- liftIO $ readIORef urlsRef
      shortened <- param "shortened"
      case M.lookup shortened urls of
        Just u -> redirect $ LT.fromStrict u
        Nothing -> raiseStatus status404 "Shortened url not found."
