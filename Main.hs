{-# LANGUAGE CPP #-}
module Main where

import Control.Monad.IO.Class
import Data.Foldable
import Data.IORef
import GHCJS.DOM
import GHCJS.DOM.Types
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.Node
import GHCJS.DOM.EventM
import GHCJS.DOM.GlobalEventHandlers

#if defined(ghcjs_HOST_OS)
run :: a -> a
run = id
#elif defined(MIN_VERSION_jsaddle_wkwebview)
import Language.Javascript.JSaddle.WKWebView (run)
#else
import Language.Javascript.JSaddle.WebKitGTK (run)
#endif

main :: IO ()
main = run mtgLife

elementWithParams :: Document -> String -> [(String, String)] -> JSM Element
elementWithParams doc elemName elemAttributes = do
  elem <- createElement doc elemName
  traverse_ (\(k, v) -> setAttribute elem k v) elemAttributes
  return elem

textElementWithParams :: Document -> String -> [(String, String)] -> String -> JSM Element
textElementWithParams doc elemName elemAttributes textContent = do
  elem <- elementWithParams doc elemName elemAttributes
  setTextContent elem $ Just textContent
  return elem

scoreUpdate :: IsNode a => IORef Int -> a -> Int -> JSM ()
scoreUpdate numberRef numberElem scoreChange = do
  newScore <- atomicModifyIORef' numberRef (\s -> (s + scoreChange, s + scoreChange))
  setTextContent numberElem $ Just $ show newScore

scoreChangeButton :: Document -> String -> Int -> (Int -> EventM HTMLDivElement MouseEvent ()) -> JSM Element
scoreChangeButton doc label changeBy changeHandler = do
  changeButton <- textElementWithParams doc "div" [("style", "width: 20%; height: 20%; margin: auto;")] label
  changeDiv <- unsafeCastTo HTMLDivElement changeButton
  _ <- on changeDiv click $ changeHandler changeBy
  return changeButton

playerPanel :: Document -> JSM Element
playerPanel doc = do
  scoreRef <- newIORef 20
  panelElem <- elementWithParams doc "div" [("style", "width: 50%; height: 100%; display: flex;")]
  numberContainer <- elementWithParams doc "div" [("style", "width: 20%; height: 20%; margin: auto;")]
  appendChild_ panelElem numberContainer
  numberElem <- textElementWithParams doc "div" [] "20"
  let scoreChangeHandler change = preventDefault >> liftIO (scoreUpdate scoreRef numberElem change)
  plus <- scoreChangeButton doc "+" 1 scoreChangeHandler
  minus <- scoreChangeButton doc "-" (-1) scoreChangeHandler
  appendChild_ panelElem minus
  appendChild_ numberContainer numberElem
  appendChild_ panelElem plus
  return panelElem

mtgLife :: JSM ()
mtgLife = do
  doc <- currentDocumentUnchecked
  body <- getBodyUnchecked doc
  root <- elementWithParams doc "div" [("style", "display: flex; flex-direction: row; height: 100vh;")]
  appendChild_ body root
  leftPanel <- playerPanel doc
  appendChild_ root leftPanel
  rightPanel <- playerPanel doc
  appendChild_ root rightPanel
  return ()