module Main where

import Prelude
import Control.Monad.Eff.Console
import DOM.HTML
import DOM.HTML.Types
import DOM.HTML.Window
import DOM.Node.Document

main = do
  w  <- window
  hd <- document w
  let d  = htmlDocumentToDocument hd
  u  <- url d
  log $ "Wow! " ++ show u
