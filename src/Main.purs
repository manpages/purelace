module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import DOM
import DOM.HTML
import DOM.HTML.Types
import DOM.HTML.Window
import DOM.HTML.Document
import DOM.Node.Types
import DOM.Node.Element hiding (id)
import qualified DOM.Node.HTMLCollection as C
import Data.Nullable (Nullable(), toMaybe)
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import qualified DOM.Node.Node as N

cardImgURL :: String -> String
cardImgURL "Purelace"       = "http://magiccards.info/scans/en/4e/293.jpg"
cardImgURL "Thoughtflare"   = "http://magiccards.info/scans/en/rtr/203.jpg"
cardImgURL "Memoricide"     = "http://magiccards.info/scans/en/mbp/29.jpg"
cardImgURL "Chill"          = "http://magiccards.info/scans/en/arena/19.jpg"
cardImgURL _                = "http://magiccards.info/scans/en/uhaa/44.jpg"

coerceE :: HTMLElement -> Element
coerceE = htmlElementToElement

coerceD :: HTMLDocument -> Document
coerceD = htmlDocumentToDocument

htmlBody :: forall eff. Eff (dom :: DOM | eff) (Nullable HTMLElement)
htmlBody = do
  w <- window
  d <- document w
  body d

unsafeA :: forall a. (Nullable a) -> a
unsafeA = fromJust <<< toMaybe

main :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
main = do
  hb <- htmlBody
  let b = coerceE $ unsafeA $ hb
  hes  <- getElementsByClassName "card" b
  one  <- C.item 1 hes
  v    <- N.textContent $ elementToNode $ unsafeA $ one
  log $ "Wow! " ++ show v
