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
import qualified DOM.Node.Document as ND
import DOM.Node.Element hiding (id)
import qualified DOM.Node.Element as E
import qualified DOM.Node.HTMLCollection as C
import Data.Nullable (Nullable(), toMaybe)
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import qualified DOM.Node.Node as N
import qualified Unsafe.Coerce as U

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

e2n :: Element -> Node
e2n = elementToNode

n2e :: Element -> Node
n2e = U.unsafeCoerce

htmlDoc :: forall eff. Eff (dom :: DOM | eff) HTMLDocument
htmlDoc = do
  w <- window
  document w

doc' :: forall eff. Eff (dom :: DOM | eff) Document
doc' = do
  d <- htmlDoc
  return $ coerceD d

htmlBody :: forall eff. Eff (dom :: DOM | eff) (Nullable HTMLElement)
htmlBody = do
  w <- window
  d <- document w
  body d

body' :: forall eff. Eff (dom :: DOM | eff) Element
body' = do
  hb <- htmlBody
  return $ coerceE $ unsafeA hb

unsafeA :: forall a. (Nullable a) -> a
unsafeA = fromJust <<< toMaybe

main :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
main = do
  d <- doc'
  b <- body'
  img  <- ND.createElement "img" d
  _    <- E.setAttribute "src" (cardImgURL "Chill") img
  b1   <- N.appendChild (e2n img) (e2n b)
  src  <- E.getAttribute "src" img
  log $ show $ unsafeA src == cardImgURL "Chill"
