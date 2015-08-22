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

mapCol :: forall a eff. HTMLCollection -> 
                        (Element -> Eff (dom :: DOM | eff) a) ->
                        Eff (dom :: DOM | eff) HTMLCollection

mapCol xs f = do
  l <- C.length xs
  mapDo l xs f
    where
      mapDo 0 xs _ = return xs
      mapDo l xs f = do
        nx <- C.item (l - 1) xs
        let x = unsafeA nx
        _ <- f x
        mapDo (l - 1) xs f

mapCards :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) HTMLCollection
mapCards = do
  d   <- doc'
  b   <- body'
  xs <- E.getElementsByClassName "card" b
  mapCol xs $ f d
    where
      f d x = do
        let xn = e2n x
        fcxn <- N.firstChild xn
        card <- N.textContent $ unsafeA fcxn
        log card
        xn1  <- N.removeChild (unsafeA fcxn) xn
        img  <- ND.createElement "img" d
        _    <- E.setAttribute "src" (cardImgURL card) img
        N.appendChild (e2n img) xn

main :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
main = do
  mapCards
  log $ "Boom!"
