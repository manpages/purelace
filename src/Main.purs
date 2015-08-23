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
import qualified DOM.Node.Attr as A
import qualified Unsafe.Coerce as U

cardImgURL :: String -> String
cardImgURL "Purelace"       = "http://magiccards.info/scans/en/4e/293.jpg"
cardImgURL "Thoughtflare"   = "http://magiccards.info/scans/en/rtr/203.jpg"
cardImgURL "Memoricide"     = "http://magiccards.info/scans/en/mbp/29.jpg"
cardImgURL "Chill"          = "http://magiccards.info/scans/en/arena/19.jpg"
cardImgURL "Name Dropping"  = "http://magiccards.info/scans/en/uh/105.jpg"
cardImgURL "..."            = "http://magiccards.info/scans/en/uh/107.jpg"
cardImgURL _                = "http://magiccards.info/scans/en/uhaa/44.jpg"

coerceE :: HTMLElement -> Element
coerceE = htmlElementToElement

coerceD :: HTMLDocument -> Document
coerceD = htmlDocumentToDocument

e2n :: Element -> Node
e2n = elementToNode

n2e :: Node -> Element
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


foreachCol :: forall a eff. HTMLCollection -> 
                        (Int -> Element -> Eff (dom :: DOM | eff) a) ->
                        Eff (dom :: DOM | eff) HTMLCollection
foreachCol xs f = do
  l <- C.length xs
  foreachDo l xs f
    where
      foreachDo 0 xs _ = return xs
      foreachDo l xs f = do
        nx <- C.item (l - 1) xs
        let x = unsafeA nx
        _ <- f (l - 1) x
        foreachDo (l - 1) xs f

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
  xs  <- E.getElementsByClassName "card" b
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

a0 :: Attr
a0 = A.attr "src" $ cardImgURL "Garfield"

a1 :: Attr
a1 = A.attr "src" $ cardImgURL "..."

an :: AttrNS
an = A.attrNS "src" (cardImgURL "Name Dropping") (Just "http://memorici.de") Nothing

main :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
main = do
  d    <- doc'
  b    <- body'
  mapCards
  xs   <- E.getElementsByClassName "card" b
  foreachCol xs test
  log $ "Boom!"
    where
      test 0 parent = do
        xn <- N.firstChild $ e2n parent
        let x = n2e $ unsafeA xn
        A.setAttribute a0 x
        got0 <- A.getAttribute a0 x
        log $ show $ (unsafeA got0).value == cardImgURL "Garfield"
        A.setAttribute a1 x
        v0 <- A.getAttribute a0 x
        v1 <- A.getAttribute a1 x
        vn <- A.getAttributeNS an x
        A.removeAttribute a0 x
        log $ show $ (unsafeA v0).value == (unsafeA v1).value && (unsafeA vn).value == cardImgURL "..." && (unsafeA v0).value == cardImgURL "..."
      test 1 parent = do
        xn <- N.firstChild $ e2n parent
        let x = n2e $ unsafeA xn
        A.setAttributeNS an x
        A.setAttribute a1 x
        A.setAttribute a0 x
        log $ show true
      test _ parent = do
        xn <- N.firstChild $ e2n parent
        let x = n2e $ unsafeA xn
        A.setAttributeNS an x
        v0 <- A.hasAttribute a0 x
        v1 <- A.hasAttributeNS an x 
        log $ show v0
        log $ show v1
        A.removeAttributeNS an x
        v2 <- A.hasAttributeNS an x
        log $ show $ not v2
