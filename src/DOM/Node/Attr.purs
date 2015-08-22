module DOM.Node.Attr where

import Control.Monad.Eff (Eff())

import Data.Nullable (Nullable())

import DOM
import DOM.Node.Types

foreign import setAttribute :: forall eff. String -> String -> Element -> Eff (dom :: DOM | eff) String
foreign import getAttribute :: forall eff. String -> Element -> Eff (dom :: DOM | eff) (Nullable String)
