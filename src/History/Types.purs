module History.Types where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML.History (Delta, DocumentTitle, URL)
import DOM.HTML.Types (HISTORY)
import Data.Foreign (Foreign)

type History e =
  { state :: Eff (history:: HISTORY, dom :: DOM | e) Foreign
  , back :: Eff (history:: HISTORY, dom :: DOM | e ) Unit
  , forward :: Eff (history:: HISTORY, dom :: DOM | e) Unit
  , go :: Delta -> Eff (history:: HISTORY, dom :: DOM | e) Unit
  , pushState :: Foreign -> DocumentTitle -> URL -> Eff (history :: HISTORY, dom :: DOM | e) Unit
  , replaceState :: Foreign -> DocumentTitle -> URL -> Eff (history :: HISTORY, dom :: DOM | e) Unit 
  }
