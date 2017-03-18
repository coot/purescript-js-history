module History.Types where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML.History (Delta, DocumentTitle, URL)
import DOM.HTML.Types (HISTORY)
import Data.Maybe (Maybe)
import Prelude (Unit)

type History e state =
  { state :: Eff (history:: HISTORY, dom :: DOM | e) (Maybe state)
  , back :: Eff (history:: HISTORY, dom :: DOM | e ) Unit
  , forward :: Eff (history:: HISTORY, dom :: DOM | e) Unit
  , go :: Delta -> Eff (history:: HISTORY, dom :: DOM | e) Unit
  , pushState :: state -> DocumentTitle -> URL -> Eff (history :: HISTORY, dom :: DOM | e) Unit
  , replaceState :: state -> DocumentTitle -> URL -> Eff (history :: HISTORY, dom :: DOM | e) Unit 
  }
