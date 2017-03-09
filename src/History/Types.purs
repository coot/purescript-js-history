module History.Types where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe)

foreign import data HISTORY :: !

type State = String

type PushState state = String -> String -> state -> Eff (history:: HISTORY) Unit

type History state =
    { length:: Eff (history:: HISTORY) Int
    , state:: Eff (history:: HISTORY) (Maybe state)
    , back:: Eff (history:: HISTORY) Unit
    , forward:: Eff (history:: HISTORY) Unit
    , go:: Int -> Eff (history:: HISTORY) Unit
    , pushState:: PushState state
    , replaceState:: PushState state
    }
