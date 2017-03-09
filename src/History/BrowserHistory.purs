module History.BrowserHistory
  ( browserHistory
  ) where

import Prelude (Unit, unit)
import Control.Monad.Eff(Eff)
import Data.Maybe (Maybe(..))

import History.Types (History, PushState, HISTORY)

foreign import _length :: Eff (history:: HISTORY) Int

foreign import _state :: forall state. (state -> Maybe state) -> Maybe state -> Eff (history:: HISTORY) (Maybe state)

foreign import _forward:: Unit -> Eff (history:: HISTORY) Unit

foreign import _back:: Unit -> Eff (history:: HISTORY) Unit

foreign import _go:: Unit -> Int -> Eff (history:: HISTORY) Unit

foreign import _pushState :: forall state. Unit -> PushState state

foreign import _replaceState :: forall state. Unit -> PushState state

browserHistory :: forall state. History state
browserHistory =
    { length: _length
    , state: _state Just Nothing
    , go: _go unit
    , back: _back unit
    , forward: _forward unit
    , pushState: _pushState unit
    , replaceState: _replaceState unit
    }
