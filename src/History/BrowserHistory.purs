module History.BrowserHistory
  ( browserHistory
  ) where

import Prelude (Unit, bind, (>>=))
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.History (Delta, DocumentTitle, URL, back, forward, go, pushState, replaceState, state)
import DOM.HTML.Types (HISTORY)
import DOM.HTML.Window (history)
import Data.Foreign (Foreign)
import History.Types (History)


-- | broweserHistory - object with ffi to the browser history
-- | [api](https://developer.mozilla.org/en-US/docs/Web/API/History)
browserHistory
  :: forall e. History e
browserHistory =
  { state: _state
  , go: _go
  , back: _back
  , forward: _forward
  , pushState: _pushState
  , replaceState: _replaceState
  }
    where
      -- note: type annotations prevent `state` from escaping its scope type error

      _state :: Eff(history :: HISTORY, dom :: DOM | e) Foreign
      _state = window >>= history >>= state

      _go
        :: Delta
        -> Eff (history :: HISTORY, dom :: DOM | e) Unit
      _go d =
        do
          h <- window >>= history
          go d h

      _back :: Eff (history :: HISTORY, dom :: DOM | e) Unit
      _back = 
        do
          h <- window >>= history
          back h

      _forward :: Eff (history :: HISTORY, dom :: DOM | e) Unit
      _forward =
        do
          h <- window >>= history
          forward h

      _pushState
        :: Foreign
        -> DocumentTitle
        -> URL
        -> Eff (history :: HISTORY, dom :: DOM | e) Unit
      _pushState s t u = 
        do
          h <- window >>= history
          pushState s t u h

      _replaceState
        :: Foreign
        -> DocumentTitle
        -> URL
        -> Eff (history :: HISTORY, dom :: DOM | e) Unit
      _replaceState s t u =
        do
          h <- window >>= history
          replaceState s t u h

