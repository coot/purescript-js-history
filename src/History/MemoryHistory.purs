module History.MemoryHistory
 ( memoryHistory
 ) where

import Control.Monad.Eff.Ref
import Data.Array as A
import Unsafe.Coerce as U
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import DOM.Event.Types (Event, EventType, customEventToEvent)
import DOM.HTML.History (Delta(..), DocumentTitle, URL)
import DOM.HTML.Types (HISTORY)
import Data.Foldable (sequence_)
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..), snd)
import History.Types (History)
import Prelude (($), (+), (-), (<*>), (<<<), Unit, bind, id, map, max, min, negate, pure)

type HistState state =
    { current :: Int
    , history :: Array { title :: DocumentTitle , url :: URL, state :: state }
    }

type EventListener eff = Event -> Eff eff Unit

-- | memoryHistory is an Eff action that return a history together with
-- | corresponding `addEventListener` function
memoryHistory
  :: forall e state
  . Eff (ref :: REF, history :: HISTORY | e)
      { history :: History e state
      , addEventListener :: EventType -> EventListener (history :: HISTORY, ref :: REF | e) -> Eff (history :: HISTORY, ref :: REF | e) Unit
      }
memoryHistory = do
  ref <- newRef { current: (-1), history: [] }
  listeners <- newRef []
  pure 
    { history: { state: map getState $ unsafeCoerceEff (readRef ref)
               , back: unsafeCoerceEff $ modifyRef ref (go (-1))
               , forward: unsafeCoerceEff $ modifyRef ref (go 1)
               , go: \(Delta d) -> unsafeCoerceEff $ modifyRef ref (go d)
               , pushState: \state title url -> unsafeCoerceEff $ modifyRef ref $ pushState state title url (readRef listeners)
               , replaceState: \title url state -> unsafeCoerceEff $ modifyRef ref $ replaceState title url state
               }
    , addEventListener: \eventType listener -> unsafeCoerceEff $ modifyRef listeners (addEventListener eventType listener)
    }
  where
    getState s = map (\e -> e.state) $ s.history A.!! s.current

    go i s = s { current = max 0 $ min (A.length s.history - 1) (s.current + i) }

    pushState state title url l s = 
      let current = s.current + 1
          history = A.snoc (A.take s.current s.history) { title, url, state }
          -- that's not safe at all
          event = customEventToEvent <<< U.unsafeCoerce $ { "type": "popstate", "state": state }
      in (runPure <<< unsafeCoerceEff) do
        listeners <- l
        sequence_ $ map snd listeners <*> pure event
        pure { current, history }

    replaceState state title url s =
        s { history = maybe s.history id $ A.updateAt s.current {title, url, state} s.history }

    addEventListener eventType cb listeners = A.snoc listeners (Tuple eventType cb)
