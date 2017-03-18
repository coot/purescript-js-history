module History.MemoryHistory
 ( memoryHistory
 ) where

import Prelude (($), (+), (-), (<*>), (<<<), Unit, bind, id, map, max, min, negate, pure)
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import DOM.Event.Types (Event, EventType, customEventToEvent)
import Data.Array as A
import Data.Foldable (sequence_)
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..), snd)
import History.Types (History, HISTORY)
import Unsafe.Coerce as U

type HistState state =
    { current :: Int
    , history :: Array { name :: String, url :: String, state :: state }
    }

type EventListener eff = Event -> Eff eff Unit

memoryHistory
  :: forall state eff
   . Eff (history :: HISTORY, ref :: REF | eff)
      { history :: History state
      , addEventListener :: EventType -> EventListener (history :: HISTORY, ref :: REF | eff) -> Eff (history :: HISTORY, ref :: REF | eff) Unit
      }
memoryHistory = do
  ref <- newRef { current: (-1), history: [] }
  listeners <- newRef []
  pure $
    { history:
      { length: map getLength $ unsafeCoerceEff (readRef ref)
      , state: map getState $ unsafeCoerceEff (readRef ref)
      , back: unsafeCoerceEff $ modifyRef ref (go (-1))
      , forward: unsafeCoerceEff $ modifyRef ref (go 1)
      , go: \i -> unsafeCoerceEff $ modifyRef ref (go i)
      , pushState: \name url state -> unsafeCoerceEff $ modifyRef ref $ pushState name url state (readRef listeners)
      , replaceState: \name url state -> unsafeCoerceEff $ modifyRef ref $ replaceState name url state
      }
    , addEventListener: \eventType listener -> unsafeCoerceEff $ modifyRef listeners (addEventListener eventType listener)
    }
  where
    getLength s = A.length s.history
    getState s = map (\e -> e.state) $ s.history A.!! s.current

    go :: Int -> HistState state -> HistState state
    go i s = s { current = max 0 $ min (A.length s.history - 1) (s.current + i) }

    pushState
      :: String
      -> String
      -> state
      -> Eff (ref :: REF, history :: HISTORY | eff) (Array (Tuple EventType (EventListener (history :: HISTORY, ref :: REF | eff))))
      -> HistState state
      -> HistState state
    pushState name url state l s = 
      let current = s.current + 1
          history = A.snoc (A.take s.current s.history) { name, url, state }
          -- that's not safe at all
          event = customEventToEvent <<< U.unsafeCoerce $ { "type": "popstate", "state": state }
      in (runPure <<< unsafeCoerceEff) do
        listeners <- l
        sequence_ $ map snd listeners <*> pure event
        pure { current, history }

    replaceState name url state s =
        s { history = maybe s.history id $ A.updateAt s.current {name, url, state} s.history }

    addEventListener
      :: EventType
      -> EventListener (history :: HISTORY, ref :: REF | eff)
      -> Array (Tuple EventType (EventListener (history :: HISTORY, ref :: REF | eff)))
      -> Array (Tuple EventType (EventListener (history :: HISTORY, ref :: REF | eff)))
    addEventListener eventType cb listeners = A.snoc listeners (Tuple eventType cb)
