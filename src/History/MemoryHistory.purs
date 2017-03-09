module History.MemoryHistory
 ( memoryHistory
 ) where

import Prelude (($), (+), (-), negate, bind, map, min, max, pure, id)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Array as A
import Data.Maybe (maybe)
import History.Types (History)

type HistState state =
    { current:: Int
    , history:: Array { name:: String, url:: String, state:: state }
    }

memoryHistory:: forall state. Eff (ref:: REF) (History state)
memoryHistory = do
    ref <- newRef { current: (-1), history: [] }
    pure $
        { length: map getLength $ unsafeCoerceEff (readRef ref)
        , state: map getState $ unsafeCoerceEff (readRef ref)
        , back: unsafeCoerceEff $ modifyRef ref (go (-1))
        , forward: unsafeCoerceEff $ modifyRef ref (go 1)
        , go: \i -> unsafeCoerceEff $ modifyRef ref (go i)
        , pushState: \name url state -> unsafeCoerceEff $ modifyRef ref (pushState name url state)
        , replaceState: \name url state -> unsafeCoerceEff $ modifyRef ref (replaceState name url state)
        }
    where
        getLength s = A.length s.history
        getState s = map (\e -> e.state) $ s.history A.!! s.current

        go :: Int -> HistState state -> HistState state
        go i s = s { current = max 0 $ min (A.length s.history - 1) (s.current + i) }

        pushState name url state s = s { current = s.current + 1
                                       , history = A.snoc (A.take s.current s.history) { name, url, state }
                                       }
        replaceState name url state s =
            s { history = maybe s.history id $ A.updateAt s.current {name, url, state} s.history }
