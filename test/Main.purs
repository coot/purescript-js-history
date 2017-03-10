module Test.Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF, newRef, readRef, modifyRef)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import DOM.Event.Types (EventType(..))
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Assert (assert)

import Data.Array as A
import History.MemoryHistory

-- main :: forall eff. Aff ( avar :: AVAR, console :: CONSOLE, testOutput :: TESTOUTPUT, err :: EXCEPTION, ref :: REF | eff) Unit
main = runTest do
    suite "MemoryHistory" do
        test "listeners are called" $ liftEff do
            ref <- newRef ([] :: Array String)
            mh <- unsafeCoerceEff memoryHistory
            unsafeCoerceEff $ do
                mh.addEventListener (EventType "popstate") (\_ -> modifyRef ref (flip A.snoc "id:1"))
                mh.addEventListener (EventType "popstate") (\_ -> modifyRef ref (flip A.snoc "id:1"))
            unsafeCoerceEff $ mh.history.pushState "next" "next" 0
            ids <- liftEff $ readRef ref
            liftAff (assert "all eventListeners were called" $ ids == [ "id:1" ])
