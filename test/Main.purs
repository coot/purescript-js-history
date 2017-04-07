module Test.Main where

import Prelude
import Data.Array as A
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF, newRef, readRef, modifyRef)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import DOM.Event.Types (EventType(..))
import DOM.HTML.Types (HISTORY)
import Data.Newtype (wrap)
import Data.Foreign (toForeign)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

import History

main :: forall e. Eff (testOutput :: TESTOUTPUT, avar :: AVAR, console :: CONSOLE, ref :: REF, history :: HISTORY | e) Unit
main = runTest do
  suite "MemoryHistory" do
    test "listeners are called" $ do
      ids <- liftEff do
        ref <- newRef ([] :: Array String)
        mh <- memoryHistory
        mh.addEventListener (EventType "popstate") (\_ -> modifyRef ref (flip A.snoc "id:1"))
        mh.addEventListener (EventType "popstate") (\_ -> modifyRef ref (flip A.snoc "id:2"))
        unsafeCoerceEff $ mh.history.pushState (toForeign 0) (wrap "next") (wrap "next")
        readRef ref
      assert ("all eventListeners were called: " <> show ids ) $ ids == [ "id:1", "id:2" ]
