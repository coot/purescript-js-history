module Test.Main where

import Prelude
import History.MemoryHistory
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
import Data.Newtype (wrap)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

-- main :: forall e. Eff (testOutput :: TESTOUTPUT, avar :: AVAR, console :: CONSOLE, ref :: REF | e) Unit
main = runTest do
  suite "MemoryHistory" do
    test "listeners are called" $ liftEff do
      ref <- newRef ([] :: Array String)
      mh <- memoryHistory
      mh.addEventListener (EventType "popstate") (\_ -> modifyRef ref (flip A.snoc "id:1"))
      mh.addEventListener (EventType "popstate") (\_ -> modifyRef ref (flip A.snoc "id:1"))
      unsafeCoerceEff $ mh.history.pushState 0 (wrap "next") (wrap "next")
      ids <- liftEff $ readRef ref
      liftAff (assert "all eventListeners were called" $ ids == [ "id:1" ])
