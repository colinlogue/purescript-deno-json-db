module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Deno (runSpecAndExitProcess)
import Test.JsonDb.Database as Test.JsonDb.Database
import Test.JsonDb.Server as Test.JsonDb.Server
import Test.JsonDatabase as Test.JsonDatabase
import Test.Spec.Assertions (shouldSatisfy)
import Effect.Class (liftEffect)


main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Test.JsonDb.Database.spec
  Test.JsonDb.Server.spec
  Test.JsonDatabase.spec
