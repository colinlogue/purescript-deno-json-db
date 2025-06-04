module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Deno (runSpecAndExitProcess)
import Test.JsonDb.Database as Test.JsonDb.Database
import Test.JsonDb.Server as Test.JsonDb.Server


main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Test.JsonDb.Database.spec
  Test.JsonDb.Server.spec
