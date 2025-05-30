module Test.JsonDb.Database where

import Prelude

import Data.Argonaut (Json, JsonDecodeError(..), decodeJson, encodeJson, fromObject)
import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import JsonDb.Database (Location, RetrievalError, JsonDatabase, createDatabase, getDatabaseInterface, get, set, update)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Foreign.Object as Object

-- Test data
newtype TestRecord = TestRecord { value :: String, count :: Int }

derive instance Eq TestRecord

instance Show TestRecord where
  show (TestRecord r) = "TestRecord { value: " <> show r.value <> ", count: " <> show r.count <> " }"

-- Functions to encode and decode TestRecord
encodeTestRecord :: TestRecord -> Json
encodeTestRecord (TestRecord rec) =
  fromObject $ Object.fromFoldable
    [ Tuple "value" (encodeJson rec.value)
    , Tuple "count" (encodeJson rec.count)
    ]

decodeTestRecord :: Json -> Either JsonDecodeError TestRecord
decodeTestRecord json = do
  obj <- decodeJson json
  value <- case Object.lookup "value" obj of
    Just v -> decodeJson v
    Nothing -> Left $ Named "value" $ MissingValue
  count <- case Object.lookup "count" obj of
    Just c -> decodeJson c
    Nothing -> Left $ Named "count" $ MissingValue
  pure $ TestRecord { value, count }

-- Test specs
spec :: Spec Unit
spec = describe "JsonDb.Database" do
  describe "Low-level operations" do
    -- Tests for direct database operations
    it "should handle set and get operations" do
      let
        location = { index: ["test", "data", "direct"], key: "directKey" }
        value = TestRecord { value: "direct test", count: 100 }

      _ <- set encodeTestRecord location value
      result <- get decodeTestRecord location

      result `shouldSatisfy` case _ of
        Right v -> v == value
        _ -> false

    it "should handle update operations" do
      let
        location = { index: ["test", "data", "update"], key: "updateKey" }
        initialValue = TestRecord { value: "update test", count: 50 }
        updateFunction = \(TestRecord r) -> TestRecord r { count = r.count * 2 }

      _ <- set encodeTestRecord location initialValue
      _ <- update encodeTestRecord decodeTestRecord location updateFunction
      result <- get decodeTestRecord location

      result `shouldSatisfy` case _ of
        Right (TestRecord r) -> r.count == 100 && r.value == "update test"
        _ -> false

  describe "Database operations" do
    it "should be able to set and get values" do
      let
        dbInit =
          { encode: encodeTestRecord
          , decode: decodeTestRecord
          , root: ["test", "data", "memoryDb"]
          }
        db = createDatabase dbInit
        interface = getDatabaseInterface db
        location = { index: ["records"], key: "testKey" }
        testValue = TestRecord { value: "test value", count: 42 }

      _ <- interface.set location testValue
      result <- interface.get location
      result `shouldSatisfy` isRight
      result `shouldSatisfy` case _ of
        Right value -> value == testValue
        _ -> false

    it "should be able to update values" do
      let
        dbInit =
          { encode: encodeTestRecord
          , decode: decodeTestRecord
          , root: ["test", "data", "memoryDb"]
          }
        db = createDatabase dbInit
        interface = getDatabaseInterface db
        location = { index: ["records"], key: "updateKey" }
        initialValue = TestRecord { value: "initial value", count: 10 }
        updateFunction = \(TestRecord r) -> TestRecord r { count = r.count + 1 }

      _ <- interface.set location initialValue
      _ <- interface.update location updateFunction
      result <- interface.get location
      result `shouldSatisfy` isRight
      result `shouldSatisfy` case _ of
        Right value -> value == TestRecord { value: "initial value", count: 11 }
        _ -> false
