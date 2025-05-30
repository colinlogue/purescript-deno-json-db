module Test.JsonDatabase where

import Prelude

import Data.Argonaut (Json, JsonDecodeError(..), decodeJson, encodeJson, fromObject)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import JsonDatabase as JsonDatabase
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)
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
spec = describe "JsonDatabase" do
  describe "Basic operations" do
    -- Basic tests for direct database operations
    it "should handle set and get operations" do
      let
        location = { index: ["test", "direct"], key: "directKey" }
        value = TestRecord { value: "direct test", count: 100 }

      _ <- JsonDatabase.set encodeTestRecord location value
      result <- JsonDatabase.get decodeTestRecord location

      result `shouldSatisfy` case _ of
        Right v -> v == value
        _ -> false

    it "should handle update operations" do
      let
        location = { index: ["test", "update"], key: "updateKey" }
        initialValue = TestRecord { value: "update test", count: 50 }
        updateFunction = \(TestRecord r) -> TestRecord r { count = r.count * 2 }

      _ <- JsonDatabase.set encodeTestRecord location initialValue
      _ <- JsonDatabase.update encodeTestRecord decodeTestRecord location updateFunction
      result <- JsonDatabase.get decodeTestRecord location

      result `shouldSatisfy` case _ of
        Right (TestRecord r) -> r.count == 100 && r.value == "update test"
        _ -> false
