module Test.JsonDb.Server where

import Prelude

import Data.Argonaut (Json, JsonDecodeError(..), decodeJson, encodeJson, fromObject)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
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
spec = describe "JsonDb.Server" do
  -- This is a placeholder for server tests
  -- Real server tests would require more setup and teardown
  describe "Server functionality" do
    it "should be testable" do
      true `shouldEqual` true
