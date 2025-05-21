module Main where

import Prelude

import Data.Argonaut.Decode.Decoders (decodeArray, decodeString)
import Data.Argonaut.Encode.Encoders (encodeArray, encodeString)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (catchError, runAff_)
import Effect.Class.Console (log)
import JsonDatabase as JsonDatabase


main :: Effect Unit
main = runAff_ (const mempty) do
  log "Starting JsonDatabase example"
  JsonDatabase.set (encodeArray encodeString) { index: [ "foo", "bar" ], key: "baz" } [ "a", "b", "c" ]
  log "Set complete\n"
  getResult <- JsonDatabase.get (decodeArray decodeString) { index: [ "foo", "bar" ], key: "baz" }
  log $ "Get result: " <> show getResult
  case getResult of
    Right value -> do
      log $ "Value: " <> show value
      updateResult <- JsonDatabase.update (encodeArray encodeString) (decodeArray decodeString) { index: [ "foo", "bar" ], key: "baz" } \v -> v <> [ "d", "e", "f" ]
      case updateResult of
        Right _ -> do
          log "Update successful"
          getResult2 <- JsonDatabase.get (decodeArray decodeString) { index: [ "foo", "bar" ], key: "baz" }
          case getResult2 of
            Right newValue -> log $ "New Value: " <> show newValue
            Left err -> case err of
              JsonDatabase.NotFound -> log "Not found"
              JsonDatabase.InvalidJson e -> log $ "Invalid JSON: " <> show e
        Left err -> case err of
          JsonDatabase.NotFound -> log "Not found"
          JsonDatabase.InvalidJson e -> log $ "Invalid JSON: " <> show e
    Left err -> case err of
      JsonDatabase.NotFound -> log "Not found"
      JsonDatabase.InvalidJson e -> log $ "Invalid JSON: " <> show e
  `catchError` \err -> do
    log $ "Error: " <> show err
    pure unit
