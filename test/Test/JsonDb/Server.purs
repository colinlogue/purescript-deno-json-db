module Test.JsonDb.Server where

import Prelude

import Control.Monad.Error.Class (throwError, try)
import Control.Promise as Promise
import Data.Argonaut (Json, JsonDecodeError(..), decodeJson, encodeJson, fromObject, jsonParser, stringify)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Foreign.Object as Object
import JsonDb.Database (JsonDatabaseInit)
import JsonDb.Server (JsonServer, closeServer, startJsonDatabaseServer)
import Promise (Promise) as P
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Unsafe.Coerce (unsafeCoerce)
import Web.Fetch (fetch)
import Web.Fetch.Headers (fromRecord)
import Web.Fetch.Integrity (Integrity(..))
import Web.Fetch.ReferrerPolicy as ReferrerPolicy
import Web.Fetch.Request (Request)
import Web.Fetch.Request as Request
import Web.Fetch.RequestBody as RequestBody
import Web.Fetch.RequestCache as RequestCache
import Web.Fetch.RequestCredentials as RequestCredentials
import Web.Fetch.RequestMode as RequestMode
import Web.Fetch.Response as Response

-- Test data
newtype TestRecord = TestRecord { value :: String, count :: Int }

derive instance Eq TestRecord

-- Helper function to convert between Promise types
convertPromise :: forall a. P.Promise a -> Promise.Promise a
convertPromise = unsafeCoerce

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

-- Server test utilities
type TestServerConfig =
  { server :: JsonServer TestRecord
  , port :: Int
  , dbInit :: JsonDatabaseInit TestRecord
  }

-- Setup and teardown for server tests
withTestServer :: (TestServerConfig -> Aff Unit) -> Aff Unit
withTestServer testFn = do
  -- Get random port to avoid conflicts
  port <- liftEffect $ randomInt 8000 9000
  let dbInit =
        { encode: encodeTestRecord
        , decode: decodeTestRecord
        , root: ["test", "data", "server"]
        }
  let serverInit =
        { database: dbInit
        , port
        }

  server <- liftEffect $ startJsonDatabaseServer serverInit

  -- Delay to ensure server is ready
  delay (Milliseconds 50.0)

  -- Run the test
  result <- try $ testFn { server, port, dbInit }

  -- Cleanup
  liftEffect $ closeServer server

  -- Re-throw any errors
  case result of
    Left err -> throwError err
    Right _ -> pure unit

-- Helper functions for API requests
getBaseUrl :: Int -> String
getBaseUrl port = "http://localhost:" <> Int.toStringAs Int.decimal port

createRequest :: Method -> String -> String -> Maybe Json -> Effect Request
createRequest method baseUrl path bodyJson = do
  let url = baseUrl <> path
      headers = fromRecord
        { "Content-Type": "application/json"
        }
      body = case bodyJson of
        Just json -> RequestBody.fromString (stringify json)
        Nothing -> RequestBody.empty
  Request.new url
    { method
    , headers
    , body
    , cache: RequestCache.Default
    , credentials: RequestCredentials.SameOrigin
    , mode: RequestMode.Cors
    , referrer: Nothing
    , referrerPolicy: ReferrerPolicy.NoReferrer
    , integrity: Integrity ""
    }

-- Helper for string contains check
contains :: String -> String -> Boolean
contains str substr = String.indexOf (Pattern substr) str /= Nothing

-- Test specs
spec :: Spec Unit
spec = describe "JsonDb.Server" do
  around withTestServer do
    describe "Server functionality" do
      it "should start and accept connections" \{ port } -> do
        let baseUrl = getBaseUrl port

        -- Test with OPTIONS request first to verify server is running
        request <- liftEffect $ createRequest OPTIONS baseUrl "/test/data/server/records/connection" Nothing

        response <- liftEffect (fetch request) >>= (convertPromise >>> Promise.toAff)
        Response.status response `shouldEqual` 200

      it "should handle PUT requests to create data" \{ port } -> do
        let baseUrl = getBaseUrl port
            record = TestRecord { value: "server test", count: 42 }
        request <- liftEffect $ createRequest PUT baseUrl "/test/data/server/records/put-test" (Just (encodeTestRecord record))

        response <- liftEffect (fetch request) >>= (convertPromise >>> Promise.toAff)
        Response.status response `shouldEqual` 200

        jsonText <- liftEffect (Response.text response) >>= (convertPromise >>> Promise.toAff)
        case jsonParser jsonText of
          Right json -> case decodeTestRecord json of
            Right result -> result `shouldEqual` record
            Left err -> fail $ "Failed to decode response: " <> printJsonDecodeError err
          Left err -> fail $ "Failed to parse JSON: " <> err

      it "should handle GET requests to retrieve data" \{ port } -> do
        let baseUrl = getBaseUrl port
            record = TestRecord { value: "retrieve test", count: 100 }
        putRequest <- liftEffect $ createRequest PUT baseUrl "/test/data/server/records/get-test" (Just (encodeTestRecord record))

        -- First create the record
        _ <- liftEffect (fetch putRequest) >>= (convertPromise >>> Promise.toAff)

        -- Then retrieve it
        getRequest <- liftEffect $ createRequest GET baseUrl "/test/data/server/records/get-test" Nothing

        response <- liftEffect (fetch getRequest) >>= (convertPromise >>> Promise.toAff)
        Response.status response `shouldEqual` 200

        jsonText <- liftEffect (Response.text response) >>= (convertPromise >>> Promise.toAff)
        case jsonParser jsonText of
          Right json -> case decodeTestRecord json of
            Right result -> result `shouldEqual` record
            Left err -> fail $ "Failed to decode response: " <> printJsonDecodeError err
          Left err -> fail $ "Failed to parse JSON: " <> err

      it "should return 404 for non-existent records" \{ port } -> do
        let baseUrl = getBaseUrl port
        getRequest <- liftEffect $ createRequest GET baseUrl "/test/data/server/records/non-existent" Nothing

        response <- liftEffect (fetch getRequest) >>= (convertPromise >>> Promise.toAff)
        Response.status response `shouldEqual` 404

      it "should handle CORS with OPTIONS requests" \{ port } -> do
        let baseUrl = getBaseUrl port
        request <- liftEffect $ createRequest OPTIONS baseUrl "/test/data/server/records/options-test" Nothing

        response <- liftEffect (fetch request) >>= (convertPromise >>> Promise.toAff)
        Response.status response `shouldEqual` 200

        -- For OPTIONS requests, we just check the status code
        -- The CORS headers are set in the server implementation

      it "should reject unsupported methods" \{ port } -> do
        let baseUrl = getBaseUrl port
        request <- liftEffect $ createRequest DELETE baseUrl "/test/data/server/records/method-test" Nothing

        response <- liftEffect (fetch request) >>= (convertPromise >>> Promise.toAff)
        Response.status response `shouldEqual` 405

      it "should handle invalid JSON in PUT requests" \{ port } -> do
        let baseUrl = getBaseUrl port
            -- For this test, we need to create a request with invalid JSON directly
            url = baseUrl <> "/test/data/server/records/invalid-json"
            headers = fromRecord { "Content-Type": "application/json" }
            invalidJson = "{invalid json"

        -- Create a request with invalid JSON body
        request <- liftEffect $ Request.new url
              { method: PUT
              , headers
              , body: RequestBody.fromString invalidJson
              , cache: RequestCache.Default
              , credentials: RequestCredentials.SameOrigin
              , mode: RequestMode.Cors
              , referrer: Nothing
              , referrerPolicy: ReferrerPolicy.NoReferrer
              , integrity: Integrity ""
              }

        response <- liftEffect (fetch request) >>= (convertPromise >>> Promise.toAff)
        Response.status response `shouldEqual` 400
