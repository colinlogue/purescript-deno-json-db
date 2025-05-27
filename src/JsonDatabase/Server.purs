module JsonDatabase.Server where

import Prelude

import Control.Monad.Error.Class (liftMaybe, try)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut (Json, JsonDecodeError, encodeJson)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Deno.HttpServer (ServeHandler)
import Deno.HttpServer as HttpServer
import Effect (Effect)
import Effect.Aff (Aff, error, runAff_)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import JsonDatabase (Location, RetrievalError(..))
import JsonDatabase as JsonDatabase
import Web.Fetch.Headers (Headers)
import Web.Fetch.Headers as Headaers
import Web.Fetch.Request (Request)
import Web.Fetch.Response (Response)
import Web.URL as URL


type JsonServerInit a =
  { encode :: a -> Json
  , decode :: Json -> Either JsonDecodeError a
  , root :: Array String
  , port :: Int
  }

newtype JsonServer a = JsonServer (JsonServerInterface a)

type JsonServerInterface a =
  { set :: Location -> a -> Aff Unit
  , get :: Location -> Aff (Either RetrievalError a)
  , update :: Location -> (a -> a) -> Aff (Either RetrievalError Unit)
  , init :: JsonServerInit a
  , close :: Effect Unit
  }

foreign import _requestJson :: EffectFn1 Request (Promise Json)

requestJson :: Request -> Aff Json
requestJson req = (liftEffect $ runEffectFn1 _requestJson req) >>= Promise.toAff

relativePathFromRoot :: Array String -> Array String -> Maybe (Array String)
relativePathFromRoot root path =
  case Array.uncons root /\ Array.uncons path of
    Nothing /\ _ -> Just path
    Just { head: rootHead, tail: rootTail } /\ Just { head: pathHead, tail: pathTail } | rootHead == pathHead ->
      relativePathFromRoot rootTail pathTail
    _ -> Nothing

locationFromPath :: Array String -> Maybe Location
locationFromPath = Array.unsnoc >>> map \{ init: index, last: key } -> { index, key }

foreign import _responseJson :: EffectFn2 Json Headers Response

responseJson :: Json -> Headers -> Aff Response
responseJson json headers = liftEffect $ runEffectFn2 _responseJson json headers

jsonResponse :: Json -> Aff Response
jsonResponse json = responseJson json $ Headaers.fromRecord { "Access-Control-Allow-Origin": "*" }

errorResponse :: String -> Int -> Aff Response
errorResponse message status = responseJson (encodeJson { error: message, status }) $ Headaers.fromRecord { "Access-Control-Allow-Origin": "*" }

foreign import requestUrl :: Request -> String

foreign import requestMethod :: Request -> String

-- TODO: Better typing
foreign import newResponse :: EffectFn2 String Headers Response

handleRequest :: forall a. JsonServer a -> Request -> Aff Response
handleRequest (JsonServer { get, set, init: { encode, decode, root } }) request = do
  let url = URL.unsafeFromAbsolute $ requestUrl request
  let path = Array.drop 1 $ String.split (Pattern "/") $ URL.pathname url
  relativePath <- liftMaybe (error "Invalid path") $ relativePathFromRoot root path
  location <- liftMaybe (error "Empty path") $ locationFromPath relativePath
  case requestMethod request of
    "GET" -> do
      result <- get location
      case result of
        Right value -> jsonResponse $ encode value
        Left NotFound -> errorResponse "Not Found" 404
        Left (InvalidJson _) -> errorResponse "Corrupted data" 500
    "PUT" -> do
      body <- try $ requestJson request
      case body of
        Right json -> do
          case decode json of
            Right value -> do
              set location value
              jsonResponse $ encode value
            Left err -> errorResponse ("Invalid JSON: " <> show err) 400
        Left err -> errorResponse ("Invalid JSON: " <> show err) 400
    _ -> errorResponse "Method Not Allowed" 405

startJsonDatabaseServer :: forall a. JsonServerInit a -> Effect (JsonServer a)
startJsonDatabaseServer init@{ encode, decode, root, port } =
  let
    dummyInterface :: JsonServerInterface a
    dummyInterface =
      { set: \location value -> JsonDatabase.set encode (updateLocation location) value
      , get: \location -> JsonDatabase.get decode (updateLocation location)
      , update: \location f -> JsonDatabase.update encode decode (updateLocation location) f
      , init
      , close: pure unit
      }

    updateLocation :: Location -> Location
    updateLocation = \location -> location { index = Array.concat [root, location.index] }

    requestHandler :: ServeHandler _
    requestHandler _ request = do
      -- Handle OPTIONS requests for CORS
      if requestMethod request == "OPTIONS" then
        let
          method = "OPTIONS"
          headers = Headaers.fromRecord
            { "Access-Control-Allow-Origin": "*"
            , "Access-Control-Allow-Methods": "GET, PUT, OPTIONS"
            , "Access-Control-Allow-Headers": "Content-Type"
            }
        in liftEffect $ runEffectFn2 newResponse method headers
      else do
        -- Handle regular requests
        handleRequest (JsonServer dummyInterface) request
  in do
    let
      serverOptions =
        { port: Int.toNumber port
        , hostname: "0.0.0.0"
        , signal: null
        , reusePort: false
        }
    server <- HttpServer.serveTcp serverOptions requestHandler

    pure $ JsonServer dummyInterface
      { close = runAff_ (const $ pure unit) $ HttpServer.shutdown server }
