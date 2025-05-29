module JsonDb.Server where

import Prelude

import Control.Monad.Error.Class (liftMaybe, try)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut (Json, encodeJson)
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
import JsonDb.Database (JsonDatabase, JsonDatabaseInit, createDatabase, getDatabaseInterface)
import Web.Fetch.Headers (Headers)
import Web.Fetch.Headers as Headers
import Web.Fetch.Request (Request)
import Web.Fetch.Response (Response)
import Web.URL as URL

type JsonServerInit a =
  { database :: JsonDatabaseInit a
  , port :: Int
  }

newtype JsonServer a = JsonServer
  { database :: JsonDatabase a
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
jsonResponse json = responseJson json $ Headers.fromRecord { "Access-Control-Allow-Origin": "*" }

errorResponse :: String -> Int -> Aff Response
errorResponse message status = responseJson (encodeJson { error: message, status }) $ Headers.fromRecord { "Access-Control-Allow-Origin": "*" }

foreign import requestUrl :: Request -> String

foreign import requestMethod :: Request -> String

foreign import newResponse :: EffectFn2 String Headers Response

handleRequest :: forall a. JsonDatabase a -> JsonDatabaseInit a -> Request -> Aff Response
handleRequest database { encode, decode, root } request = do
  let url = URL.unsafeFromAbsolute $ requestUrl request
  let path = Array.drop 1 $ String.split (Pattern "/") $ URL.pathname url
  relativePath <- liftMaybe (error "Invalid path") $ relativePathFromRoot root path
  location <- liftMaybe (error "Empty path") $ locationFromPath relativePath
  let dbInterface = getDatabaseInterface database
  case requestMethod request of
    "GET" -> do
      result <- dbInterface.get location
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
              dbInterface.set location value
              jsonResponse $ encode value
            Left err -> errorResponse ("Invalid JSON: " <> show err) 400
        Left err -> errorResponse ("Invalid JSON: " <> show err) 400
    _ -> errorResponse "Method Not Allowed" 405

startJsonDatabaseServer :: forall a. JsonServerInit a -> Effect (JsonServer a)
startJsonDatabaseServer { database: dbInit, port } = do
  let database = createDatabase dbInit

  let requestHandler :: ServeHandler _
      requestHandler _ request = do
        -- Handle OPTIONS requests for CORS
        if requestMethod request == "OPTIONS" then
          let
            method = "OPTIONS"
            headers = Headers.fromRecord
              { "Access-Control-Allow-Origin": "*"
              , "Access-Control-Allow-Methods": "GET, PUT, OPTIONS"
              , "Access-Control-Allow-Headers": "Content-Type"
              }
          in liftEffect $ runEffectFn2 newResponse method headers
        else do
          -- Handle regular requests
          handleRequest database dbInit request

  let serverOptions =
        { port: Int.toNumber port
        , hostname: "0.0.0.0"
        , signal: null
        , reusePort: false
        }

  server <- HttpServer.serveTcp serverOptions requestHandler

  pure $ JsonServer
    { database
    , close: runAff_ (const $ pure unit) $ HttpServer.shutdown server
    }

-- Accessor functions
getDatabase :: forall a. JsonServer a -> JsonDatabase a
getDatabase (JsonServer { database }) = database

closeServer :: forall a. JsonServer a -> Effect Unit
closeServer (JsonServer { close }) = close
