module JsonDatabase.Server where

import Prelude

import Control.Monad.Error.Class (liftMaybe, try)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut (Json, JsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Deno.HttpServer (AbortSignal, HttpServer)
import Deno.HttpServer as HttpServer
import Effect (Effect)
import Effect.Aff (Aff, error, makeAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import JsonDatabase (Location, RetrievalError(..))
import JsonDatabase as JsonDatabase
import Web.Fetch.Request (Request)
import Web.Fetch.Request as Request
import Web.Fetch.Response (Response)
import Web.Fetch.Response as Response
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

requestJson :: Request -> Aff Json
requestJson req = Promise.toAff (Request.json req)

relativePathFromRoot :: Array String -> Array String -> Maybe (Array String)
relativePathFromRoot root path =
  case Array.uncons root /\ Array.uncons path of
    Nothing /\ _ -> Just path
    Just { head: rootHead, tail: rootTail } /\ Just { head: pathHead, tail: pathTail } | rootHead == pathHead ->
      relativePathFromRoot rootTail pathTail
    _ -> Nothing

locationFromPath :: Array String -> Maybe Location
locationFromPath = Array.unsnoc >>> map \{ init: index, last: key } -> { index, key }

jsonResponse :: Json -> Aff Response
jsonResponse json = liftEffect $ Response.json json { headers = { "Access-Control-Allow-Origin": "*" } }

errorResponse :: String -> Int -> Aff Response
errorResponse message status = liftEffect $ 
  Response.json { error: message } { status, headers = { "Access-Control-Allow-Origin": "*" } }

handleRequest :: forall a. JsonServer a -> Request -> Aff Response
handleRequest (JsonServer { get, set, init: { encode, decode, root } }) request = do
  let url = URL.unsafeFromAbsolute $ Request.url request
  let path = Array.drop 1 $ String.split (Pattern "/") $ URL.pathname url
  relativePath <- liftMaybe (error "Invalid path") $ relativePathFromRoot (spy "root" root) (spy "path" path)
  location <- liftMaybe (error "Empty path") $ locationFromPath relativePath
  case Request.method request of
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

    requestHandler :: Request -> { remoteAddr :: _ } -> Aff Response
    requestHandler request _ = do
      liftEffect $ Console.log $ "Received request: " <> Request.method request <> " " <> Request.url request
      -- Handle OPTIONS requests for CORS
      if Request.method request == "OPTIONS" then
        liftEffect $ Response.new' { 
          headers: { 
            "Access-Control-Allow-Origin": "*", 
            "Access-Control-Allow-Methods": "GET, PUT, OPTIONS",
            "Access-Control-Allow-Headers": "Content-Type" 
          }
        }
      else do
        -- Handle regular requests
        response <- handleRequest (JsonServer dummyInterface) request
        -- Add CORS header to response
        -- As of now, we can't modify headers directly, so CORS is handled in the jsonResponse and errorResponse functions
        liftEffect $ Console.log $ "Sending response with status: " <> show (Response.status response)
        pure response
  in do
    let serverOptions = { port, hostname: "0.0.0.0", signal: toNullable Nothing }
    server <- HttpServer.serveTcp serverOptions requestHandler
    
    pure $ JsonServer dummyInterface 
      { close = liftEffect $ HttpServer.shutdown server *> pure unit }
