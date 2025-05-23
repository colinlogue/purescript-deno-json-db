module JsonDatabase.Server where

import Prelude

import Control.Monad.Error.Class (liftMaybe, try)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut (Json, JsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Aff, error, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, runEffectFn1, runEffectFn2, runEffectFn3)
import JsonDatabase (Location, RetrievalError(..))
import JsonDatabase as JsonDatabase
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

foreign import _requestURL :: Fn1 Request String

foreign import _requestMethod :: Fn1 Request String

foreign import _requestJson :: EffectFn3 Request (EffectFn1 Json Unit) (EffectFn1 Error Unit) Json

requestJson :: Request -> Aff Json
requestJson req = makeAff \cb ->
  let
    onSuccess = cb <<< Right
    onFailure = cb <<< Left
  in
    runEffectFn3 _requestJson req (mkEffectFn1 onSuccess) (mkEffectFn1 onFailure) *> mempty

relativePathFromRoot :: Array String -> Array String -> Maybe (Array String)
relativePathFromRoot root path =
  case Array.uncons root /\ Array.uncons path of
    Nothing /\ _ -> Just path
    Just { head: rootHead, tail: rootTail } /\ Just { head: pathHead, tail: pathTail } | rootHead == pathHead ->
      relativePathFromRoot rootTail pathTail
    _ -> Nothing

locationFromPath :: Array String -> Maybe Location
locationFromPath = Array.unsnoc >>> map \{ init: index, last: key } -> { index, key }

foreign import _jsonResponse :: EffectFn1 Json Response

jsonResponse :: Json -> Aff Response
jsonResponse = runEffectFn1 _jsonResponse >>> liftEffect

foreign import _errorResponse :: EffectFn2 String Int Response

errorResponse :: String -> Int -> Aff Response
errorResponse message = runEffectFn2 _errorResponse message >>> liftEffect

handleRequest :: forall a. JsonServer a -> Request -> Aff Response
handleRequest (JsonServer { get, set, init: { encode, decode, root } }) request = do
  let url = URL.unsafeFromAbsolute $ _requestURL request
  let path = Array.drop 1 $ String.split (Pattern "/") $ URL.pathname url
  relativePath <- liftMaybe (error "Invalid path") $ relativePathFromRoot (spy "root" root) (spy "path" path)
  location <- liftMaybe (error "Empty path") $ locationFromPath relativePath
  case _requestMethod request of
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

foreign import _runServer :: EffectFn2 Int (EffectFn1 Request (Promise Response)) (Effect Unit)

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

    requestHandler :: Request -> Effect (Promise Response)
    requestHandler request = Promise.fromAff do
      handleRequest (JsonServer dummyInterface) request
  in do
    close <- runEffectFn2 _runServer port (mkEffectFn1 requestHandler)
    pure $ JsonServer dummyInterface { close = close }
