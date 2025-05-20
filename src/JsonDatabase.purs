module JsonDatabase where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, parseJson, stringifyWithIndent)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Deno as Deno
import Deno.Errors (isNotFoundError)
import Effect.Aff (Aff, throwError, try)


type Location =
  { index :: Array String
  , key :: String
  }

set :: forall a. (a -> Json) -> Location -> a -> Aff Unit
set encode { index, key } value =
  let
    json = stringifyWithIndent 2 $ encode value
    dirname = String.joinWith "/" index
    path = dirname <> "/" <> key <> ".json"
  in do
    Deno.mkdir (Just { recursive: Just true, mode: Nothing }) dirname
    Deno.writeTextFile mempty path json

data RetrievalError
  = NotFound
  | InvalidJson JsonDecodeError

get :: forall a. (Json -> Either JsonDecodeError a) -> Location -> Aff (Either RetrievalError a)
get decode { index, key } =
  do
    let path = String.joinWith "/" index <> "/" <> key <> ".json"
    readResult <- try $ Deno.readTextFile path
    case readResult of
      Right contents -> do
        case parseJson contents of
          Right json -> pure $ case decode json of
            Right value -> Right value
            Left err -> Left (InvalidJson err)
          Left err -> pure $ Left (InvalidJson err)
      Left err ->
        if isNotFoundError err then
          pure $ Left NotFound
        else
          throwError err

update :: forall a. (a -> Json) -> (Json -> Either JsonDecodeError a) -> Location -> (a -> a) -> Aff (Either RetrievalError Unit)
update encode decode { index, key } f =
  do
    result <- get decode { index, key }
    case result of
      Right value -> do
        let newValue = f value
        pure <$> set encode { index, key } newValue
      Left err -> pure $ Left err
