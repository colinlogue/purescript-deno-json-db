module JsonDatabase
  ( Location
  , RetrievalError(..)
  , get
  , set
  , update
  ) where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, parseJson, stringifyWithIndent)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String as String
import Deno (mkdir, open) as Deno
import Deno.Errors (isNotFoundError)
import Deno.FsFile (FsFile)
import Deno.FsFile as FsFile
import Deno.MkdirOptions as MkdirOptions
import Deno.OpenOptions (OpenOptions)
import Deno.OpenOptions as OpenOptions
import Effect (Effect)
import Effect.Aff (Aff, finally, makeAff, throwError, try)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn3, EffectFn4, mkEffectFn1, runEffectFn3, runEffectFn4)


type Location =
  { index :: Array String
  , key :: String
  }

foreign import _writeTextToFile :: EffectFn4 String FsFile (Effect Unit) (EffectFn1 Error Unit) Unit

writeTextToFile :: String -> FsFile -> Aff Unit
writeTextToFile contents file = makeAff \cb ->
  let
    onSuccess = cb (Right unit)
    onFailure = cb <<< Left
  in
    runEffectFn4 _writeTextToFile contents file onSuccess (mkEffectFn1 onFailure) *> mempty

data LockType
  = Shared
  | Exclusive

isExclusive :: LockType -> Boolean
isExclusive = case _ of
  Shared -> false
  Exclusive -> true

withFile :: OpenOptions -> String -> forall a. (FsFile -> Aff a) -> Aff a
withFile opts path action = do
  file <- Deno.open opts path
  finally (liftEffect $ FsFile.close file) (action file)

withFileLock :: forall a. OpenOptions -> LockType -> String -> (FsFile -> Aff a) -> Aff a
withFileLock opts lockType path action = do
  withFile opts path \file -> withLock lockType file $ action file

withLock :: LockType -> FsFile -> forall a. Aff a -> Aff a
withLock lockType file action = do
  FsFile.lock (isExclusive lockType) file
  finally (FsFile.unlock file) action

set :: forall a. (a -> Json) -> Location -> a -> Aff Unit
set encode { index, key } value = do

  -- Create the directory if it doesn't exist.
  let dirname = String.joinWith "/" index
  Deno.mkdir (MkdirOptions.recursive true) dirname

  -- Open the file for writing, truncating any existing content.
  let path = dirname <> "/" <> key <> ".json"
  withFileLock (OpenOptions.create true <> OpenOptions.write true <> OpenOptions.truncate true) Shared path \file -> do

    -- Write the JSON content to the file.
    let json = stringifyWithIndent 2 $ encode value
    writeTextToFile json file

data RetrievalError
  = NotFound
  | InvalidJson JsonDecodeError

derive instance Generic RetrievalError _

instance Show RetrievalError where
  show = genericShow

foreign import _readTextFromFile :: EffectFn3 FsFile (EffectFn1 String Unit) (EffectFn1 Error Unit) Unit

readTextFromFile :: FsFile -> Aff String
readTextFromFile file = makeAff \cb ->
  let
    onSuccess = cb <<< Right
    onFailure = cb <<< Left
  in
    runEffectFn3 _readTextFromFile file (mkEffectFn1 onSuccess) (mkEffectFn1 onFailure) *> mempty

get :: forall a. (Json -> Either JsonDecodeError a) -> Location -> Aff (Either RetrievalError a)
get decode { index, key } =
  let
    path = String.joinWith "/" index <> "/" <> key <> ".json"

    action :: FsFile -> Aff (Either RetrievalError a)
    action file = do

      -- Read the initial contents of the file.
      contents <- readTextFromFile file

      -- Parse it as JSON and attempt to decode it.
      let value = parseJson contents >>= decode

      -- Return the correct error if decoding failed.
      pure $ lmap InvalidJson value

  in
    -- Attempt to read the file and decode it as JSON, returning the appropriate
    -- error if the file does not exist or rethrowing any other error.
    try (withFileLock (OpenOptions.read true) Exclusive path action) >>= case _ of
      Right result -> pure result
      Left err ->
        if isNotFoundError err then
          pure $ Left NotFound
        else
          throwError err

update :: forall a. (a -> Json) -> (Json -> Either JsonDecodeError a) -> Location -> (a -> a) -> Aff (Either RetrievalError Unit)
update encode decode { index, key } f =
  let
    path = String.joinWith "/" index <> "/" <> key <> ".json"

    action :: FsFile -> Aff (Either RetrievalError Unit)
    action file = do

      -- Read the initial contents of the file.
      contents <- readTextFromFile file

      -- Parse it as JSON and attempt to decode it.
      let value = parseJson contents >>= decode

      -- If decoding was successful, update the file with the new value.
      case value of
        Left err -> pure $ Left (InvalidJson err)
        Right value' -> Right <$> do

          -- Apply the update function to the decoded value and encode it back to JSON.
          let newValue = f value'
          let newJson = stringifyWithIndent 2 $ encode newValue

          -- Reset the file pointer to the beginning and truncate the file.
          FsFile.seek 0 FsFile.seekStart file
          FsFile.truncate Nothing file

          -- Write the new JSON content to the file.
          writeTextToFile newJson file

  in
    -- Attempt to read and write to the file, returning the appropriate error if the
    -- file does not exist or rethrowing any other error.
    try (withFileLock (OpenOptions.write true <> OpenOptions.read true) Exclusive path action) >>= case _ of
      Right result -> pure result
      Left err ->
        if isNotFoundError err then
          pure $ Left NotFound
        else
          throwError err
