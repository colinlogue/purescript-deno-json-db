module Deno where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, notNull, null, toNullable)
import Deno.WriteFileOptions (WriteFileOptions)
import Effect (Effect)
import Effect.Aff (Aff, Error, makeAff)
import Effect.Uncurried (EffectFn1, EffectFn3, EffectFn4, EffectFn5, mkEffectFn1, runEffectFn3, runEffectFn4, runEffectFn5)


type MkdirOptions =
  { recursive :: Maybe Boolean
  , mode :: Maybe Int
  }

foreign import _mkdir :: EffectFn4 (Nullable { recursive :: Nullable Boolean, mode :: Nullable Int }) String (Effect Unit) (EffectFn1 Error Unit) Unit

mkdir :: Maybe MkdirOptions -> String -> Aff Unit
mkdir opts path = makeAff \cb ->
  let
    onSuccess = cb (Right unit)
    onFailure = cb <<< Left
    options = case opts of
      Nothing -> null
      Just o -> notNull { recursive: toNullable o.recursive, mode: toNullable o.mode }
  in
    runEffectFn4 _mkdir options path onSuccess (mkEffectFn1 onFailure) *> mempty

foreign import _readTextFile :: EffectFn3 String (EffectFn1 String Unit) (EffectFn1 Error Unit) Unit

readTextFile :: String -> Aff String
readTextFile path = makeAff \cb ->
  let
    onSuccess = cb <<< Right
    onFailure = cb <<< Left
  in
    runEffectFn3 _readTextFile path (mkEffectFn1 onSuccess) (mkEffectFn1 onFailure) *> mempty

foreign import _writeTextFile :: EffectFn5 WriteFileOptions String String (Effect Unit) (EffectFn1 Error Unit) Unit

writeTextFile :: WriteFileOptions -> String -> String -> Aff Unit
writeTextFile opts path content = makeAff \cb ->
  let
    onSuccess = cb (Right unit)
    onFailure = cb <<< Left
  in
    runEffectFn5 _writeTextFile opts path content onSuccess (mkEffectFn1 onFailure) *> mempty

