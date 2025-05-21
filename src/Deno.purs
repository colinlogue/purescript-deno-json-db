module Deno where

import Prelude

import Data.Either (Either(..))
import Deno.FsFile (FsFile)
import Deno.MkdirOptions (MkdirOptions)
import Deno.OpenOptions (OpenOptions)
import Deno.WriteFileOptions (WriteFileOptions)
import Effect (Effect)
import Effect.Aff (Aff, Error, makeAff)
import Effect.Uncurried (EffectFn1, EffectFn3, EffectFn4, EffectFn5, mkEffectFn1, runEffectFn3, runEffectFn4, runEffectFn5)


foreign import _mkdir :: EffectFn4 MkdirOptions String (Effect Unit) (EffectFn1 Error Unit) Unit

mkdir :: MkdirOptions -> String -> Aff Unit
mkdir opts path = makeAff \cb ->
  let
    onSuccess = cb (Right unit)
    onFailure = cb <<< Left
  in
    runEffectFn4 _mkdir opts path onSuccess (mkEffectFn1 onFailure) *> mempty

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

foreign import _open :: EffectFn4 OpenOptions String (EffectFn1 FsFile Unit) (EffectFn1 Error Unit) Unit

open :: OpenOptions -> String -> Aff FsFile
open opts path = makeAff \cb ->
  let
    onSuccess = cb <<< Right
    onFailure = cb <<< Left
  in
    runEffectFn4 _open opts path (mkEffectFn1 onSuccess) (mkEffectFn1 onFailure) *> mempty
