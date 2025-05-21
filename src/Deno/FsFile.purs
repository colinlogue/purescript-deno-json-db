module Deno.FsFile where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn3, EffectFn4, EffectFn5, mkEffectFn1, runEffectFn1, runEffectFn3, runEffectFn4, runEffectFn5)


foreign import data FsFile :: Type

foreign import _close :: EffectFn1 FsFile Unit

close :: FsFile -> Effect Unit
close = runEffectFn1 _close

foreign import _lock :: EffectFn4 FsFile Boolean (Effect Unit) (EffectFn1 Error Unit) Unit

lock :: Boolean -> FsFile -> Aff Unit
lock blocking file = makeAff \cb ->
  let
    onSuccess = cb (Right unit)
    onFailure = cb <<< Left
  in
    runEffectFn4 _lock file blocking onSuccess (mkEffectFn1 onFailure) *> mempty

foreign import _unlock :: EffectFn3 FsFile (Effect Unit) (EffectFn1 Error Unit) Unit

unlock :: FsFile -> Aff Unit
unlock file = makeAff \cb ->
  let
    onSuccess = cb (Right unit)
    onFailure = cb <<< Left
  in
    runEffectFn3 _unlock file onSuccess (mkEffectFn1 onFailure) *> mempty

foreign import _seek :: EffectFn5 Int SeekMode FsFile (Effect Unit) (EffectFn1 Error Unit) Unit

foreign import data SeekMode :: Type

foreign import seekStart :: SeekMode

foreign import seekCurrent :: SeekMode

foreign import seekEnd :: SeekMode

seek :: Int -> SeekMode -> FsFile -> Aff Unit
seek offset mode file = makeAff \cb ->
  let
    onSuccess = cb (Right unit)
    onFailure = cb <<< Left
  in
    runEffectFn5 _seek offset mode file onSuccess (mkEffectFn1 onFailure) *> mempty

foreign import _truncate :: EffectFn4 (Nullable Int) FsFile (Effect Unit) (EffectFn1 Error Unit) Unit

truncate :: Maybe Int -> FsFile -> Aff Unit
truncate size file = makeAff \cb ->
  let
    onSuccess = cb (Right unit)
    onFailure = cb <<< Left
  in
    runEffectFn4 _truncate (toNullable size) file onSuccess (mkEffectFn1 onFailure) *> mempty