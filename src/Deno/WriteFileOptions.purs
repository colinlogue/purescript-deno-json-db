module Deno.WriteFileOptions where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)


foreign import data WriteFileOptions :: Type

foreign import empty :: WriteFileOptions

foreign import append_ :: Boolean -> WriteFileOptions

foreign import create :: Boolean -> WriteFileOptions

foreign import createNew :: Boolean -> WriteFileOptions

foreign import mode :: Int -> WriteFileOptions

foreign import combine :: Fn2 WriteFileOptions WriteFileOptions WriteFileOptions

instance Semigroup WriteFileOptions where
  append = runFn2 combine

instance Monoid WriteFileOptions where
  mempty = empty
