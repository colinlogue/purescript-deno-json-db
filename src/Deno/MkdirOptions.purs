module Deno.MkdirOptions where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)


foreign import data MkdirOptions :: Type

foreign import recursive :: Boolean -> MkdirOptions

foreign import mode :: Int -> MkdirOptions

foreign import empty :: MkdirOptions

foreign import combine :: Fn2 MkdirOptions MkdirOptions MkdirOptions

instance Semigroup MkdirOptions where
  append = runFn2 combine

instance Monoid MkdirOptions where
  mempty = empty
