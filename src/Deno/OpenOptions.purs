module Deno.OpenOptions where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..), maybe)


foreign import data OpenOptions :: Type

foreign import empty :: OpenOptions

foreign import read :: Boolean -> OpenOptions

foreign import write :: Boolean -> OpenOptions

foreign import append_ :: Boolean -> OpenOptions

foreign import truncate :: Boolean -> OpenOptions

foreign import create :: Boolean -> OpenOptions

foreign import createNew :: Boolean -> OpenOptions

foreign import mode :: Int -> OpenOptions

foreign import combine :: Fn2 OpenOptions OpenOptions OpenOptions

instance Semigroup OpenOptions where
  append = runFn2 combine

instance Monoid OpenOptions where
  mempty = empty

type OpenOptionsData =
  { read :: Maybe Boolean
  , write :: Maybe Boolean
  , append :: Maybe Boolean
  , truncate :: Maybe Boolean
  , create :: Maybe Boolean
  , createNew :: Maybe Boolean
  , mode :: Maybe Int
  }

defaults :: OpenOptionsData
defaults =
  { read : Nothing
  , write : Nothing
  , append : Nothing
  , truncate : Nothing
  , create : Nothing
  , createNew : Nothing
  , mode : Nothing
  }

openOptions :: OpenOptionsData -> OpenOptions
openOptions opts =
  let
    read' = maybe empty read opts.read
    write' = maybe empty write opts.write
    append' = maybe empty append_ opts.append
    truncate' = maybe empty truncate opts.truncate
    create' = maybe empty create opts.create
    createNew' = maybe empty createNew opts.createNew
    mode' = maybe empty mode opts.mode
  in
    read' <> write' <> append' <> truncate' <> create' <> createNew' <> mode'
