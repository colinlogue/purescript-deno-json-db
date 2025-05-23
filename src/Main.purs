module Main where

import Prelude

import Data.Argonaut (Json)
import Data.Either (Either(..))
import Effect (Effect)
import JsonDatabase.Server (JsonServerInit, startJsonDatabaseServer)


main :: Effect Unit
main = void $ startJsonDatabaseServer init
  where
    init :: JsonServerInit Json
    init =
      { encode: identity
      , decode: Right
      , root: []
      , port: 1248
      }