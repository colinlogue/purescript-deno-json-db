module JsonDb.Database where

import Prelude

import Data.Argonaut (Json, JsonDecodeError)
import Data.Either (Either)
import Effect.Aff (Aff)
import JsonDatabase (Location, RetrievalError)
import JsonDatabase as JsonDatabase

type JsonDatabaseInit a =
  { encode :: a -> Json
  , decode :: Json -> Either JsonDecodeError a
  , root :: Array String
  }

type JsonDatabaseInterface a =
  { set :: Location -> a -> Aff Unit
  , get :: Location -> Aff (Either RetrievalError a)
  , update :: Location -> (a -> a) -> Aff (Either RetrievalError Unit)
  }

newtype JsonDatabase a = JsonDatabase (JsonDatabaseInterface a)

createDatabase :: forall a. JsonDatabaseInit a -> JsonDatabase a
createDatabase { encode, decode, root } = JsonDatabase
  { set: \location value -> JsonDatabase.set encode (updateLocation location) value
  , get: \location -> JsonDatabase.get decode (updateLocation location)
  , update: \location f -> JsonDatabase.update encode decode (updateLocation location) f
  }
  where
    updateLocation :: Location -> Location
    updateLocation location = location { index = root <> location.index }

-- Accessor functions
getDatabaseInterface :: forall a. JsonDatabase a -> JsonDatabaseInterface a
getDatabaseInterface (JsonDatabase interface) = interface
