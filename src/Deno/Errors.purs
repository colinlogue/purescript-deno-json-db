module Deno.Errors where

import Prelude

import Effect.Exception (Error)


foreign import isNotFoundError :: Error -> Boolean