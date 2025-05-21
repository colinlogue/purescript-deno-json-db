module Deno.Errors where

import Effect.Exception (Error)


foreign import isNotFoundError :: Error -> Boolean