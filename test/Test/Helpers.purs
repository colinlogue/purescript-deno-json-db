module Test.Helpers where

import Prelude

import Effect.Aff (Aff)

-- Simplified test helpers
setupTestDirectories :: Aff Unit
setupTestDirectories = pure unit

cleanupTestData :: Aff Unit
cleanupTestData = pure unit

-- Function to clean test directory - simplified version
cleanTestDir :: String -> Aff Unit
cleanTestDir path = do
  -- For tests, we'll just do nothing
  pure unit


