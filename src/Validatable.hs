{-# LANGUAGE MultiParamTypeClasses #-}

module Validatable (Result(..), Validatable, validate, vall) where

-- | Result of validation.
data Result e = Fail e
              | Success
  deriving (Show)

instance Functor Result where
  fmap f Success = Success
  fmap f (Fail e) = Fail (f e)

-- | Class of things that can be validated.
class Validatable v m e where
  -- | Runs the monadic actions in the validator, returning a result.
  validate :: v m e -> m (Result e)
  -- | Builds a composite validator that returns the first argument when validated if
  --   any monadic action yields False.
  vall :: e -> [m Bool] -> v m e
