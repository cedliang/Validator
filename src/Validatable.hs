{-# LANGUAGE MultiParamTypeClasses #-}

module Validatable where

data Result e = Fail e
              | Success
  deriving (Show)

instance Functor Result where
  fmap f Success = Success
  fmap f (Fail e) = Fail (f e)

class Validatable v m e where
  validate :: v m e -> m (Result e)
  vall :: e -> [m Bool] -> v m e
