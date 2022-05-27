{-# LANGUAGE ScopedTypeVariables #-}

module Validator (validate, Validator(Val, Result), ValidationResult(..)) where

import           Control.Exception (throwIO)
import           Control.Monad.Catch (MonadCatch, SomeException, try)

data Validator m e = Val (m Bool, e)
                   | Chain (Validator m e) (Validator m e)
                   | Result (ValidationResult e)

data ValidationResult e = Fail e
                        | Success
  deriving (Show)

instance Semigroup (Validator m e) where
  (<>) = Chain

instance Monoid (Validator m e) where
  mempty = Result Success

validate :: MonadCatch m => Validator m e -> m (ValidationResult e)
validate v = do
  r <- reduce v
  case r of
    Result result -> pure result
    comp          -> validate comp
  where
    reduce :: forall m e. MonadCatch m => Validator m e -> m (Validator m e)
    reduce r@(Result _) = pure r
    reduce (Val (computation, e)) = do
      r <- try computation :: m (Either SomeException Bool)
      case r of
        Left _      -> pure $ Result $ Fail e
        Right False -> pure $ Result $ Fail e
        Right _     -> pure $ Result Success
    reduce (Chain comp1 comp2) = do
      b <- reduce comp1
      case b of
        Result (Fail e) -> pure $ Result $ Fail e
        Result Success  -> reduce comp2
