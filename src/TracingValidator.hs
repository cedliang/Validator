{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TracingValidator (validate, Validator(Val), Result(..)) where

import           Control.Monad.Catch (MonadCatch, SomeException, try)

data Result e = Fail e
              | Success
  deriving (Show)

class Validatable v m e where
  validate :: v m e -> m (Result e)

data Validator m e = Val (m Bool) e
                   | Chain [Validator m e]

instance MonadCatch m => Validatable Validator m e where
  validate :: forall m e. MonadCatch m => Validator m e -> m (Result e)
  validate (Val computation e) = do
    r <- try computation :: m (Either SomeException Bool)
    case r of
      Left _      -> pure $ Fail e
      Right False -> pure $ Fail e
      Right _     -> pure Success
  validate (Chain []) = pure Success
  validate (Chain (c:cs)) = do
    b <- validate c
    case b of
      Success -> validate $ Chain cs
      f       -> pure f

instance Semigroup (Validator m e) where
  (<>) v1 v2 = Chain [v1, v2]

instance Monoid (Validator m e) where
  mempty = Chain []

data ValGroup m e = ValGroup e [Validator m e]
                  | ChainGroup [ValGroup m e]

instance (MonadCatch m, Monoid e) => Validatable ValGroup m e where
  validate
    :: forall m e. (MonadCatch m, Monoid e) => ValGroup m e -> m (Result e)
  validate (ValGroup e v) = do
    r <- validate $ mconcat v
    case r of
      Success -> pure Success
      Fail ie -> pure $ Fail $ e <> ie
  validate (ChainGroup []) = pure Success
  validate (ChainGroup (c:cs)) = do
    b <- validate c
    case b of
      Success -> validate $ ChainGroup cs
      Fail e  -> pure $ Fail e

instance Semigroup (ValGroup m e) where
  (<>) vg1 vg2 = ChainGroup [vg1, vg2]

instance Monoid (ValGroup m e) where
  mempty = ChainGroup []
