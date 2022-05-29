{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Validator (Validator(Val)) where

import           Control.Monad.Catch (MonadCatch, SomeException, try)
import           Validatable

data Validator m e = Val (m Bool) e
                   | Chain [Validator m e]

instance Semigroup (Validator m e) where
  (<>) v1 v2 = Chain [v1, v2]

instance Monoid (Validator m e) where
  mempty = Chain []

instance (MonadCatch m) => Validatable Validator m e where
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

  vall e conds = mconcat $ map (`Val` e) conds

