{-# LANGUAGE ScopedTypeVariables #-}

module Validator (validate, Validator(Val), Result(..), vall) where

import           Control.Monad.Catch (MonadCatch, SomeException, try)

data Validator m e = Val (m Bool) e
                   | Chain [Validator m e]

data Result e = Fail e
              | Success
  deriving (Show)

instance Semigroup (Validator m e) where
  (<>) v1 v2 = Chain [v1, v2]

instance Monoid (Validator m e) where
  mempty = Chain []

vall :: MonadCatch m => e -> [m Bool] -> Validator m e
vall e conds = Chain $ map (`Val` e) conds

-- checkGroup :: (Monoid e, MonadCatch m) => e -> Validator m e -> m (Result e)
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
