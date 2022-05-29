-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Cedric Liang 2022
--
-- License     : BSD-style
--
-- Stability   : experimental
--
-- Sequential validators on monadic actions that support early termination.
-- Simpler to work with than a transformer or MonadPlus approach.
--
-------------------------------------------------------------------------------------
module Validator
    ( Validator(Val)
    , MValidator(MVal, MGroup)
    , Validatable
    , Result(..)
    , validate
    , vall
    , veach) where

import           Control.Monad.Catch (MonadCatch, SomeException, try)
import           MValidator (MValidator(MGroup, MVal))
import           Validatable (Result(..), Validatable, validate, vall, veach)

-- | Basic sequential validator consisting of monadic computations that return Boolean.
--   Validators are monoids and can be composed.
--
--   Short circuits in the event of failure, ensuring subsequent computations are not run.
--
--   There is no restriction on the return type associated with failure, but also no accumulation of
--   failure contexts. For a version that does accumulate along its call stack (ie, for tracing),
--   use MValidator.
data Validator m e = Val (m Bool) e
                   | Chain [Validator m e]

instance Semigroup (Validator m e) where
  (<>) v1 v2 = Chain [v1, v2]

instance Monoid (Validator m e) where
  mempty = Chain []

instance (Monad m) => Validatable Validator m e where
  validate (Val computation e) = do
    r <- computation
    (if r
     then pure Success
     else pure $ Fail e)
  validate (Chain []) = pure Success
  validate (Chain (c:cs)) = do
    b <- validate c
    case b of
      Success -> validate $ Chain cs
      f       -> pure f

  veach ls = mconcat $ map (uncurry Val) ls
