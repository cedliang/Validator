module MValidator (MValidator(MVal, MGroup)) where

import           Control.Monad.Catch (MonadCatch, SomeException, try)
import           Validatable

-- | Validator where the fail type e is a Monoid. The MGroup constructor's first argument
--   is front-mappended to the result of the associated MValidator if it fails.
--
--   Allows for the construction of nested validation stacks with accumulation of the monoid value
--   in the event of failure.
data MValidator m e = MVal (m Bool) e
                    | MChain [MValidator m e]
                    | MGroup e (MValidator m e)

instance Semigroup (MValidator m e) where
  (<>) v1 v2 = MChain [v1, v2]

instance Monoid (MValidator m e) where
  mempty = MChain []

instance (Monoid e, MonadCatch m) => Validatable MValidator m e where
  validate (MVal computation e) = do
    r <- try computation :: m (Either SomeException Bool)
    case r of
      Left _      -> pure $ Fail e
      Right False -> pure $ Fail e
      Right _     -> pure Success
  validate (MChain []) = pure Success
  validate (MChain (c:cs)) = do
    b <- validate c
    case b of
      Success -> validate $ MChain cs
      f       -> pure f
  validate (MGroup e v) = do
    r <- validate v
    case r of
      Success -> pure Success
      Fail ie -> pure $ Fail $ e <> ie

  veach ls = mconcat $ map (uncurry MVal) ls