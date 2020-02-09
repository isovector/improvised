{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module Test.Improvised.Internal where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Data.Coerce
import Type.Errors

------------------------------------------------------------------------------
-- | Internal type family for providing helpful error messages if you attempt
-- to call 'improvise' without first calling
-- 'Test.Improvised.makeImprovCollection'.
--
-- Instances of this family should always be the unit type.
type family IsImprovCollection dict


------------------------------------------------------------------------------
-- | Internal type family for providing helpful error messages if you attempt
-- to call 'improvise' without first calling
-- 'Test.Improvised.makeImprovCollection'.
--
-- Given the type @a b c@, this family expand to @''ShowType' a@
type family ShowTypeAppHead (t :: k) :: ErrorMessage where
  ShowTypeAppHead (t a) = ShowTypeAppHead t
  ShowTypeAppHead t     = 'ShowType t


newtype Improvisable dict m a = Improvisable (ReaderT dict m a)
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans (Improvisable dict) where
  lift = Improvisable . lift
  {-# INLINABLE lift #-}


improvise :: dict -> Improvisable dict m a -> m a
improvise dict (Improvisable r) = runReaderT r dict
{-# INLINABLE improvise #-}


coerceImprovisable :: Improvisable dict m a -> dict -> m a
coerceImprovisable = coerce
{-# INLINABLE coerceImprovisable #-}

