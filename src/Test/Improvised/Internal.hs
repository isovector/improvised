{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module Test.Improvised.Internal where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import FCI.Internal
import Data.Coerce


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

