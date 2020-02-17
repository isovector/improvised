{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -ddump-splices      #-}

module Test where

import Test.Improvised
import Test.Improvised.Internal
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.Morph

class Monad m => Foo m where
  foo :: m (WriterT [Int] m String)

makeImprovised ''Foo

-- newtype instance Improvised (Foo (m_atgM))
--   = FooImpl {__foo :: (m_atgM (WriterT [Int] m_atgM String))}

-- class HasFoo m_atgM env_atmo | env_atmo -> m_atgM where
--   getFooImprovised :: env_atmo -> Improvised (Foo m_atgM)

-- instance HasFoo m_atgM (Improvised (Foo m_atgM)) where
--   {-# INLINABLE getFooImprovised #-}
--   getFooImprovised = id
-- type instance IsImprovCollection (Improvised (Foo m_atgM)) = ()

-- instance (Monad m_atgM, HasFoo m_atgM dict_atmp) =>
--          Foo (Improvisable dict_atmp m_atgM) where
--   foo = Improvisable $ ReaderT $ \r -> fmap (hoist lift) $ __foo (getFooImprovised r)
--     -- r <- lift $ Improvisable ask
--     -- __foo (getFooImprovised r)
--   --   hoist lift r


-- instance

-- makeImprovised ''Foo

-- one case
--   foo = do
--     dict <- lift $ Improvisable $ asks getFooImprovised
--     let r = __foo dict
--     hoist lift r
