{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-float-in #-}

module FCI.Internal (
    module M
  , inst
  ) where

import Unsafe.Coerce

import FCI.Internal.Types as M
import FCI.Internal.TH    as M

-------------------------------------------------------------------------------
infixr 1 :=>

-------------------------------------------------------------------------------
-- | /Reflects/ constraint into correspoding representation - can be used to
-- access normal class instances from the environment. This function is meant
-- to be used with @TypeApplications@ when it's usage is ambiguous.
--
-- TODO: example
inst :: forall c. c => Improvised c
inst = case unsafeCoerce id :: c :=> Improvised c of Wants d -> d

-------------------------------------------------------------------------------
-- | Type of computation @a@ requiring constraint @c@ - used internally when
-- transforming between such value and it's actual representation as a
-- function.
newtype c :=> a where
  Wants :: (c => a) -> c :=> a

