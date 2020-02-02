{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoPolyKinds            #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -ddump-splices      #-}

module WeirdTypesSpec where

import Test.Hspec
import Test.Improvised
import Data.Kind

class Empty (m :: Type -> Type) where
makeImprovised ''Empty

class Monad m => Superclass m where
makeImprovised ''Superclass

class Params (a :: Type) (b :: Type) (c :: Type) (d :: Type) (m :: Type -> Type) where
makeImprovised ''Params

class InferredKinds a b c d (m :: Type -> Type) where
  inferredKinds :: m (a b (b c), b d, d)
makeImprovised ''InferredKinds

-- -- Doesn't work; mentions an explicit kind in the produced code but doesn't
-- -- bind it
-- class ImplicitKind a (m :: Type -> Type) where
-- makeImprovised ''ImplicitKind

class (Eq a, Ord b) => MultiSuperclasses a b (m :: Type -> Type) where
makeImprovised ''MultiSuperclasses

class Fundeps a b (m :: Type -> Type) | m -> a b where
  fundeps :: m (a, b)
makeImprovised ''Fundeps

-- -- Doesn't work, fails to propagate the ambiguous tyvar when lifting the
-- -- instance
-- class Ambiguous (m :: Type -> Type) where
--   ambiguous :: a
-- makeImprovised ''Ambiguous

-- -- Doesn't work --- fails to propagate the kind sig on 'a' when binding vars
-- class TypeInType (k :: Type) (a :: k) (m :: Type -> Type) where
-- makeImprovised ''TypeInType


spec :: Spec
spec = describe "weird types" $ it "should compile" $ True `shouldBe` True

