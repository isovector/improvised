{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}

module CollectionSpec where

import Data.Functor.Identity
import Test.Hspec
import Test.Improvised


class Monad m => MonadFoo m where
  foo :: m Int
makeImprovised ''MonadFoo


class Monad m => MonadBar m where
  bar :: Int -> m Int -> m Int
  baz :: m Int
makeImprovised ''MonadBar

data AppDicts m =
  AppDicts
    (Improvised (MonadFoo m))
    (Improvised (MonadBar m))
makeImprovCollection ''AppDicts


myBusinessLogic :: (MonadFoo m, MonadBar m) => m Int
myBusinessLogic =
  bar 13 $ do
    x <- foo
    y <- baz
    pure $ x + y

tested :: forall m. Monad m => m Int
tested = improvise mocks myBusinessLogic
  where
    mocks :: AppDicts m
    mocks =
      AppDicts
        (MonadFoo (pure 5))
        (MonadBar (\n -> fmap (+ n)) (pure 7))


spec :: Spec
spec = describe "makeImprovCollection" $
  it "allows for capture of multiple constraints" $ do
    runIdentity tested `shouldBe` 25

