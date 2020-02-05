{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}

module QuantifiedSpec where

import Test.Hspec
import Test.Improvised


class Monad m => MonadFinal m where
  finally :: m a -> m b -> m a
makeImprovised ''MonadFinal


spec :: Spec
spec = describe "quantified" $ do
  it "should compile" $ do
    True `shouldBe` True

