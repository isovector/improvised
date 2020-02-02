{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}

module MockSpec where

import Control.Monad.Trans.Writer
import Test.Hspec
import Test.Improvised


------------------------------------------------------------------------------
-- | MonadFoo lets you foo
class MonadFoo m where
  foo :: m Int
  faz :: Int -> m ()
makeImprovised ''MonadFoo


data Action s = Faz s
  deriving (Show, Eq)


test :: Improvised (MonadFoo (Writer [Action Int]))
test =
  MonadFoo
    { _faz = tell . pure . Faz
    , _foo = pure 5
    }


ok :: [Action Int]
ok = snd . runWriter . improvise test $ do
  s <- foo
  faz s
  faz 10


spec :: Spec
spec = describe "mocking" $ do
  it "should allow us to write into a Writer target to log what actions occurred" $ do
    ok `shouldBe` [Faz 5, Faz 10]

