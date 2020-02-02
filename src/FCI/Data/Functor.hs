{-# language TemplateHaskell #-}

module FCI.Data.Functor (
    Dict (..)
  , fmapFunctor
  , coerceFunctor
  ) where

import Data.Coerce

import FCI.Internal

-------------------------------------------------------------------------------
unsafeMkInst defaultOptions ''Functor

-------------------------------------------------------------------------------
-- | Creates 'Functor' instance from mapping function.
fmapFunctor :: (forall a b. (a -> b) -> f a -> f b) -> Dict (Functor f)
fmapFunctor _fmap = Functor{
    _fmap
  , (|<$) = _fmap . const
  }

-------------------------------------------------------------------------------
-- | Creates 'Functor' instace for any type that can be "'coerce'd out".
coerceFunctor :: forall f. Newtype f => Dict (Functor f)
coerceFunctor = Functor{
    _fmap = coerce
  , (|<$) = (coerce :: (a -> b -> a) -> a -> f b -> f a) const
  }
