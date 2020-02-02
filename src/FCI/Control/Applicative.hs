{-# language TemplateHaskell #-}

module FCI.Control.Applicative (
    Improvised (..)
  , module M
  ) where

import FCI.Data.Functor as M
import FCI.Internal

-------------------------------------------------------------------------------
unsafeMkInst defaultOptions ''Applicative

