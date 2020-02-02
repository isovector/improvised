{-# language TemplateHaskell #-}

module FCI.Control.Monad (
    Improvised (..)
  , module M
  ) where


import FCI.Control.Applicative as M
import FCI.Internal

-------------------------------------------------------------------------------
unsafeMkInst defaultOptions ''Monad

