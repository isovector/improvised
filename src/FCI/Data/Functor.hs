{-# OPTIONS_GHC -ddump-splices #-}
{-# language TemplateHaskell    #-}

module FCI.Data.Functor (
    Improvised (..)
  ) where

import FCI.Internal

-------------------------------------------------------------------------------
unsafeMkInst defaultOptions ''Functor

