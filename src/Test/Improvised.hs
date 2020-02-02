{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module Test.Improvised
  ( Improvisable ()
  , improvise
  -- , runImprovisable
  , Improvised
  -- , CaptureInst ()
  , makeImprovised
  , makeImprovCollection
  ) where

import FCI.Internal
import Test.Improvised.Internal
import Test.Improvised.TH

