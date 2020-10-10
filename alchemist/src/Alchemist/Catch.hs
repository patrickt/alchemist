{-# LANGUAGE ImportQualifiedPost #-}

module Alchemist.Catch
  ( new,
  )
where

import Alchemist.Experiment
import Control.Monad.Catch qualified as Exc
import Data.Text (Text)

new :: (Exc.MonadCatch m) => Text -> m a -> Experiment m a
new n c =
  Experiment
    { enabled = pure True,
      control = c,
      candidates = []
      raised = const Exc.throwM,
      name = n,
      comparator = (==),
      publish = const (pure ())
    }
