module Control.Effect.Alchemist.Error
  ( new
  ) where

import Control.Effect.Error
import Alchemist.Experiment
import Data.Text (Text)

-- | Creates an 'Experiment' suitable for running an action in 'IO'.
new ::
  (Eq a, Has (Error e) sig m) =>
  -- | the name of this experiment
  Text ->
  -- | the control (default) action to run
  m a ->
  Experiment m e a
new n c =
  Experiment
    { enabled = pure True,
      control = c,
      candidates = [],
      raised = const throwError,
      name = n,
      comparator = \x y -> pure (x == y)
    }
