module Control.Effect.Alchemist.Error
  ( experiment
  ) where

import Control.Effect.Error
import Alchemist.Experiment
import Data.Text (Text)
import Data.Monoid

-- | Creates an 'Experiment' suitable for running an action in 'IO'.
experiment ::
  (Eq a, Has (Error e) sig m) =>
  -- | the name of this experiment
  Text ->
  -- | the control (default) action to run
  m a ->
  Experiment m e a
experiment n c =
  Experiment
    { enabled = pure True,
      control = c,
      candidates = [],
      attempt = \a -> catchError (Right `fmap` a) (return . Left),
      name = n,
      report = getAp <$> mempty
    }
