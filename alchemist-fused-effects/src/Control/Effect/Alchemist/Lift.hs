module Control.Effect.Alchemist.Lift
  ( new,
    run,
  )
where

import Control.Effect.Exception hiding (run)
import Alchemist.Experiment
import Data.Text (Text)

new ::
  (Eq a, Has (Lift IO) sig m) =>
  Text ->
  IO a ->
  Experiment m SomeException a
new n c =
  Experiment
    { enabled = pure True,
      control = sendM c,
      candidates = [],
      raised = const throwIO,
      name = n,
      comparator = \x y -> pure (x == y)
    }

-- Note that this function says nothing about any @Error@ constraints;
-- any errors thrown with 'throwError' will bubble up to any enclosing
-- scope.
run ::
  (Has (Lift IO) sig m) =>
  Experiment m e a ->
  m a
run = undefined
