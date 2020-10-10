module Control.Effect.Alchemist.Lift
  ( new,
  )
where

import Control.Effect.Exception

new ::
  (Eq a, Has (Lift IO) sig m) =>
  Text ->
  IO a ->
  Experiment m a
new n c =
  Experiment
    { enabled = pure True,
      control = liftIO c,
      candidates = [],
      raised = const Eff.throwIO,
      comparator = (==),
      publish = const (pure ())
    }

-- Note that this function says nothing about any @Error@ constraints;
-- any errors thrown with 'throwError' will bubble outward of
run ::
  (Has (Lift IO) sig m) =>
  Experiment m a ->
  m a
run = undefined
