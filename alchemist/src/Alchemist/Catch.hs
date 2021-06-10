{-# LANGUAGE ImportQualifiedPost #-}

module Alchemist.Catch
  ( new,
  )
where

import Alchemist.Experiment
import Control.Monad.Catch qualified as Exc
import Data.Text (Text)
import Data.Monoid

-- Note that if you use this in conjunction with @MonadError@,
-- any errors thrown by @throwError@ will bubble up to their enclosing
-- scope rather than being caught by a @run@ function. This interface only
-- catches GHC's exceptions hierarchy. For an interface that uses @MonadError@,
-- consult @alchemist-mtl@'s @Control.Monad.Error.Alchemist@.
new :: (Exc.MonadCatch m, Exc.Exception e, Eq a) => Text -> m a -> Experiment m e a
new n c =
  Experiment
    { enabled = pure True,
      control = c,
      candidates = [],
      attempt = Exc.try,
      report = getAp <$> mempty,
      name = n,
      comparator = \x y -> pure (x == y)
    }
