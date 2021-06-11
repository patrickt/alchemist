{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}

module Alchemist.Candidate (Candidate (..)) where

import Data.Kind (Type)
import Data.Text (Text)

-- | A 'Candidate' wraps a monadic action and an identificatory name.
data Candidate (m :: Type -> Type) a = Candidate
  { action :: m a,
    name :: Text
  }
  deriving stock (Functor)
