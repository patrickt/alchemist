{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}

{-|

Module      : Alchemist
Copyright   : (c) Patrick Thomson, 2021
License     : MIT
Maintainer  : Patrick Thomson
Stability   : experimental
Portability : POSIX

@alchemist@ is a library for refactoring critical paths. You build up 'Experiment' values with a DSL, starting
with a reference behavior, called the /control/, that represents the existing behavior of your app. From
then on, you add one or more /candidate/ computations to this 'Experiment', each candidate representing a
monadic computation whose result should be equal to that of running the control. Each time an 'Experiment' is
invoked, it logs the results of running all the candidate computations to some user-defined portal downstream.

@alchemist@ is useful when you have Extremely Important Code Paths that Absolutely Should Not Break In
Production. Though Haskell's type system catches many errors that other languages do not, types can't catch
all errors, particularly the dynamic errors that GHC can throw even in pure code. By slowly factoring out your
critical sections into @alchemist@-compatible blocks, you have an additional layer of confidence during the
refactoring and testing process.

= Exception semantics

Part of @alchemist@'s responsibilities is handling and and reporting errors that occur in candidates.
However, Haskell programs handle errors in diverse ways: pure code can use 'Either' or
'Control.Exception.throw', 'IO'-based code can use 'Control.Exception.throwIO', code that uses
the built-in @exceptions@ library can use 'Control.Monad.Catch.throwM', and effect systems can use
constructs like @MonadError@ or a @Throw@ effect. The 'Experiment' type aims to be generalizable
to all these situations. However, there are certain important facts to remember:

1. Errors encountered when running an experiment's 'Alchemist.Experiment.control' are thrown, as this indicates
   a problem in your existing code that you should address before running any experiments.
2. Consider carefully your desired behavior when using both error types (like 'Maybe' or 'Either' 'String') and exception types (of the sort thrown by 'Control.Exception.throw' or 'Control.Monad.Catch.throwM'). You can always override the function used to catch errors by assigning

-}
module Alchemist
  ( Experiment (..)
  , candidate
  ) where

import Alchemist.Experiment
