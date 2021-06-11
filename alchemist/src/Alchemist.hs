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
monadic computation whose result should be equal to that of running the control. Finally, you run this experiment
in your chosen monad, which executes both the control action and zero or more of the associated candidates.
Running an experiment returns the control value so that downstream code can act on it, but also returns information
about which experiments were run, the time it took to execute them, and whether their values were equivalent
to the control value. An optional user-defined reporting function can send information about failed experiments
to a logging or telemetry service.

== Simple example

> import Alchemist.IO
>
> experiment (print "control running")
>   & withCandidate (print "candidate 1")
>   & withCandidate (print "candidate 2")
>   & run

The above code will print @"control running"@, and then zero, one, or both of the candidate actions.

== A more complicated example

> import Alchemist.IO
> import Database.SQLite.Simple
>
> let conn :: Connection = ...
> rowCount <- experiment (length <$> query_ conn "SELECT * FROM users")
>   & withCandidate "faster" (fromOnly <$> query_ conn "SELECT COUNT(*) FROM users")
>   & run

In this example, we have a known-good behavior (extracting all users from a database, then computing)
the length of that list), and a candidate that should be faster (unless our database is hideously broken).
The @rowCount@ variable will be that returned by the action passed to @experiment@; as such, you can add
and remove experiments with reasonable confidence you won't affect semantics of downstream code (see the
section on exception semantics below).

@alchemist@ is useful when you have Extremely Important Code Paths that Absolutely Should Not Break In
Production. Though Haskell's type system catches many errors that other languages do not, types can't catch
all errors, particularly the dynamic errors that GHC can throw even in pure code. By slowly factoring out your
critical sections into @alchemist@-compatible blocks, you have an additional layer of confidence during the
refactoring and testing process.

= How to use it

If you're working in 'IO', import 'Alchemist.IO'. If you're working in a monad that can catch
GHC's 'Control.Exception' hierarchy, import 'Alchemist.Catch'. Further packages will exist that
provide effectful interfaces when catching error values with @mtl@ or @fused-effects@.

The @experiment@ functions in the aforementioned modules create a new 'Experiment' value with sensible
defaults filled in. You can then use the combinators in "Control.Experiment" to populate the experiment with
candidates. At the end, a @run@ function will execute the control and some (possibly empty) subset of its
candidates. You can also import "Control.Experiment" directly and populate its record fields yourself, should
your monad be complicated or you prefer the record-of-functions approach.

= Exception semantics

Part of @alchemist@'s responsibilities is handling and and reporting errors that occur in candidates.
However, Haskell programs handle errors in diverse ways: pure code can use 'Either' or
'Control.Exception.throw', 'IO'-based code can use 'Control.Exception.throwIO', code that uses
the built-in @exceptions@ library can use 'Control.Monad.Catch.throwM', and effect systems can use
constructs like @MonadError@ or a @Throw@ effect. The 'Experiment' type aims to be generalizable
to all these situations; all that is required is to define appropriate 'attempt' and 'report' functions.

Errors encountered when running an experiment's 'Alchemist.Experiment.control' are thrown, as this indicates a
problem in your existing code that you should address before running any experiments. Furthermore, errors not
captured by 'attempt' will be bubbled up to a caller. If you're in 'IO' and you want to catch /all/ possible
exceptions that the Haskell runtime could throw, specialize your 'Experiment''s exception type to
'Control.Exception.SomeException'. If you're within an error monad, you can catch the associated error
type. If you're in a monad with both exception-handling and error-handling semantics (such as one implementing
'MonadCatch' and 'MonadError', choose your desired semantics carefully, and consider running in
'IO' with 'liftIO' or refactoring your monad stack to use either errors or exceptions but not both.

-}

module Alchemist
  ( module X
  ) where

import Alchemist.IO as X
