# alchemist

[![GitHub CI](https://github.com/patrickt/alchemist/workflows/CI/badge.svg)](https://github.com/patrickt/alchemist/actions)
[![Hackage](https://img.shields.io/hackage/v/alchemist.svg?logo=haskell)](https://hackage.haskell.org/package/alchemist)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

`alchemist` is library for refactoring critical paths. You build up `Experiment` values with a DSL, starting with a reference behavior, called here the _control_, that represents the existing behavior of your app. From then on, you add one or more `candidate` computations to this `Experiment`, each candidate representing a monadic computation whose result should be equal to that of running the control. Each time an `Experiment` is invoked, it logs the results of running all the candidate computations to some user-defined portal downstream.

`alchemist` is useful when you have Extremely Important Code Paths that Absolutely Should Not Break In Production. Though Haskell's type system catches many errors that other languages do not, types can't catch all errors, particularly the dynamic errors that GHC can throw even in pure code. By slowly factoring out your critical sections into `alchemist`-compatible blocks, you have an additional layer of confidence during the refactoring and testing process.

This is a clone of Ruby's [`scientist`](http://github.com/github/scientist).

Example:

``` haskell
import Alchemist.IO as Alc

main = do
  let myOldFunction = someExistingFunction
  let myNewFunction = newFunctionToTest
  -- Both myOldFunction and myNewFunction are run,
  -- their outputs are compared, and information is logged.
  result <-
    Alc.new "example" myOldFunction
      & Alc.try myNewFunction
      & Alc.run
```

## To-dos

Write `alchemist-mtl`.

## Thanks to

The `scientist` authors, and the legendary producer of the same name.
