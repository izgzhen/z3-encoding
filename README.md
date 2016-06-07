z3-encoding
=====

[![Build Status](https://travis-ci.org/izgzhen/z3-encoding.svg?branch=master)](https://travis-ci.org/izgzhen/z3-encoding)

A library targeting at providing high-level, extensible, easy to use Haskell interface to [Z3 solver](http://research.microsoft.com/en-us/um/redmond/projects/z3/).

## Features
* [x] Primitive types: boolean, integer, double precision float number
* [x] Complex types: map, set, ADT
* [x] Type inference
* [x] Logic primitives and connectives: true, false, conjunction, disjunction, negation, implication
* [x] Logic qualifiers: universal, existential
* [x] Built-in assertions for primitive types: equality, less than 
* [x] Built-in assertions for complex types
* [ ] Extensible function
* [ ] Extensible assertion

And all above are provided in a declarative, composable style! See `Z3.Type` and `Z3.Logic` for details.

## Usage
1. Install `z3`, noting its `include` path and `lib` path
2. `git clone https://github.com/izgzhen/z3-haskell`
3. `git clone https://github.com/izgzhen/z3-encoding`
4. Adapt `z3-encoding/stack.yaml` to your specific condition, esp.:
    * `packages`
    * `extra-include-dirs`
    * `extra-lib-dirs`

## Upstream
Currently, it supports [z3 v4.4.1](https://github.com/Z3Prover/z3), which is bound through a low-level Haskell library [z3-haskell](https://hackage.haskell.org/package/z3). z3-haskell is not updated to be compatible with the newest z3 release, so you might need to use [my own fork](https://github.com/izgzhen/z3-haskell) to make things work.

Also, current version of this package supports the newest GHC (8.0.1).

