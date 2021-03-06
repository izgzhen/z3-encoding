z3-encoding
=====

[![Join the chat at https://gitter.im/izgzhen/z3-encoding](https://badges.gitter.im/izgzhen/z3-encoding.svg)](https://gitter.im/izgzhen/z3-encoding?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[![Build Status](https://travis-ci.org/izgzhen/z3-encoding.svg?branch=master)](https://travis-ci.org/izgzhen/z3-encoding)

Assertion language embedded in Haskell, based on [Z3 solver](http://research.microsoft.com/en-us/um/redmond/projects/z3/).

## Features
* [x] Primitive types: boolean, integer, double precision float number
* [x] Complex types: set, <strike>map, ADT</strike>
* [x] Logic primitives and connectives: true, false, conjunction, disjunction, negation, implication
* [x] Logic qualifiers: universal, existential
* [x] Assertions for primitive types: equality, less than
* [x] Assertions for complex types: membership testing
* [ ] Extensible function
* [ ] Extensible assertion
* [x] Static type safety

## Usage
1. Install `z3`, noting its `include` path and `lib` path as specified by `prefix=`
3. `git clone https://github.com/izgzhen/z3-encoding`
4. Adapt `z3-encoding/stack.yaml` to your specific condition, esp.:
    * `extra-include-dirs`
    * `extra-lib-dirs`

## Upstream
Currently, it supports [z3 v4.4.1](https://github.com/Z3Prover/z3/releases), through a low-level Haskell library [z3-haskell](https://hackage.haskell.org/package/z3).

Also, current version of this package supports the GHC v8.0.1.
