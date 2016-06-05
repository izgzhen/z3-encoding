{-# LANGUAGE ScopedTypeVariables #-}

module Z3.Spec where

import Z3.Type
import Common

data Term = TmVar Name
          | TmVal Value
          deriving (Show, Eq)

data Pred = PTrue
          | PFalse
          | PConj Pred Pred
          | PDisj Pred Pred
          | PNeg Pred
          | PForAll Name Type Pred
          | PExists Name Type Pred
          | PImpli Pred Pred
          | PCmp CmpOp Term Term
          | PAssert Assertion
          deriving (Show, Eq)

data Assertion = AInMap Term Term Term -- K, V, M
               deriving (Show, Eq)

data BiOp = BPlus | BMinus deriving (Show, Eq)

data CmpOp = CLess | CEq deriving (Show, Eq)
