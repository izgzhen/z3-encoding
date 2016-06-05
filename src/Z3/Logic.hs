{-# LANGUAGE ScopedTypeVariables #-}

module Z3.Logic where

import Z3.Type

data Term = TmVar String
          | TmVal Value
          deriving (Show, Eq)

data Pred = PTrue
          | PFalse
          | PConj Pred Pred
          | PDisj Pred Pred
          | PNeg Pred
          | PForAll String Type Pred
          | PExists String Type Pred
          | PImpli Pred Pred
          | PCmp CmpOp Term Term
          | PAssert Assertion
          deriving (Show, Eq)

data Assertion = AInMap Term Term Term -- K, V, M
               | AInSet Term Term -- E, S
               deriving (Show, Eq)

data BiOp = BPlus | BMinus deriving (Show, Eq)

data CmpOp = CLess | CEq deriving (Show, Eq)
