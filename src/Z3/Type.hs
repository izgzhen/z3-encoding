{-# LANGUAGE UndecidableInstances #-}

module Z3.Type where

import Z3.Logic
import qualified Data.Map as M
import qualified Data.Set as S

data Value = VBool Bool
           | VInt Int
           | VDouble Double
           | VMap (M.Map Value Value)
           | VSet (S.Set Value)
           deriving (Show, Ord, Eq)

data Term = TmVar String
          | TmVal Value
          | TmApp String [Term]
          deriving (Show, Eq)

data Type = TyVar String
          | TyBool
          | TyInt
          | TyDouble
          | TyMap Type Type
          | TySet Type
          | TyApp Type Type
          | TyADT String
          deriving (Show, Eq)

data TS = TSInner Type
        | TSForall String TS
        deriving (Show, Eq)

data Assertion = AInMap Term Term Term -- K, V, M
               | AInSet Term Term -- E, S
               | AEq Term Term
               | ALess Term Term
               deriving (Show, Eq)

type Z3Pred = Pred Term Type Assertion
