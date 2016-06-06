{-# LANGUAGE UndecidableInstances #-}

module Z3.Type where

import Z3.Monad
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
          | TyString
          | TyMap Type Type
          | TySet Type
          | TyApp Type Type
          -- (declare-datatypes (T) ((BinTree (leaf (value T)) (node (left BinTree) (right BinTree)))))
          | TyADT Sort
          deriving (Show, Eq)

data TS = TSInner Type
        | TSForall String TS
        deriving (Show, Eq)
