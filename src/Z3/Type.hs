{-# LANGUAGE UndecidableInstances #-}

module Z3.Type where

import qualified Data.Map as M
import qualified Data.Set as S

data Value = VBool Bool
           | VInt Int
           | VDouble Double
           | VMap (M.Map Value Value)
           | VSet (S.Set Value)
           | VCons String [Value]
           | VDestr String Value
           deriving (Show, Ord, Eq)

data Type = TyVar String
          | TyBool
          | TyInt
          | TyDouble
          | TyString
          | TyMap Type Type
          | TySet Type
          -- (declare-datatypes (T) ((BinTree (leaf (value T)) (node (left BinTree) (right BinTree)))))
          | TyADT String (M.Map String (M.Map String Type))
          deriving (Show, Eq)

data TS = TSInner Type
        | TSForall String TS
        deriving (Show, Eq)
