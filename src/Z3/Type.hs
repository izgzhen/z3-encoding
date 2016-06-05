{-# LANGUAGE UndecidableInstances #-}

module Z3.Type where

import Common

import qualified Data.Map as M
import qualified Data.Set as S

data Value = VBool Bool
           | VInt Int
           | VDouble Double
           | VMap (M.Map Value Value)
           -- | VSet (S.Set Value)
           -- | VADT Name [Value]
           deriving (Show, Eq)

data Type = TyVar String
          | TyBool
          | TyInt
          | TyDouble
          | TyString
          | TyMap Type Type
          -- | TySet Type
          -- | TyADT Name (M.Map Name [Type])
          deriving (Show, Eq)

data TS = TSInner Type
        | TSForall String TS
        deriving (Show, Eq)
