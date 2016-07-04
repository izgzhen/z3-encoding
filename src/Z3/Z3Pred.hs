{-
  Sealed AST for out-of-box use

  The design choice is to fix all possible *primitive*
  types as an AST, so a sound type checking is possible here.

  Question: How to handle TmApp (which is only instantiated as
  constructors) and potential user functions here? Do we have to
  provide an extra field or user can make up a Encoded Expr which
  layers on top of Term here (while preserving (expected) type soundness
  provided in this module)?

-}
{-# LANGUAGE StandaloneDeriving #-}

module Z3.Z3Pred where

import Z3.Base.Logic
import Z3.Base.Class
import Z3.Base.Atom
import Z3.Context
import Z3.Datatypes
import Z3.Monad

data Term = -- XXX: Verify that TmVar can serve at the same time as
            --      qualified var, bounded var, and zero-arity constructor
            TmVar   String
          | TmInt   Int
          | TmDouble Double
          | TmBool  Bool
          | TmString String
          | TmLE    Term Term
          | TmGE    Term Term
          | TmSub   Term Term
          | TmAdd   Term Term
          | TmMul   Term Term
          | TmDiv   Term Term
          | TmMod   Term Term
          | TmRem   Term Term
          | TmMinus Term
          | TmIf    Term Term Term
          | TmCons String HeteroList Type

deriving instance Eq Term

data Type = TyBool
          | TyInt
          | TyDouble
          | TyString
          | TyMap Type Type
          | TySet Type
          | TyADT String

deriving instance Eq Type

type Z3Pred tm = Pred tm Type ()

instance Z3Encoded Term where
    encode (TmVar x) = fst <$> getValBind x
    encode (TmInt n) = encode n
    encode (TmBool b) = encode b
    encode (TmLE t1 t2) = encode (LessE t1 t2)
    encode (TmGE t1 t2) = encode (GreaterE t1 t2)
    encode (TmAdd t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkAdd [a1, a2]
    encode (TmSub t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkSub [a1, a2]
    encode (TmMul t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkMul [a1, a2]
    encode (TmDiv t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkDiv a1 a2
    encode (TmMod t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkMod a1 a2
    encode (TmRem t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkRem a1 a2
    encode (TmMinus t) = do
        a <- encode t
        mkUnaryMinus a
    encode (TmIf p c a) = do
        a1 <- encode p
        a2 <- encode c
        a3 <- encode a
        mkIte a1 a2 a3
    encode (TmCons fname args retty) = do
      retSort <- sortOf retty
      encodeCons fname args retSort

-- XXX: Maybe if we have type checking this can be neater
instance Z3Sorted Term where
    sortOf (TmVar x) = snd <$> getValBind x
    sortOf (TmInt _) = mkIntSort
    sortOf (TmBool _) = mkBoolSort
    sortOf (TmLE _ _) = mkBoolSort
    sortOf (TmGE _ _) = mkBoolSort
    sortOf (TmAdd _ _) = mkIntSort
    sortOf (TmSub _ _) = mkIntSort
    sortOf (TmMul _ _) = mkIntSort
    sortOf (TmDiv _ _) = mkIntSort
    sortOf (TmMod _ _) = mkIntSort
    sortOf (TmRem _ _) = mkIntSort
    sortOf (TmMinus _) = mkIntSort
    sortOf (TmIf _ c _) = sortOf c
    sortOf (TmCons _ _ retty) = sortOf retty

instance Z3Sorted Type where
    sortOf TyBool     = mkBoolSort
    sortOf TyInt      = mkIntSort
    sortOf TyDouble   = mkRealSort
    sortOf (TyMap ty1 ty2) = do
        s1 <- sortOf ty1
        s2 <- sortOf ty2
        mkArraySort s1 s2
    sortOf (TySet ty) = do
        s <- sortOf ty
        intSort <- mkIntSort
        mkArraySort s intSort
    sortOf (TyADT tyName) = getDataType tyName

instance Z3Encoded () where

checkPre :: Z3Sorted tm => Z3Pred tm -> Z3SMT e (Result, Maybe Model)
checkPre pre = local $ do
    ast <- encode pre
    local (assert ast >> getModel)
