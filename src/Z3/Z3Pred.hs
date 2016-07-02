-- Sealed layer for out-of-box use
{-# LANGUAGE StandaloneDeriving #-}

module Z3.Z3Pred where

import Z3.Base.Logic
import Z3.Base.Class
import Z3.Base.Encoding
import Z3.Base.Atom
import Z3.Monad

import qualified Data.Map as M

data Term = TmVar   String
          | TmNum   Int
          | TmBool  Bool
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
          | TmApp   String HeteroList Type

deriving instance Eq Term

data Type = TyBool
          | TyInt
          | TyDouble
          | TyMap Type Type
          | TySet Type
          | TyADT String

deriving instance Eq Type

type Z3Pred = Pred Term Type ()

instance Z3Encoded Term where
    encode (TmVar x) = do
        ctx <- getValBindCtx
        case M.lookup x ctx of
            Just (idx, _) -> return idx
            Nothing -> smtError $ "Can't find variable " ++ x
    encode (TmNum n) = mkIntSort >>= mkInt n
    encode (TmBool b) = mkBool b
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
    encode (TmApp fname args retty) = do
      retSort <- sortOf retty
      encodeApp fname args retSort

instance Z3Sorted Term where
    sortOf (TmVar x) = do
        ctx <- getValBindCtx
        case M.lookup x ctx of
            Just (_, s) -> return s
            Nothing -> smtError $ "Can't find variable " ++ x
    sortOf (TmNum _) = mkIntSort
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
    sortOf (TmApp _ _ retty) = sortOf retty

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
    sortOf (TyADT tyName)  = do
      ctx <- getDataTypeCtx
      case M.lookup tyName ctx of
          Just s  -> return s
          Nothing -> smtError $ "Undefined type: " ++ tyName

instance Z3Encoded () where
