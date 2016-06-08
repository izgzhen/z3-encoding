-- Demo of how to instantiate the Pred

module Z3.Demo where

import Z3.Logic
import Z3.Class
import Z3.Encoding
import Z3.Assertion
import Z3.Monad

import qualified Data.Map as M

data Term = TmVar String
          | TmApp String HeteroList Type

deriving instance Eq Term

data Type = TyBool
          | TyInt
          | TyDouble
          | TyMap Type Type
          | TySet Type
          | TyADT String

deriving instance Eq Type

type Z3Pred = Pred Term Type Assertion

instance Z3Encoded Term where
    encode (TmVar x) = do
        ctx <- getQualifierCtx
        case M.lookup x ctx of
            Just (idx, _) -> return idx
            Nothing -> smtError $ "Can't find variable " ++ x
    encode (TmApp fname args retty) = do
      retSort <- sort retty
      encodeApp fname args retSort

instance Z3Sorted Term where
    sort (TmVar x) = do
        ctx <- getQualifierCtx
        case M.lookup x ctx of
            Just (_, s) -> return s
            Nothing -> smtError $ "Can't find variable " ++ x
    sort (TmApp _ _ retty) = sort retty

instance Z3Sorted Type where
    sort TyBool     = mkBoolSort
    sort TyInt      = mkIntSort
    sort TyDouble   = mkRealSort
    sort (TyMap ty1 ty2) = do
        s1 <- sort ty1
        s2 <- sort ty2
        mkArraySort s1 s2
    sort (TySet ty) = do
        s <- sort ty
        intSort <- mkIntSort
        mkArraySort s intSort
    sort (TyADT tyName)  = do
      ctx <- getDataTypeCtx
      case M.lookup tyName ctx of
          Just s  -> return s
          Nothing -> smtError $ "Undefined type: " ++ tyName
