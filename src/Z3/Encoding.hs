{-# LANGUAGE LambdaCase #-}

module Z3.Encoding where

import Z3.Context
import Z3.Logic
import Z3.Type
import Z3.Infer
import Z3.Monad hiding (mkMap)

import Control.Monad.State
import Control.Monad.Except (throwError)
import qualified Data.Map as M

mkAST :: Pred -> SMT AST
mkAST PTrue = mkTrue
mkAST PFalse = mkFalse
mkAST (PConj p1 p2) = do
    a1 <- mkAST p1
    a2 <- mkAST p2
    mkAnd [a1, a2]

mkAST (PDisj p1 p2) = do
    a1 <- mkAST p1
    a2 <- mkAST p2
    mkOr [a1, a2]
mkAST (PNeg p) = mkAST p >>= mkNot

mkAST (PForAll x pty p) = do
    sym <- mkStringSymbol x
    sort <- tyToSort pty
    idx <- mkBound 0 sort
    local $ do
        bindQualified x idx
        a <- mkAST p
        mkForall [] [sym] [sort] a

mkAST (PExists x pty p) = do
    sym <- mkStringSymbol x
    sort <- tyToSort pty
    idx <- mkBound 0 sort
    local $ do
        bindQualified x idx
        a <- mkAST p
        mkExists [] [sym] [sort] a

mkAST (PImpli p1 p2) = do
    a1 <- mkAST p1
    a2 <- mkAST p2
    mkImplies a1 a2

mkAST (PCmp cmpop tm1 tm2) = do
    a1 <- mkTermAST tm1
    a2 <- mkTermAST tm2
    case cmpop of
        CLess -> mkLe a1 a2
        CEq   -> mkEq a1 a2

mkAST (PAssert a) = mkAssertAST a

mkAssertAST :: Assertion -> SMT AST
mkAssertAST (AInMap k v m) = do
    kTm <- mkTermAST k
    vTm <- mkTermAST v
    mTm <- mkTermAST m
    lhs <- mkSelect kTm mTm
    mkEq lhs vTm

mkTermAST :: Term -> SMT AST
mkTermAST (TmVar v) = do
    ctx <- get
    case M.lookup v (_valBindings ctx) of
        Just val -> mkValAST val
        Nothing  -> case M.lookup v (_qualifierContext ctx) of
            Just idx -> return idx
            Nothing -> throwError $ "Unbound variable: " ++ v

mkTermAST (TmVal pval) = mkValAST pval

mkValAST :: Value -> SMT AST
mkValAST = \case
    VBool b     -> mkBool b
    VInt i      -> mkIntSort >>= mkInt i
    VDouble n   -> mkRealNum n
    VMap m      -> mkMap m

mkMap :: M.Map Value Value -> SMT AST
mkMap m = do
    let tm = TmVal (VMap m)
    (tyk, tyv) <- case runInfer tm of
                    Left err -> throwError err
                    Right (TyMap t1 t2) -> return (t1, t2)
                    Right o -> throwError $ "Infer wrongly " ++ show tm ++ " as " ++ show o
    sk <- tyToSort tyk
    sv <- tyToSort tyv
    arrs <- mkArraySort sk sv
    fid <- genFreshId
    arr <- mkFreshConst ("map" ++ show fid ++ "_") arrs
    foldM (\arr' (k, v) -> do
        kast <- mkValAST k
        vast <- mkValAST v
        mkStore arr' kast vast) arr (M.toList m)

tyToSort :: Type -> SMT Sort
tyToSort TyBool   = mkBoolSort
tyToSort TyInt    = mkIntSort
tyToSort TyDouble = mkRealSort
tyToSort other    = throwError $ "can't tyToSort " ++ show other
