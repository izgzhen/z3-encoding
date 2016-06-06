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
import qualified Data.Set as S

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
    lhs <- mkSelect mTm kTm
    mkEq lhs vTm
mkAssertAST (AInSet e s) = do
    eTm <- mkTermAST e
    sTm <- mkTermAST s
    lhs <- mkSelect sTm eTm
    one <- (mkIntSort >>= mkInt 1)
    mkEq one lhs

mkTermAST :: Term -> SMT AST
mkTermAST (TmVar v) = do
    ctx <- get
    case M.lookup v (_bindings ctx) of
        Just val -> mkValAST val
        Nothing  -> case M.lookup v (_qualifierContext ctx) of
            Just idx -> return idx
            Nothing -> do -- zero arity func
                decl <- mkFunc v
                mkApp decl []
mkTermAST (TmVal pval) = mkValAST pval
mkTermAST (TmApp f tms) = mkAppAST f tms

mkFunc :: String -> SMT FuncDecl
mkFunc fname = do
    ctx <- get
    case M.lookup fname (_funcContext ctx) of
        Just ty -> do
            let tys = flattenApp ty
            paramSorts <- mapM tyToSort (init tys)
            retSort <- tyToSort (last tys)
            sym <- mkStringSymbol fname
            mkFuncDecl sym paramSorts retSort
        Nothing -> throwError $ "Unbound variable: " ++ fname

mkAppAST :: String -> [Term] -> SMT AST
mkAppAST fname args = do
    argASTs <- mapM mkTermAST args
    ctx <- _funcContext <$> get
    decl <- mkFunc fname
    mkApp decl argASTs

flattenApp (TyApp t1 t2) = flattenApp t1 ++ flattenApp t2
flattenApp other = [other]

mkValAST :: Value -> SMT AST
mkValAST = \case
    VBool b     -> mkBool b
    VInt i      -> mkIntSort >>= mkInt i
    VDouble n   -> mkRealNum n
    VMap m      -> mkMap m
    VSet s      -> mkSet s

mkSet :: S.Set Value -> SMT AST
mkSet s = do
    let tm = TmVal (VSet s)
    ctx <- _funcContext <$> get
    tyElem <- case runInfer ctx tm of
                Left err        -> throwError err
                Right (TySet t) -> return t
                Right other     -> throwError $ "Infer wrongly " ++
                                                show tm ++ " as " ++ show other
    sortElem <- tyToSort tyElem
    intSort <- mkIntSort
    arrSort <- mkArraySort sortElem intSort
    fid <- genFreshId
    arr <- mkFreshConst ("set" ++ "_" ++ show fid) arrSort
    mapM_ (\e -> do
        ast <- mkValAST e
        sel <- mkSelect arr ast
        one <- (mkIntSort >>= mkInt 1)
        mkEq sel one >>= assert) (S.toList s)
    def <- mkArrayDefault arr
    zero <- (mkIntSort >>= mkInt 0)
    mkEq zero def >>= assert
    return arr

mkMap :: M.Map Value Value -> SMT AST
mkMap m = do
    let tm = TmVal (VMap m)
    ctx <- _funcContext <$> get
    (tyk, tyv) <- case runInfer ctx tm of
                    Left err -> throwError err
                    Right (TyMap t1 t2) -> return (t1, t2)
                    Right o -> throwError $ "Infer wrongly " ++ show tm ++ " as " ++ show o
    sk <- tyToSort tyk
    sv <- tyToSort tyv
    vDef <- defaultOf tyv
    arrSort <- mkArraySort sk sv
    fid <- genFreshId
    arr <- mkFreshConst ("map" ++ "_" ++ show fid) arrSort
    mapM_ (\(k, v) -> do
        kast <- mkValAST k
        vast <- mkValAST v
        sel <- mkSelect arr kast
        mkEq sel vast >>= assert) (M.toList m)
    def <- mkArrayDefault arr
    mkEq vDef def >>= assert
    return arr
