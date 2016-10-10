{-# LANGUAGE UndecidableInstances #-}

module Z3.Base.SMTR where

import Z3.Monad
import Z3.Base.Language
import Z3.Base.Class

newtype SMTR m e a = SMTR { unSMTR :: SMT m e => m e AST }

mkBiOp :: (AST -> AST -> m e AST) -> SMTR m e a -> SMTR m e b -> SMTR m e c
mkBiOp op a b = SMTR $ do
    a' <- unSMTR a
    b' <- unSMTR b
    op a' b'

mkBiOp' :: ([AST] -> m e AST) -> SMTR m e a1 -> SMTR m e a2 -> SMTR m e a
mkBiOp' op a b = SMTR $ do
    a' <- unSMTR a
    b' <- unSMTR b
    op [a', b']

mkUnOp :: (AST -> m e AST) -> SMTR m e a1 -> SMTR m e a
mkUnOp op x = SMTR (unSMTR x >>= op)

mkBinder :: Z3Sorted a1 => ([t] -> [Symbol] -> [Sort] -> AST -> m e AST) ->
                           Z3Sort a1 -> (SMTR m1 e1 a3 -> SMTR m e a2) -> SMTR m e a
mkBinder op s f = SMTR $ do
    sym <- mkStringSymbol "x"
    s' <- sortOf s
    idx <- mkBound 0 s'
    local $ do
        body <- unSMTR (f $ SMTR $ return idx)
        op [] [sym] [s'] body

instance SMT m e => Language (SMTR m e) where
    lit = SMTR . encode

    (.+) = mkBiOp' mkAdd
    (.-) = mkBiOp' mkSub
    (.*) = mkBiOp' mkMul
    (./) = mkBiOp mkDiv
    mod_ = mkBiOp mkMod
    rem = mkBiOp mkRem
    neg = mkUnOp mkUnaryMinus

    (.<) = mkBiOp mkLt
    (.<=) = mkBiOp mkLe
    (.=) = mkBiOp mkEq
    (.>) = mkBiOp mkGt
    (.>=) = mkBiOp mkGe

    (/\) = mkBiOp' mkAnd
    (\/) = mkBiOp' mkOr
    xor = mkBiOp mkXor
    not_ = mkUnOp mkNot

    forall_ = mkBinder mkForall
    exists = mkBinder mkExists
    (==>) = mkBiOp mkImplies

    if_ p c a = SMTR $ do
        a1 <- unSMTR p
        a2 <- unSMTR c
        a3 <- unSMTR a
        mkIte a1 a2 a3

    member_ e s = SMTR $ do
        eTm <- unSMTR e
        sTm <- unSMTR s
        lhs <- mkSelect sTm eTm
        -- XXX: magic number
        one <- (mkIntSort >>= mkInt 1)
        mkEq one lhs

instance SMT m e => OptLang (SMTR m e) where
    none = SMTR $ do
        sym <- mkStringSymbol "none"
        Just s <- getDataTypeMaybe "option" -- FIXME
        decl <- mkFuncDecl sym [] s
        mkApp decl []

    some a = SMTR $ do
        sa <- sortOf' a
        asta <- unSMTR $ lit a
        sym <- mkStringSymbol "some"
        Just s <- getDataTypeMaybe "option" -- FIXME
        decl <- mkFuncDecl sym [sa] s
        mkApp decl [asta]
