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

    add = mkBiOp' mkAdd
    sub = mkBiOp' mkSub
    mul = mkBiOp' mkMul
    div_ = mkBiOp mkDiv
    mod_ = mkBiOp mkMod
    rem = mkBiOp mkRem
    neg = mkUnOp mkUnaryMinus

    lessThan = mkBiOp mkLt
    lessEqual = mkBiOp mkLe
    equals = mkBiOp mkEq
    greaterThan = mkBiOp mkGt
    greaterEqual = mkBiOp mkGe

    and_ = mkBiOp' mkAnd
    or_ = mkBiOp' mkOr
    xor = mkBiOp mkXor
    not_ = mkUnOp mkNot

    forall_ = mkBinder mkForall
    exists = mkBinder mkExists

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

example :: SMT m e => SMTR m e Bool
example = forall_ Z3Sort (\(x :: SMTR m e Int) -> lessThan x (lit 1))
