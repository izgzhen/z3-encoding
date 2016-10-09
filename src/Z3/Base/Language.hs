{-# LANGUAGE GADTs, UndecidableInstances #-}

-- Inspired by http://okmij.org/ftp/tagless-final/course/TTF.hs

module Z3.Base.Language where

import Z3.Monad
import Z3.Base.Class
import qualified Data.Set as S

class Language repr where
    lit :: Z3Lit a => a -> repr a
    add, sub, mul :: Num a => repr a -> repr a -> repr a
    div_ :: Fractional a => repr a -> repr a -> repr a
    mod_, rem :: Integral a => repr a -> repr a -> repr a
    neg :: Num a => repr a -> repr a
    equals :: Eq a => repr a -> repr a -> repr Bool
    lessThan, lessEqual, greaterThan, greaterEqual :: Ord a => repr a -> repr a -> repr Bool
    and_, or_, xor :: repr Bool -> repr Bool -> repr Bool
    not_ :: repr Bool -> repr Bool
    if_ :: repr Bool -> repr a -> repr a -> repr a
    forall_ :: Z3Sorted a => Z3Sort a -> (repr a -> repr b) -> repr Bool
    exists :: Z3Sorted a => Z3Sort a -> (repr a -> repr b) -> repr Bool
    member_ :: Ord a => repr a -> repr (S.Set a) -> repr Bool


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
