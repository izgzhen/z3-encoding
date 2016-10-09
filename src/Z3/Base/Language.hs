{-# LANGUAGE GADTs, UndecidableInstances #-}

-- Inspired by http://okmij.org/ftp/tagless-final/course/TTF.hs

module Z3.Base.Language where

import Z3.Monad
import Z3.Base.Class
import qualified Data.Set as S

class Language repr where
    lit :: Z3Lit a => a -> repr a
    add :: Num a => repr a -> repr a -> repr a
    equals :: Eq a => repr a -> repr a -> repr Bool
    lessThan, lessEqual, greaterThan, greaterEqual :: Ord a => repr a -> repr a -> repr Bool
    and_, or_, xor :: repr Bool -> repr Bool -> repr Bool
    not_ :: repr Bool -> repr Bool
    forall_ :: Z3Sorted a => Z3Sort a -> (repr a -> repr b) -> repr Bool
    exists :: Z3Sorted a => Z3Sort a -> (repr a -> repr b) -> repr Bool
    member_ :: Ord a => repr a -> repr (S.Set a) -> repr Bool


newtype SMTR m e a = SMTR { unSMTR :: SMT m e => m e AST }

mkBiOp :: (AST -> AST -> m e AST) -> SMTR m e a -> SMTR m e b -> SMTR m e c
mkBiOp op a b = SMTR $ do
    a' <- unSMTR a
    b' <- unSMTR b
    op a' b'

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

    add = mkBiOp (\a' b' -> mkAdd [a', b'])

    lessThan = mkBiOp mkLt
    lessEqual = mkBiOp mkLe
    equals = mkBiOp mkEq
    greaterThan = mkBiOp mkGt
    greaterEqual = mkBiOp mkGe

    and_ = mkBiOp (\a' b' -> mkAnd [a', b'])
    or_ = mkBiOp (\a' b' -> mkOr [a', b'])
    xor = mkBiOp mkXor
    not_ b = SMTR (unSMTR b >>= mkNot)

    forall_ = mkBinder mkForall
    exists = mkBinder mkExists

    member_ e s = SMTR $ do
        eTm <- unSMTR e
        sTm <- unSMTR s
        lhs <- mkSelect sTm eTm
        -- XXX: magic number
        one <- (mkIntSort >>= mkInt 1)
        mkEq one lhs

example :: SMT m e => SMTR m e Bool
example = forall_ Z3Sort (\(x :: SMTR m e Int) -> lessThan x (lit 1))
