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
    forall_ :: Z3Sorted a => Z3Sort a -> (repr a -> repr b) -> repr Bool
    member_ :: Ord a => repr a -> repr (S.Set a) -> repr Bool

newtype SMTR m e a = SMTR { unSMTR :: SMT m e => m e AST }

mkBiOp :: (AST -> AST -> m e AST) -> SMTR m e a -> SMTR m e b -> SMTR m e c
mkBiOp f a b = SMTR $ do
    a' <- unSMTR a
    b' <- unSMTR b
    f a' b'

instance SMT m e => Language (SMTR m e) where
    lit = SMTR . encode

    add = mkBiOp (\a' b' -> mkAdd [a', b'])

    lessThan = mkBiOp mkLt
    lessEqual = mkBiOp mkLe
    equals = mkBiOp mkEq
    greaterThan = mkBiOp mkGt
    greaterEqual = mkBiOp mkGe

    forall_ s f = SMTR $ do
        sym <- mkStringSymbol "x"
        s' <- sortOf s
        idx <- mkBound 0 s'
        local $ do
            body <- unSMTR (f $ SMTR $ return idx)
            mkForall [] [sym] [s'] body

    member_ e s = SMTR $ do
        eTm <- unSMTR e
        sTm <- unSMTR s
        lhs <- mkSelect sTm eTm
        -- XXX: magic number
        one <- (mkIntSort >>= mkInt 1)
        mkEq one lhs

example :: SMT m e => SMTR m e Bool
example = forall_ Z3Sort (\(x :: SMTR m e Int) -> lessThan x (lit 1))
