{-# LANGUAGE GADTs, UndecidableInstances #-}

-- Inspired by http://okmij.org/ftp/tagless-final/course/TTF.hs

module Z3.Base.Tagless where

import Z3.Monad
import Z3.Base.Class

class Language repr where
    lit :: Z3Lit a => a -> repr a
    add :: repr Int -> repr Int -> repr Int
    less :: Ord a => repr a -> repr a -> repr Bool
    forall_ :: Z3Sorted a => Z3Sort a -> (repr a -> repr b) -> repr Bool

newtype SMTR m e a = SMTR { unSMTR :: SMT m e => m e AST }

instance SMT m e => Language (SMTR m e) where
    lit = SMTR . encode
    add a b = SMTR $ do
        a' <- unSMTR a
        b' <- unSMTR b
        mkAdd [a', b']
    less a b = SMTR $ do
        a' <- unSMTR a
        b' <- unSMTR b
        mkLe a' b'
    forall_ s f = SMTR $ do
        sym <- mkStringSymbol "x"
        s' <- sortOf s
        idx <- mkBound 0 s'
        local $ do
            body <- unSMTR (f $ SMTR $ return idx)
            mkForall [] [sym] [s'] body

example :: SMT m e => SMTR m e Bool
example = forall_ Z3Sort (\(x :: SMTR m e Int) -> less x (lit 1))
