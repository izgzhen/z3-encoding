{-# LANGUAGE GADTs, UndecidableInstances #-}

-- Inspired by http://okmij.org/ftp/tagless-final/course/TTF.hs

module Z3.Base.Tagless where

import Z3.Monad

data Z3Sort a = Z3Sort

class Z3Sorted a where
    sortOf :: Z3Sort a -> Z3 Sort

class Language repr where
    int :: Int -> repr Int
    add :: repr Int -> repr Int -> repr Int
    forall_ :: Z3Sorted a => Z3Sort a -> (repr a -> repr b) -> repr Bool

newtype SMT m a = SMT { runSMT :: m AST }

instance Language (SMT Z3) where
    int i = SMT $ mkIntSort >>= mkInt i
    add a b = SMT $ do
        a' <- runSMT a
        b' <- runSMT b
        mkAdd [a', b']
    forall_ s f = SMT $ do
        sym <- mkStringSymbol "x"
        s' <- sortOf s
        idx <- mkBound 0 s'
        local $ do
            body <- runSMT (f $ SMT $ return idx)
            mkForall [] [sym] [s'] body

runZ3 :: SMT Z3 () -> IO (Either String a)
runZ3 = undefined
