{-# LANGUAGE GADTs, UndecidableInstances, ScopedTypeVariables, FlexibleInstances #-}

-- Inspired by http://okmij.org/ftp/tagless-final/course/TTF.hs

-- module Z3.Base.Tagless where

import Z3.Monad

data Z3Sort a = Z3Sort

class Z3Sorted a where
    sortOf :: Z3Sort a -> Z3 Sort

instance Z3Sorted Int where
    sortOf _ = mkIntSort

class Language repr where
    int :: Int -> repr Int
    add :: repr Int -> repr Int -> repr Int
    less :: Ord a => repr a -> repr a -> repr Bool
    forall_ :: Z3Sorted a => Z3Sort a -> (repr a -> repr b) -> repr Bool

newtype SMT m a = SMT { unSMT :: m AST }

instance Language (SMT Z3) where
    int i = SMT $ mkIntSort >>= mkInt i
    add a b = SMT $ do
        a' <- unSMT a
        b' <- unSMT b
        mkAdd [a', b']
    less a b = SMT $ do
        a' <- unSMT a
        b' <- unSMT b
        mkLe a' b'
    forall_ s f = SMT $ do
        sym <- mkStringSymbol "x"
        s' <- sortOf s
        idx <- mkBound 0 s'
        local $ do
            body <- unSMT (f $ SMT $ return idx)
            mkForall [] [sym] [s'] body

assert_ :: SMT Z3 Bool -> Z3 ()
assert_ (SMT m) = m >>= assert

runZ3 :: Z3 () -> IO (Result, Maybe Model)
runZ3 m = evalZ3With Nothing (opt "MODEL" True) (m >> getModel)

example :: Z3 ()
example = assert_ $ forall_ Z3Sort (\(x :: SMT Z3 Int) -> less x (int 1))

main :: IO ()
main = runZ3 example >>= print . fst
