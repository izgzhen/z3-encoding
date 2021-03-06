{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A concrete context implement SMT provided *for convenience*

module Z3.Context (Z3SMT, localSMT, checkPre) where

import Z3.Monad
import Z3.Base.Class
import Z3.Base.SMTR
import Z3.Datatypes

import qualified Data.Map as M

import Control.Monad.State
import Control.Monad.Except

data SMTContext e = SMTContext {
    -- | From type name to Z3 sort
    _datatypeCtx :: M.Map String Sort,
    -- | Counter used to generate globally unique ID
    _counter :: Int,
    -- | Extra field reserved for extension
    _extra :: e
} deriving (Show, Eq)

newtype Z3SMT e a = Z3SMT { unZ3SMT :: ExceptT String (StateT (SMTContext e) Z3) a }
    deriving (Monad, Applicative, Functor, MonadState (SMTContext e), MonadIO, MonadError String)

instance MonadZ3 (Z3SMT e) where
  getSolver  = Z3SMT (lift (lift getSolver))
  getContext = Z3SMT (lift (lift getContext))

instance SMT Z3SMT e where
    genFreshId = do
        i <- _counter <$> get
        modify (\ctx -> ctx { _counter = i + 1 })
        return i

    runSMT e datatypes smt = evalZ3With Nothing opts m
        where
            smt' = do
                sorts <- mapM encodeDataType datatypes
                let datatypeCtx = M.fromList (zip (map fst datatypes) sorts)
                modify $ \ctx -> ctx { _datatypeCtx = datatypeCtx }
                smt

            opts = opt "MODEL" True
            m = evalStateT (runExceptT (unZ3SMT smt'))
                           (SMTContext M.empty 0 e)
    getDataTypeMaybe x = do
        ctx <- _datatypeCtx <$> get
        return $ M.lookup x ctx

    getExtra = _extra <$> get

    modifyExtra f = modify $ \ctx -> ctx { _extra = f (_extra ctx) }

localSMT :: Z3SMT e a -> Z3SMT e a
localSMT m = do
    s <- get
    ret <- local m
    put s
    return ret

checkPre :: SMTR Z3SMT e Bool -> Z3SMT e (Result, Maybe Model)
checkPre (SMTR m) = (m >>= assert) >> getModel
