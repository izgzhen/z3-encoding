{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A concrete context implement SMT provided *for convenience*

module Z3.Context (Z3SMT, localSMT) where

import Z3.Monad
import Z3.Base.Class
import Z3.Datatypes

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

data SMTContext e = SMTContext {
    -- | Bind local variables introduced by qualifiers to de brujin index in Z3
    _valBindCtx :: M.Map String (AST, Sort),
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

    runSMT initialBindings datatypes e smt = evalZ3With Nothing opts m
        where
            smt' = do
                sorts <- mapM encodeDataType datatypes
                let datatypeCtx = M.fromList (zip (map fst datatypes) sorts)
                modify $ \ctx -> ctx { _datatypeCtx = datatypeCtx }
                bindVals initialBindings
                smt

            -- XXX: not sure what does this option mean
            opts = opt "MODEL" True
            m = evalStateT (runExceptT (unZ3SMT smt'))
                           (SMTContext M.empty M.empty 0 e)

    bindVal x ast st = modify $ \ctx ->
            ctx { _valBindCtx = M.insert x (ast, st) (_valBindCtx ctx) }

    modifyValBindCtx bindings = do
        modify $ \ctx -> ctx { _valBindCtx = M.empty }
        bindVals bindings

    getValBindMaybe x = do
        ctx <- _valBindCtx <$> get
        return $ M.lookup x ctx

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
