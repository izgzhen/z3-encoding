{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A concrete context implement SMT provided *for convenience*

module Z3.Context (Z3SMT) where

import Z3.Monad
import Z3.Base.Class
import Z3.Base.Encoding

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
            bind (x:xs, Cons v vs) = do
                ast <- encode v
                st <- sortOf v
                bindVal x ast st
                bind (xs, vs)
            bind _ = return ()

            smt' = do
                sorts <- mapM encodeDataType datatypes
                let datatypeCtx = M.fromList (zip (map fst datatypes) sorts)
                modify $ \ctx -> ctx { _datatypeCtx = datatypeCtx }
                bind initialBindings
                smt

            -- XXX: not sure what does this option mean
            opts = opt "MODEL" True
            m = evalStateT (runExceptT (unZ3SMT smt'))
                           (SMTContext M.empty M.empty 0 e)

    bindVal x ast st = modify $ \ctx ->
            ctx { _valBindCtx = M.insert x (ast, st) (_valBindCtx ctx) }

    getValBindCtx = _valBindCtx <$> get

    getDataTypeCtx = _datatypeCtx <$> get

    getExtra = _extra <$> get

    modifyExtra f = modify $ \ctx -> ctx { _extra = f (_extra ctx) }
