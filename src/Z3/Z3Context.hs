{-
    izgzhen: Core.Context should provide the basic support which *can* be extended,
    but never posing any more constraint on how it should be used. In this way,
    we can build a layered structure which is extensible and modular.
-}

{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, RankNTypes, GeneralizedNewtypeDeriving #-}

module Z3.Z3Context (
    Z3SMT,
) where

import Z3.Monad
import Z3.Core.Class
import Z3.Encoding

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

data Z3SMTContext e = Z3SMTContext {
    -- Functions
    -- _funcContext :: M.Map String Type,
    -- Bind local variables introduced by qualifiers to de brujin index in Z3
    _qualifierContext :: M.Map String (AST, Sort),
    -- From name to Z3 sort
    _datatypeCtx :: M.Map String Sort,
    -- Counter used to generate globally unique ID
    _counter :: Int,
    _extra :: e
} deriving (Show, Eq)

newtype Z3SMT e a = Z3SMT { unZ3SMT :: ExceptT String (StateT (Z3SMTContext e) Z3) a }
    deriving (Monad, Applicative, Functor, MonadState (Z3SMTContext e), MonadIO, MonadError String)

instance MonadZ3 (Z3SMT e) where
  getSolver  = Z3SMT (lift (lift getSolver))
  getContext = Z3SMT (lift (lift getContext))

-- instance Monad (Z3SMT e) where
--   return = Z3SMT . return

-- instance MonadIO (Z3SMT e) where
--     liftIO = Z3SMT . liftIO

-- instance Applicative (Z3SMT e) where
--   pure = Z3SMT . pure
--    -- (<*>) :: f (a -> b) -> f a -> f b
--    -- (*>) :: f a -> f b -> f b
--    -- (<*) :: f a -> f b -> f a

-- instance MonadError String (Z3SMT e) where
--  throwError = Z3SMT . throwError
--  catchError = Z3SMT . catchError

instance SMT Z3SMT e where
    genFreshId = do
        i <- _counter <$> get
        modify (\ctx -> ctx { _counter = i + 1 })
        return i

    runSMT datatypes e smt = evalZ3With Nothing opts m
        where
            smt' = do
                sorts <- mapM encodeDataType datatypes
                let datatypeCtx = M.fromList (zip (map fst datatypes) sorts)
                modify $ \ctx -> ctx { _datatypeCtx = datatypeCtx }
                smt

            opts = opt "MODEL" True
            m = evalStateT (runExceptT (unZ3SMT smt'))
                           (Z3SMTContext M.empty M.empty 0 e)

    bindQualified x idx s = modify $ \ctx ->
            ctx { _qualifierContext = M.insert x (idx, s) (_qualifierContext ctx) }

    getQualifierCtx = _qualifierContext <$> get

    getDataTypeCtx = _datatypeCtx <$> get

    getExtra = _extra <$> get

    smtError = throwError

    modifyExtra f = modify $ \ctx -> ctx { _extra = f (_extra ctx) }
