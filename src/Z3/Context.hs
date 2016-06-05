module Z3.Context where

import Z3.Type
import Z3.Monad

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

data SMTContext = SMTContext {
    _valBindings :: M.Map String Value,
    _typeContext :: M.Map String Sort,
    _counter :: Int
} deriving (Show, Eq)

type SMT = ExceptT String (StateT SMTContext Z3)

instance MonadZ3 SMT where
  getSolver  = lift (lift getSolver)
  getContext = lift (lift getContext)

genFreshId :: SMT Int
genFreshId = do
    i <- _counter <$> get
    modify (\ctx -> ctx { _counter = i + 1 })
    return i


runSMT :: M.Map String Value -> SMT a -> IO (Either String a)
runSMT binds smt = evalZ3With Nothing opts m
    where
        opts = opt "MODEL" True
        m = evalStateT (runExceptT smt) (SMTContext binds M.empty 0)

addType :: String -> Sort -> SMT ()
addType x s = modify (\ctx -> ctx { _typeContext = M.insert x s (_typeContext ctx)})
