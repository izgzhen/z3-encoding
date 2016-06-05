module Z3.Context where

import Z3.Type
import Z3.Monad

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

data SMTContext = SMTContext {
    -- Globally free bindings
    _valBindings :: M.Map String Value,
    -- Bind local variables introduced by qualifiers to de brujin index in Z3
    _qualifierContext :: M.Map String AST,
    -- Counter used to generate globally unique ID
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

bindQualified :: String -> AST -> SMT ()
bindQualified x idx = modify (\ctx ->
        ctx { _qualifierContext = M.insert x idx (_qualifierContext ctx)}
    )
