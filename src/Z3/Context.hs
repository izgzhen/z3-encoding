module Z3.Context where

import Z3.Type
import Z3.Monad

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

data Decls = Decls {
    _valBindings  :: M.Map String Value,
    _datatypes :: [(String, [(String, [(String, Type)])])]
} deriving (Show, Eq)


data SMTContext = SMTContext {
    -- Globally free bindings
    _bindings :: M.Map String Value,
    -- Functions
    _funcContext :: M.Map String Type,
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

runSMT :: Decls -> SMT a -> IO (Either String a)
runSMT decls smt = evalZ3With Nothing opts m
    where
        opts = opt "MODEL" True
        funcs = constructFuncs (_datatypes decls)
        smt' = mapM_ initDataType (_datatypes decls) >> smt
        m = evalStateT (runExceptT smt')
                       (SMTContext (_valBindings decls) funcs M.empty 0)

initDataType :: (String, [(String, [(String, Type)])]) -> SMT (String, Sort)
initDataType (tyName, alts) = do
    constrs <- mapM (\(consName, fields) -> do
                        consSym <- mkStringSymbol consName
                        recogSym <- mkStringSymbol ("is_" ++ consName)
                        flds <- flip mapM fields $ \(fldName, fldTy) -> do
                            symFld <- mkStringSymbol fldName
                            sort <- tyToSort fldTy
                            return (symFld, Just sort, -1) -- XXX: non-rec
                        mkConstructor consSym recogSym flds
                    ) alts
    sym <- mkStringSymbol tyName
    sort <- mkDatatype sym constrs
    return (tyName, sort)

constructFuncs :: [(String, [(String, [(String, Type)])])] -> M.Map String Type
constructFuncs = M.fromList . concat . concatMap f
    where
        f (tyName, alts) =
            flip map alts $ \(consName, fields) ->
                let ftys = map snd fields
                    ty   = TyADT tyName
                    cons = (consName, foldr TyApp ty ftys)
                    dess = map (\(desName, fty) -> (desName, TyApp ty fty)) fields
                in cons : dess

bindQualified :: String -> AST -> SMT ()
bindQualified x idx = modify $ \ctx ->
        ctx { _qualifierContext =
              M.insert x idx (_qualifierContext ctx)
            }


tyToSort :: Type -> SMT Sort
tyToSort TyBool   = mkBoolSort
tyToSort TyInt    = mkIntSort
tyToSort TyDouble = mkRealSort
tyToSort other    = throwError $ "can't tyToSort " ++ show other

defaultOf :: Type -> SMT AST
defaultOf TyInt = do
    s <- mkIntSort
    mkInt (-1) s
