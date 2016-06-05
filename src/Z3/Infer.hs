module Z3.Infer where

import Z3.Spec
import Z3.Type

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

-- Unification

type Substitution = M.Map String Type

unify :: Type -> Type -> Maybe Substitution
unify (TyVar x) t  = Just $ M.singleton x t
unify t (TyVar x)  = Just $ M.singleton x t
unify (TyMap tyk tyv) (TyMap tyk' tyv') = do
    s1 <- unify tyk tyk'
    s2 <- unify (tyv `subst` s1) tyv'
    return $ s1 `M.union` s2
-- unify (TySet t) (TySet t') = unify t t'

unify t1 t2 | t1 == t2  = Just M.empty
            | otherwise = Nothing

subst :: Type -> Substitution -> Type
subst t@(TyVar x) s = case M.lookup x s of
    Just t' -> t'
    Nothing -> t

subst (TyMap t1 t2) s = TyMap (t1 `subst` s) (t2 `subst` s)
-- subst (TySet t) s     = TySet (t `subst` s)
subst ty _ = ty

-- Algorithm W

newtype Counter = Counter { unCounter :: Int } deriving (Eq, Show)

newName :: MonadState Counter m => m String
newName = do
    Counter i <- get
    let name = "_x" ++ show i
    put $ Counter (i + 1)
    return name

newTyVar :: MonadState Counter m => m Type
newTyVar = TyVar <$> newName

infer :: M.Map String TS -> Term -> Infer (Substitution, Type)
infer env expr = case expr of
    TmVal pval -> case pval of
        VBool _   -> return (M.empty, TyBool)
        VInt _    -> return (M.empty, TyInt)
        VDouble _ -> return (M.empty, TyDouble)
        VMap m ->
            let l = M.toList m in
            if length l > 0
                then do
                    let (tm1, tm2):_ = l
                    (s1, t1) <- infer env (TmVal tm1)
                    (s2, t2) <- infer (env `substEnv` s1) (TmVal tm2)
                    t <- newTyVar
                    case unify (t1 `subst` s2) (TyMap t2 t) of
                        Just v  -> return (v `unionUpdate` (s2 `unionUpdate` s1), t `subst` v)
                        Nothing -> throwError $ "can't unify " ++
                                                "t1: " ++ show (t1 `subst` s2) ++ " with " ++
                                                "t2: " ++ show (TyMap t2 t)
                else do
                    t1 <- newTyVar
                    t2 <- newTyVar
                    return (M.empty, TyMap t1 t2)

        -- VSet s -> do
        --     let l = S.toList s
        --     if length l > 0
        --         then infer env (TmVal (head l))
        --         else do
        --             t <- newTyVar
        --             return (M.empty, TySet t)
        other -> throwError $ "can't infer" ++ show other
    TmVar x ->
        case M.lookup x env of
            Just ts -> do
                let (tyvars, ty) = canonicalize ts
                let n = length tyvars
                tyvars' <- sequence $ take n $ repeat newTyVar
                let s = M.fromList $ zip tyvars tyvars'
                return (M.empty, ty `subst` s)
            Nothing -> throwError $ "no such variable: " ++ show x

    other -> throwError $ "can't infer type for " ++ show other

type Infer = ExceptT String (ReaderT () (State Counter))

runInfer :: Term -> Either String Type
runInfer term =
    let initCounter = Counter 0
        initEnv     = M.empty
        m           = infer initEnv term
        e           = fst $ runState (runReaderT (runExceptT m) ()) initCounter
    in  snd <$> e

-- Helpers

substEnv :: M.Map String TS -> Substitution -> M.Map String TS
substEnv e s = M.map (`substInner` s) e

unionUpdate :: Ord k => M.Map k v -> M.Map k v -> M.Map k v
unionUpdate a b = (b M.\\ a) `M.union` a

tyClosure :: M.Map String TS -> Type -> TS
tyClosure e t =
    let tvars = freeInType t L.\\ freeInEnv e
    in  mkTS tvars t

mkTS :: [String] -> Type -> TS
mkTS [] ty     = TSInner ty
mkTS (t:ts) ty = TSForall t (mkTS ts ty)

canonicalize :: TS -> ([String], Type)
canonicalize (TSInner ty)    = ([], ty)
canonicalize (TSForall t ty) =
    let (tyvars, ty') = canonicalize ty
    in  (t : tyvars, ty')

freeInEnv :: M.Map String TS -> [String]
freeInEnv = foldr L.union [] . map freeInTS . M.elems

freeInTS :: TS -> [String]
freeInTS ts =
    let (tyvars, ty) = canonicalize ts
    in  freeInType ty L.\\ tyvars

freeInType :: Type -> [String]
freeInType (TyVar x) = [x]
freeInType (TyMap t1 t2) = freeInType t1 ++ freeInType t2
-- freeInType (TySet t) = freeInType t
freeInType _ = []


substInner :: TS -> Substitution -> TS
substInner ts s =
    let (tyvars, ty) = canonicalize ts
        s' = foldr M.delete s tyvars
    in  mkTS tyvars $ ty `subst` s'

