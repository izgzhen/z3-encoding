-- |
-- Type classes and built-in implementation for primitive Haskell types
-- 

module Z3.Base.Class (
    -- ** Types whose values are encodable to Z3 internal AST
    Z3Lit(..),
    -- ** Types representable as Z3 Sort
    Z3Sorted(..),
    -- ** Type proxy helper, used with Z3Sorted
    Z3Sort(..),
    -- ** Monad which can be instantiated into a concrete context
    SMT(..),
    Type(..),
    Datatype
) where

import Z3.Monad
import Control.Monad.Except

import qualified Data.Set as S

data Z3Sort a = Z3Sort

newtype Type m e = Type { unType :: m e Sort }

type Datatype m e = (String, [(String, [(String, Type m e)])])

class Z3Sorted a where
    sortOf :: SMT m e => Z3Sort a -> m e Sort

class Z3Sorted a => Z3Lit a where
    encode :: SMT m e => a -> m e AST
    sortOf' :: SMT m e => a -> m e Sort
    sortOf' _ = sortOf (Z3Sort :: Z3Sort a)

instance Z3Sorted Int where
    sortOf _ = mkIntSort

instance Z3Lit Int where
    encode i = mkIntSort >>= mkInt i

instance Z3Sorted Double where
    sortOf _ = mkRealSort

instance Z3Lit Double where
    encode = mkRealNum

instance Z3Sorted Bool where
    sortOf _ = mkBoolSort

instance Z3Lit Bool where
    encode = mkBool

instance Z3Sorted (Maybe a) where
    sortOf _ = do
        Just s <- getDataTypeMaybe "option" -- FIXME
        return s

class (MonadError String (m e), MonadZ3 (m e)) => SMT m e where
    -- | Globally unique id
    genFreshId :: m e Int

    -- | Given extra field and the SMT monad, return the fallible result in IO monad
    runSMT :: e -> [Datatype m e] -> m e a -> IO (Either String a)

    getDataTypeMaybe :: String -> m e (Maybe Sort)

    getDataType :: String -> m e Sort
    getDataType x = getDataTypeMaybe x >>= \case
        Just p  -> return p
        Nothing -> smtError $ "no such type variable: " ++ x

    -- | Get extra
    getExtra :: m e e

    -- | Set extra
    modifyExtra :: (e -> e) -> m e ()

    -- | User don't have to import throwError
    smtError :: String -> m e a
    smtError = throwError

-- Basic idea:
-- Set v =def= Map v {0, 1}
instance (Z3Sorted v, Z3Lit v) => Z3Lit (S.Set v) where
    encode s = do
        setSort <- sortOf (Z3Sort :: Z3Sort (S.Set v))
        fid <- genFreshId
        arr <- mkFreshConst ("set" ++ "_" ++ show fid) setSort
        mapM_ (\e -> do
            ast <- encode e
            sel <- mkSelect arr ast
            one <- (mkIntSort >>= mkInt 1)
            mkEq sel one >>= assert) (S.toList s)
        arrValueDef <- mkArrayDefault arr
        zero <- (mkIntSort >>= mkInt 0)
        mkEq zero arrValueDef >>= assert
        return arr

instance Z3Sorted v => Z3Sorted (S.Set v) where
    sortOf _ = do
        sortElem <- sortOf (Z3Sort :: Z3Sort v)
        intSort <- mkIntSort
        mkArraySort sortElem intSort
