{-
    izgzhen: Core.Context should provide the basic support which *can* be extended,
    but never posing any more constraint on how it should be used. In this way,
    we can build a layered structure which is extensible and modular.
-}

{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}

module Z3.Core.Class (
    Z3Encoded,
    Z3Sorted,
    Z3Sort(..),
    Z3Reserved,
    sortPhantom,
    encode,
    sort,
    def,
    SMT,
    runSMT,
    smtError,
    genFreshId,
    modifyExtra,
    getExtra,
    getQualifierCtx,
    getDataTypeCtx,
    bindQualified
) where

import Z3.Monad

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

data Z3Sort a = Z3Sort

class Z3Encoded a where
    encode :: SMT m e => a -> m e AST

-- HACK: Unsafe
class Z3Sorted a where
    sort :: SMT m e => a -> m e Sort
    sort _ = sortPhantom (Z3Sort :: Z3Sort a)

    sortPhantom :: SMT m e => Z3Sort a -> m e Sort
    sortPhantom _ = smtError "sort error"

class Z3Encoded a => Z3Reserved a where
    def :: a

class (MonadError String (m e), MonadZ3 (m e)) => SMT m e where
    genFreshId :: m e Int
    runSMT :: Z3Sorted ty => [(String, [(String, [(String, ty)])])] -> e -> m e a -> IO (Either String a)
    bindQualified :: String -> AST -> Sort -> m e ()
    getQualifierCtx :: m e (M.Map String (AST, Sort))
    getDataTypeCtx :: m e (M.Map String Sort)
    getExtra :: m e e
    smtError :: String -> m e a
    modifyExtra :: (e -> e) -> m e ()
