{-# LANGUAGE ScopedTypeVariables #-}

module Z3.Container where

import Z3.Core.Class
import Z3.Monad

import qualified Data.Map as M
import qualified Data.Set as S

instance (Z3Sorted v, Z3Encoded v) => Z3Encoded (S.Set v) where
    encode s = do
        setSort <- sort s
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
    sortPhantom _ = do
        sortElem <- sortPhantom (Z3Sort :: Z3Sort v)
        intSort <- mkIntSort
        mkArraySort sortElem intSort

instance (Z3Sorted k, Z3Encoded k, Z3Sorted v, Z3Reserved v) => Z3Encoded (M.Map k v) where
    encode m = do
        fid <- genFreshId
        arrSort <- sort m
        arr <- mkFreshConst ("map" ++ "_" ++ show fid) arrSort
        mapM_ (\(k, v) -> do
            kast <- encode k
            vast <- encode v
            sel <- mkSelect arr kast
            mkEq sel vast >>= assert) (M.toList m)
        arrValueDef <- mkArrayDefault arr
        vdef <- encode (def :: v)
        mkEq arrValueDef vdef >>= assert
        return arr

instance (Z3Sorted k, Z3Sorted v) => Z3Sorted (M.Map k v) where
    sortPhantom _ = do
        sk <- sortPhantom  (Z3Sort :: Z3Sort k)
        sv <- sortPhantom  (Z3Sort :: Z3Sort v)
        mkArraySort sk sv
