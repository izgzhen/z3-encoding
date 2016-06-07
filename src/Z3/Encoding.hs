{-# LANGUAGE RankNTypes, GADTs #-}

module Z3.Encoding (
  HeteroList(..),
  encodeApp,
  encodeDataType
) where

import Z3.Core.Class
import Z3.Core.Logic
import Z3.Monad hiding (mkMap, App)

data HeteroList where
    Cons :: forall a. (Z3Sorted a, Z3Encoded a) => a -> HeteroList -> HeteroList
    Nil :: HeteroList

mapH :: (forall a. (Z3Sorted a, Z3Encoded a) => a -> b) -> HeteroList -> [b]
mapH _ Nil = []
mapH f (Cons a l) = f a : mapH f l

encodeApp :: SMT m e => String -> HeteroList -> Sort -> m e AST
encodeApp fname args retSort = do
    paramSorts <- sequence $ mapH sort args
    sym <- mkStringSymbol fname
    decl <- mkFuncDecl sym paramSorts retSort
    argASTs <- sequence $ mapH encode args
    mkApp decl argASTs

encodeDataType :: SMT m e => Z3Sorted ty => (String, [(String, [(String, ty)])]) -> m e Sort
encodeDataType (tyName, alts) = do
    constrs <- mapM (\(consName, fields) -> do
                        consSym <- mkStringSymbol consName
                        recogSym <- mkStringSymbol ("is_" ++ consName)
                        flds <- flip mapM fields $ \(fldName, fldTy) -> do
                            symFld <- mkStringSymbol fldName
                            sort <- sort fldTy
                            return (symFld, Just sort, -1) -- XXX: non-rec
                        mkConstructor consSym recogSym flds
                    ) alts
    sym <- mkStringSymbol tyName
    sort <- mkDatatype sym constrs
    return sort

instance (Z3Sorted t, Z3Sorted ty, Z3Encoded a) => Z3Encoded (Pred t ty a) where
    encode PTrue = mkTrue
    encode PFalse = mkFalse
    encode (PConj p1 p2) = do
        a1 <- encode p1
        a2 <- encode p2
        mkAnd [a1, a2]

    encode (PDisj p1 p2) = do
        a1 <- encode p1
        a2 <- encode p2
        mkOr [a1, a2]
    encode (PNeg p) = encode p >>= mkNot

    encode (PForAll x ty p) = do
        sym <- mkStringSymbol x
        xsort <- sort ty
        idx <- mkBound 0 xsort
        local $ do
            bindQualified x idx xsort
            a <- encode p
            mkForall [] [sym] [xsort] a

    encode (PExists x ty p) = do
        sym <- mkStringSymbol x
        xsort <- sort ty
        idx <- mkBound 0 xsort
        local $ do
            bindQualified x idx xsort
            a <- encode p
            mkExists [] [sym] [xsort] a

    encode (PImpli p1 p2) = do
        a1 <- encode p1
        a2 <- encode p2
        mkImplies a1 a2

    encode (PAssert a) = encode a

instance Z3Reserved Int where
    def = -1 -- XXX: Magic number

instance Z3Sorted Int where
    sortPhantom _ = mkIntSort

instance Z3Encoded Int where
    encode i = mkIntSort >>= mkInt i

instance Z3Reserved Double where
    def = -1.0 -- XXX: Magic number

instance Z3Sorted Double where
    sortPhantom _ = mkRealSort

instance Z3Encoded Double where
    encode = mkRealNum

instance Z3Reserved Bool where
    def = False -- XXX: Magic number

instance Z3Sorted Bool where
    sortPhantom _ = mkBoolSort

instance Z3Encoded Bool where
    encode = mkBool
