module Z3.Encoding (
  HeteroList(..),
  encodeApp,
  encodeDataType
) where

import Z3.Class
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
                            s <- sort fldTy
                            return (symFld, Just s, -1) -- XXX: non-rec
                        mkConstructor consSym recogSym flds
                    ) alts
    sym <- mkStringSymbol tyName
    mkDatatype sym constrs
