-- |
-- Prviding some Z3 encoding for certain language constructs
-- Require a Class.SMT context to work

module Z3.Base.Encoding (
  -- ** encode function application
  encodeApp,
  -- ** encode datatype definition
  encodeDataType
) where

import Z3.Base.Class
import Z3.Monad hiding (mkMap, App)

encodeApp :: SMT m e => String -> HeteroList -> Sort -> m e AST
encodeApp fname args retSort = do
    paramSorts <- sequence $ mapH sortOf args
    sym <- mkStringSymbol fname
    decl <- mkFuncDecl sym paramSorts retSort
    argASTs <- sequence $ mapH encode args
    mkApp decl argASTs

encodeDataType :: SMT m e => Z3Sorted ty => (String, [(String, [(String, ty)])]) -> m e Sort
encodeDataType (tyName, alts) = do
    constrs <- mapM (\(consName, fields) -> do
                        consSym <- mkStringSymbol consName
                        -- recognizer. e.g. is_None None = True, is_None (Some _) = False
                        recogSym <- mkStringSymbol ("is_" ++ consName)
                        flds <- flip mapM fields $ \(fldName, fldTy) -> do
                            symFld <- mkStringSymbol fldName
                            s <- sortOf fldTy
                            return (symFld, Just s, -1) -- XXX: non-rec
                        mkConstructor consSym recogSym flds
                    ) alts
    sym <- mkStringSymbol tyName
    mkDatatype sym constrs
