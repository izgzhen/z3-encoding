module Z3.Datatypes where

import Z3.Base.Class
import Z3.Monad hiding (mkMap, App)

encodeDataType :: SMT m e => (String, [(String, [(String, Type m e)])]) -> m e Sort
encodeDataType (tyName, alts) = do
    constrs <- mapM (\(consName, fields) -> do
                        consSym <- mkStringSymbol consName
                        -- recognizer. e.g. is_None None = True, is_None (Some _) = False
                        recogSym <- mkStringSymbol ("is_" ++ consName)
                        flds <- flip mapM fields $ \(fldName, fldTy) -> do
                            symFld <- mkStringSymbol fldName
                            s <- unType fldTy
                            return (symFld, Just s, -1) -- XXX: non-rec
                        mkConstructor consSym recogSym flds
                    ) alts
    sym <- mkStringSymbol tyName
    mkDatatype sym constrs

tlist :: SMT m e => [Type m e]
tlist = [Type $ sortOf (Z3Sort :: Z3Sort Int),
         Type $ sortOf (Z3Sort :: Z3Sort Bool)]

