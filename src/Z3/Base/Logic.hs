-- | Predicates

module Z3.Base.Logic (Pred(..)) where

import Z3.Monad
import Z3.Base.Class
import Z3.Base.Atom

data Pred t ty a where
    PTrue   :: Pred t ty a
    PFalse  :: Pred t ty a
    PConj   :: Pred t ty a -> Pred t ty a -> Pred t ty a
    PDisj   :: Pred t ty a -> Pred t ty a -> Pred t ty a
    PXor    :: Pred t ty a -> Pred t ty a -> Pred t ty a
    PNeg    :: Pred t ty a -> Pred t ty a
    PForAll :: String -> ty -> Pred t ty a -> Pred t ty a
    PExists :: String -> ty -> Pred t ty a -> Pred t ty a
    PExists2 :: String -> String -> ty -> Pred t ty a -> Pred t ty a
    PImpli  :: Pred t ty a -> Pred t ty a -> Pred t ty a
    PIff    :: Pred t ty a -> Pred t ty a -> Pred t ty a
    PAtom   :: Atom -> Pred t ty a
    -- User-extension
    PExt    :: a -> Pred t ty a

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

    encode (PXor p1 p2) = do
        a1 <- encode p1
        a2 <- encode p2
        mkXor a1 a2

    encode (PNeg p) = encode p >>= mkNot

    encode (PForAll x ty p) = do
        sym <- mkStringSymbol x
        xsort <- sortOf ty
        -- "0" is de brujin idx for current binder
        -- it is passed to Z3 which returns an intenal (idx :: AST)
        -- This (idx :: AST) will be used to replace the variable
        -- in the abstraction body when encountered, thus it is stored
        -- in context by bindVal we provide
        -- XXX: we should save and restore qualifier context here
        idx <- mkBound 0 xsort
        local $ do
            bindVal x idx xsort
            body <- encode p
            -- The first [] is [Pattern], which is not really useful here
            mkForall [] [sym] [xsort] body

    encode (PExists x ty p) = do
        sym <- mkStringSymbol x
        xsort <- sortOf ty
        idx <- mkBound 0 xsort
        local $ do
            bindVal x idx xsort
            a <- encode p
            mkExists [] [sym] [xsort] a

    -- HACK
    encode (PExists2 x y ty p) = do
        sym1 <- mkStringSymbol x
        sym2 <- mkStringSymbol y
        xsort <- sortOf ty
        idx1 <- mkBound 0 xsort
        idx2 <- mkBound 1 xsort
        local $ do
            bindVal x idx1 xsort
            bindVal y idx2 xsort
            a <- encode p
            mkExists [] [sym1, sym2] [xsort, xsort] a

    encode (PImpli p1 p2) = do
        a1 <- encode p1
        a2 <- encode p2
        mkImplies a1 a2

    encode (PIff p1 p2) = do
        a1 <- encode p1
        a2 <- encode p2
        mkIff a1 a2

    encode (PAtom a)  = encode a

    encode (PExt ext) = encode ext
