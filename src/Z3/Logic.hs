{-# LANGUAGE ScopedTypeVariables #-}

module Z3.Logic where

data Pred t ty a = PTrue
                 | PFalse
                 | PConj (Pred t ty a) (Pred t ty a)
                 | PDisj (Pred t ty a) (Pred t ty a)
                 | PNeg (Pred t ty a)
                 | PForAll String ty (Pred t ty a)
                 | PExists String ty (Pred t ty a)
                 | PImpli (Pred t ty a) (Pred t ty a)
                 | PAssert a
                 deriving (Show, Eq)
