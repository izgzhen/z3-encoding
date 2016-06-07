{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, GADTs #-}

module Z3.Core.Logic where

data Pred t ty a where
    PTrue   :: Pred t ty a
    PFalse  :: Pred t ty a
    PConj   :: Pred t ty a -> Pred t ty a -> Pred t ty a
    PDisj   :: Pred t ty a -> Pred t ty a -> Pred t ty a
    PNeg    :: Pred t ty a -> Pred t ty a
    PForAll :: String -> ty -> Pred t ty a -> Pred t ty a
    PExists :: String -> ty -> Pred t ty a -> Pred t ty a
    PImpli  :: Pred t ty a -> Pred t ty a -> Pred t ty a
    PAssert :: a -> Pred t ty a
    deriving (Show)
