module Z3.Base.Hlist where

import Z3.Base.Class

data HeteroList where
    Cons :: forall a. (Z3Sorted a, Z3Prim a) => a -> HeteroList -> HeteroList
    Nil :: HeteroList

instance Eq HeteroList where
  Nil == Nil = True
  Cons _ h1 == Cons _ h2 = h1 == h2
  _ == _ = False

mapH :: (forall a. (Z3Sorted a, Z3Prim a) => a -> b) -> HeteroList -> [b]
mapH _ Nil = []
mapH f (Cons a l) = f a : mapH f l
