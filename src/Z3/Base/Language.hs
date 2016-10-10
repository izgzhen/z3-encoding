{-# LANGUAGE GADTs #-}

-- Inspired by http://okmij.org/ftp/tagless-final/course/TTF.hs

module Z3.Base.Language where

import Z3.Base.Class
import qualified Data.Set as S

class Language repr where
    lit :: Z3Lit a => a -> repr a
    (.+), (.-), (.*) :: Num a => repr a -> repr a -> repr a
    (./) :: Fractional a => repr a -> repr a -> repr a
    mod_, rem :: Integral a => repr a -> repr a -> repr a
    neg :: Num a => repr a -> repr a
    (.=) :: Eq a => repr a -> repr a -> repr Bool
    (.<), (.<=), (.>), (.>=) :: Ord a => repr a -> repr a -> repr Bool
    (/\), (\/), xor :: repr Bool -> repr Bool -> repr Bool
    not_ :: repr Bool -> repr Bool
    if_ :: repr Bool -> repr a -> repr a -> repr a
    forall_ :: Z3Sorted a => Z3Sort a -> (repr a -> repr b) -> repr Bool
    exists :: Z3Sorted a => Z3Sort a -> (repr a -> repr b) -> repr Bool
    (==>) :: repr Bool -> repr Bool -> repr Bool
    member_ :: Ord a => repr a -> repr (S.Set a) -> repr Bool

class Language repr => OptLang repr where
    none :: repr (Maybe a)
    some :: Z3Lit a => a -> repr (Maybe a)
