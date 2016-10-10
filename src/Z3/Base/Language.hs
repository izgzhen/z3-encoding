{-# LANGUAGE GADTs #-}

-- Inspired by http://okmij.org/ftp/tagless-final/course/TTF.hs

module Z3.Base.Language where

import Z3.Base.Class
import qualified Data.Set as S

class Language repr where
    lit :: Z3Lit a => a -> repr a
    add, sub, mul :: Num a => repr a -> repr a -> repr a
    div_ :: Fractional a => repr a -> repr a -> repr a
    mod_, rem :: Integral a => repr a -> repr a -> repr a
    neg :: Num a => repr a -> repr a
    equals :: Eq a => repr a -> repr a -> repr Bool
    lessThan, lessEqual, greaterThan, greaterEqual :: Ord a => repr a -> repr a -> repr Bool
    and_, or_, xor :: repr Bool -> repr Bool -> repr Bool
    not_ :: repr Bool -> repr Bool
    if_ :: repr Bool -> repr a -> repr a -> repr a
    forall_ :: Z3Sorted a => Z3Sort a -> (repr a -> repr b) -> repr Bool
    exists :: Z3Sorted a => Z3Sort a -> (repr a -> repr b) -> repr Bool
    member_ :: Ord a => repr a -> repr (S.Set a) -> repr Bool
