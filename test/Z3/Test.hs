module Z3.Test (spec) where

import Z3.Base.Class
import Z3.Base.Logic
import Z3.Z3Pred
import Z3.Base.Encoding
import Z3.Context
import Z3.Base.Atom
import Z3.Monad hiding (mkMap)

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import qualified Data.Set as S

import Test.Hspec

tests :: [(Z3Pred, Either String Result)]
tests = [
    (PTrue, Right Sat),
    (PFalse, Right Unsat),
    (PConj PTrue PFalse, Right Unsat),
    (PConj PTrue PTrue, Right Sat),
    (PDisj PTrue PFalse, Right Sat),
    (PDisj PFalse PFalse, Right Unsat),
    (PNeg PFalse, Right Sat),
    (PAtom (Equal (1 :: Int) (1 :: Int)), Right Sat),
    (PAtom (Equal True False), Right Unsat),
    (PForAll "x" TyInt PTrue, Right Sat),
    (PExists "x" TyInt (PAtom (Equal (TmVar "x") (1 :: Int))), Right Sat),
    (PForAll "x" TyInt (PImpli (PAtom (Less (TmVar "x") (0 :: Int)))
                               (PAtom (Less (TmVar "x") (1 :: Int)))), Right Sat),
    (PForAll "x" TyInt (PImpli (PAtom (Less (TmVar "x") (1 :: Int)))
                               (PAtom (Less (TmVar "x") (0 :: Int)))), Right Unsat),
    (PForAll "x" TyInt (PImpli PTrue PFalse), Right Unsat),
    (PAtom (InMap (1 :: Int) (1 :: Int) (M.singleton (1 :: Int) 1)), Right Sat),
    (PAtom (InSet (10 :: Int) (S.singleton (10 :: Int))), Right Sat),
    (PAtom (Equal (TmApp "none" Nil (TyADT "optionInt"))
                    (TmApp "none" Nil (TyADT "optionInt"))), Right Sat),
    (PForAll "x" (TyADT "optionInt") PTrue, Right Sat),
    (PAtom (Equal (TmApp "just" (Cons (1 :: Int) Nil) (TyADT "optionInt"))
                    (TmApp "just" (Cons (1 :: Int) Nil) (TyADT "optionInt"))), Right Sat),
    (PExists "x" TyInt $ PConj (PAtom $ GreaterE (TmSub (TmVar "x") (TmNum 1)) (3 :: Int))
                               (PAtom $ GreaterE (TmSub (TmSub (TmVar "x") (TmNum 2)) (TmNum 1)) (0 :: Int)), Right Sat),
    (PExists "x" TyInt $ PConj (PAtom $ Less (TmVar "x") (TmNum 0))
                               (PNeg $ PAtom $ LessE (TmVar "x") (TmNum 0)), Right Unsat),
    (PExists2 "x" "y" TyInt $ PConj (PAtom $ GreaterE (TmIf (TmLE (TmVar "x") (TmVar "y")) (TmVar "x") (TmVar "y")) $ TmVar "x")
                                    (PAtom $ GreaterE (TmIf (TmLE (TmVar "x") (TmVar "y")) (TmVar "x") (TmVar "y")) $ TmVar "y"), Right Sat),
    (PExists2 "x" "y" TyInt $ PConj (PConj (PAtom $ LessE (TmVar "x") $ TmNum 3)
                                           (PAtom $ LessE (TmVar "y") $ TmNum 3))
                                    (PAtom $ GreaterE (TmAdd (TmVar "x") (TmVar "y")) $ TmNum 9), Right Unsat),
    (PExists2 "x" "y" TyInt $ PNeg $ PConj (PAtom $ GreaterE (TmIf (TmLE (TmVar "x") (TmVar "y")) (TmVar "x") (TmVar "y")) $ TmVar "x")
                                           (PAtom $ GreaterE (TmIf (TmLE (TmVar "x") (TmVar "y")) (TmVar "x") (TmVar "y")) $ TmVar "y"), Right Sat)
    ]

checkPre :: Z3Pred -> Z3SMT () (Result, Maybe Model)
checkPre pre = local $ do
    ast <- encode pre
    local (assert ast >> getModel)

test :: (Z3Pred, Either String Result) -> IO ()
test (p, expected) = do
    let adts = [("optionInt", [("none", []),
                               ("just", [("val", TyInt)])])]
    ret <- runSMT adts () $ do
        (r, _mm) <- checkPre p
        case r of
            Unsat -> do
                core <- getUnsatCore
                liftIO $ sequence_ (map print core)
                return r
            other -> return other
    ret `shouldBe` expected

spec :: Spec
spec = forM_ tests $ (\pair@(p, expected) -> do
         it {-(show p ++ " → " ++ show expected )-}"no show" $
           test pair)
