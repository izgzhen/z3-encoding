module Z3.Test (spec) where

import Z3.Class
import Z3.Logic
import Z3.Demo
import Z3.Encoding
import Z3.Context
import Z3.Assertion
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
    (PAssert (Equal (1 :: Int) (1 :: Int)), Right Sat),
    (PAssert (Equal True False), Right Unsat),
    (PForAll "x" TyInt PTrue, Right Sat),
    (PExists "x" TyInt (PAssert (Equal (TmVar "x") (1 :: Int))), Right Sat),
    (PForAll "x" TyInt (PImpli (PAssert (Less (TmVar "x") (0 :: Int)))
                               (PAssert (Less (TmVar "x") (1 :: Int)))), Right Sat),
    (PForAll "x" TyInt (PImpli (PAssert (Less (TmVar "x") (1 :: Int)))
                               (PAssert (Less (TmVar "x") (0 :: Int)))), Right Unsat),
    (PForAll "x" TyInt (PImpli PTrue PFalse), Right Unsat),
    (PAssert (InMap (1 :: Int) (1 :: Int) (M.singleton (1 :: Int) 1)), Right Sat),
    (PAssert (InSet (10 :: Int) (S.singleton (10 :: Int))), Right Sat),
    (PAssert (Equal (TmApp "none" Nil (TyADT "optionInt"))
                    (TmApp "none" Nil (TyADT "optionInt"))), Right Sat),
    (PForAll "x" (TyADT "optionInt") PTrue, Right Sat),
    (PAssert (Equal (TmApp "just" (Cons (1 :: Int) Nil) (TyADT "optionInt"))
                    (TmApp "just" (Cons (1 :: Int) Nil) (TyADT "optionInt"))), Right Sat),
    (PExists "x" TyInt $ PConj (PAssert $ GreaterE (TmSub (TmVar "x") (TmNum 1)) (3 :: Int))
                               (PAssert $ GreaterE (TmSub (TmSub (TmVar "x") (TmNum 2)) (TmNum 1)) (0 :: Int)), Right Sat),
    (PExists "x" TyInt $ PConj (PAssert $ Less (TmVar "x") (TmNum 0))
                               (PNeg $ PAssert $ LessE (TmVar "x") (TmNum 0)), Right Unsat),
    (PExists "x" TyInt $ PExists "y" TyInt $ PConj (PAssert $ Greater (TmIf (TmLE (TmVar "x") (TmVar "y")) (TmVar "x") (TmVar "y")) $ TmVar "x")
                                                   (PAssert $ Greater (TmIf (TmLE (TmVar "x") (TmVar "y")) (TmVar "x") (TmVar "y")) $ TmVar "y"), Right Sat),
    (PExists "x" TyInt $ PExists "y" TyInt $ PConj (PConj (PAssert $ LessE (TmVar "x") $ TmNum 3)
                                                          (PAssert $ LessE (TmVar "y") $ TmNum 3))
                                                   (PAssert $ GreaterE (TmAdd (TmVar "x") (TmVar "y")) $ TmNum 9), Right Unsat)
    (PExists "x" TyInt $ PExists "y" TyInt $ PNeg $ PConj (PAssert $ GreaterE (TmIf (TmLE (TmVar "x") (TmVar "y")) (TmVar "x") (TmVar "y")) $ TmVar "x")
                                                          (PAssert $ GreaterE (TmIf (TmLE (TmVar "x") (TmVar "y")) (TmVar "x") (TmVar "y")) $ TmVar "y"), Right Sat)
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

