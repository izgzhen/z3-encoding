module Z3.Test (spec) where

import Z3.Context
import Z3.Logic
import Z3.Type
import Z3.Encoding
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
    (PAssert (AEq (TmVal (VInt 1)) (TmVal (VInt 1))), Right Sat),
    (PAssert (AEq (TmVal (VBool True)) (TmVal (VBool False))), Right Unsat),
    (PForAll "x" TyInt PTrue, Right Sat),
    (PExists "x" TyInt (PAssert (AEq (TmVar "x") (TmVal (VInt 1)))), Right Sat),
    (PForAll "x" TyInt (PImpli (PAssert (ALess (TmVar "x") (TmVal (VInt 0))))
                               (PAssert (ALess (TmVar "x") (TmVal (VInt 1))))), Right Sat),
    (PForAll "x" TyInt (PImpli (PAssert (ALess (TmVar "x") (TmVal (VInt 1))))
                               (PAssert (ALess (TmVar "x") (TmVal (VInt 0))))), Right Unsat),
    (PForAll "x" TyInt (PImpli PTrue PFalse), Right Unsat),
    (PAssert (AInMap (TmVal (VInt 1)) (TmVal (VInt 1))
                     (TmVal (VMap (M.singleton (VInt 1) (VInt 1))))), Right Sat),
    (PAssert (AInSet (TmVal (VInt 10))
                     (TmVal (VSet (S.singleton (VInt 10))))), Right Sat),
    (PAssert (AEq (TmVar "none") (TmVar "none")), Right Sat),
    (PForAll "x" (TyADT "optionInt") PTrue, Right Sat),
    (PAssert (AEq (TmApp "just" [TmVal (VInt 1)]) (TmApp "just" [TmVal (VInt 1)])), Right Sat)
    ]

checkPre :: Z3Pred -> SMT (Result, Maybe Model)
checkPre pre = local $ do
    ast <- mkAST pre
    local (assert ast >> getModel)

test :: (Z3Pred, Either String Result) -> IO ()
test (p, expected) = do
    let adts = [("optionInt", [("none", []),
                               ("just", [("val", TyInt)])])]
    ret <- runSMT (Decls M.empty adts) $ do
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
         it (show p ++ " → " ++ show expected ) $
           test pair)

