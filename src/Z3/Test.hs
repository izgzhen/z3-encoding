module Z3.Test where

import Z3.Context
import Z3.Logic
import Z3.Type
import Z3.Encoding
import Z3.Monad hiding (mkMap)

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M

checkPre :: Pred -> SMT (Result, Maybe Model)
checkPre pre = local $ do
    ast <- mkAST pre
    local (assert ast >> getModel)

test :: IO ()
test = flip mapM_ tests $ \(p, expected) -> do
    ret <- runSMT M.empty $ do
        (r, mm) <- checkPre p
        case mm of
            Just model -> do
                modelStr <- showModel model
                if length modelStr > 0 then
                    liftIO $ putStrLn ("Model: " ++ modelStr ++ ".")
                else liftIO $ putStrLn "No model."
            Nothing -> liftIO $ putStrLn "No model."
        case r of
            Unsat -> do
                core <- getUnsatCore
                liftIO $ sequence_ (map print core)
                return r
            other -> return other

    if ret == expected
        then putStrLn $ "√ Passed: " ++ show p
        else putStrLn $ "× Failed: " ++ show p ++ ", error: " ++ show ret

tests :: [(Pred, Either String Result)]
tests = [
    (PTrue, Right Sat),
    (PFalse, Right Unsat),
    (PConj PTrue PFalse, Right Unsat),
    (PConj PTrue PTrue, Right Sat),
    (PDisj PTrue PFalse, Right Sat),
    (PDisj PFalse PFalse, Right Unsat),
    (PNeg PFalse, Right Sat),
    (PCmp CEq (TmVal (VInt 1)) (TmVal (VInt 1)), Right Sat),
    (PCmp CEq (TmVal (VBool True)) (TmVal (VBool False)), Right Unsat),
    (PForAll "x" TyInt PTrue, Right Sat),
    (PExists "x" TyInt (PCmp CEq (TmVar "x") (TmVal (VInt 1))), Right Sat),
    (PForAll "x" TyInt (PImpli (PCmp CLess (TmVar "x") (TmVal (VInt 0)))
                               (PCmp CLess (TmVar "x") (TmVal (VInt 1)))), Right Sat),
    (PForAll "x" TyInt (PImpli (PCmp CLess (TmVar "x") (TmVal (VInt 1)))
                               (PCmp CLess (TmVar "x") (TmVal (VInt 0)))), Right Unsat),
    (PForAll "x" TyInt (PImpli PTrue PFalse), Right Unsat),
    (PAssert (AInMap (TmVal (VInt 1)) (TmVal (VInt 1))
                     (TmVal (VMap (M.singleton (VInt 1) (VInt 1))))), Right Sat)
    ]

test2 = runSMT M.empty $ do
    mAst <- mkMap (M.fromList [(VInt 1, VInt 2),
                               (VInt 2, VInt 4),
                               (VInt (-1), VInt (-1)) -- HACK: guard
                              ])
    -- assert mAst
    (r, Just m) <- getModel
    ms <- showModel m
    liftIO (putStrLn $ "Model: " ++ ms)
    return r

