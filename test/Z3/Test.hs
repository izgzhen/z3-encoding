{-# LANGUAGE ScopedTypeVariables #-}

module Z3.Test (spec) where

import Z3.Monad hiding (mkMap)

import Z3.Base.Class
import Z3.Base.Language
import Z3.Base.SMTR
import Z3.Context

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as S

import Test.Hspec

type R = SMTR Z3SMT ()

int :: Int -> R Int
int = lit

tests :: [(R Bool, Either String Result)]
tests = [
    (lit True, Right Sat),
    (lit False, Right Unsat),
    (lit True /\ lit False, Right Unsat),
    (lit True /\ lit True, Right Sat),
    (lit True \/ lit False, Right Sat),
    (lit False \/ lit False, Right Unsat),
    (not_ (lit False), Right Sat),
    (int 1 .= int 1, Right Sat),
    (lit True .= lit False, Right Unsat),
    (forall_ Z3Sort (\(_ :: R Int) -> (lit True)), Right Sat),
    (exists Z3Sort (\(x :: R Int) -> x .= int 1), Right Sat),
    (forall_ Z3Sort (\(x :: R Int) -> (x .< int 0) ==> (x .< int 1)), Right Sat),
    (forall_ Z3Sort (\(x :: R Int) -> (x .< int 1) ==> (x .< int 0)), Right Unsat),
    (forall_ Z3Sort (\(_ :: R Int) -> lit True ==> lit False), Right Unsat),
    -- (PAtom (InMap (1 :: Int) (1 :: Int) (M.singleton (1 :: Int) 1)), Right Sat),
    (int 10 `member_` lit (S.singleton 10), Right Sat),
    ((none :: R (Maybe Int)) .= none, Right Sat),
    (forall_ Z3Sort (\(_ :: R (Maybe Int)) -> lit True), Right Sat),
    (some (1 :: Int) .= some (1 :: Int), Right Sat),
    (exists Z3Sort (\(x :: R Int) ->
        ((x .- int 1) .>= int 3) /\ (((x .- int 2) .- int 1) .>= int 0)), Right Sat),
    (exists Z3Sort (\(x :: R Int) ->
        (x .< int 0) /\ not_ (x .<= int 0)), Right Unsat),
    (exists Z3Sort (\(x :: R Int) ->
        exists Z3Sort (\y ->
            (if_ (x .<= y) x y .>= x) /\ (if_ (x .<= y) x y .>= y))), Right Sat),
    (exists Z3Sort (\(x :: R Int) ->
        exists Z3Sort (\y ->
            ((x .<= int 3) /\ (y .<= int 3)) /\ ((x .+ y) .>= int 9))), Right Unsat)
    ]

test :: (SMTR Z3SMT () Bool, Either String Result) -> IO ()
test (m, expected) = do
    let adts = [("option", [("none", []),
                            ("some", [("unSome", Type $ sortOf (Z3Sort :: Z3Sort Int))])])]
    ret <- runSMT () adts $ do
        (r, _mm) <- checkPre m
        case r of
            Unsat -> do
                core <- getUnsatCore
                liftIO $ sequence_ (map print core)
                return r
            other -> return other
    ret `shouldBe` expected

spec :: Spec
spec = forM_ tests $ (\pair@(_, expected) -> it ("expected: " ++ show expected) $ test pair)
