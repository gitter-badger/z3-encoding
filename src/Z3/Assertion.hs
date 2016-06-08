{-# LANGUAGE LambdaCase, RankNTypes, GADTs #-}

module Z3.Assertion where

import Z3.Context
import Z3.Encoding()
import Z3.Monad

import qualified Data.Map as M
import qualified Data.Set as S

data Assertion where
    InMap :: forall k v. (Z3Sorted k, Z3Encoded k, Z3Sorted v, Z3Reserved v) => k -> v -> M.Map k v -> Assertion
    InSet :: forall v. (Z3Encoded v, Z3Sorted v) => v -> S.Set v -> Assertion
    Equal :: forall v1 v2. (Z3Encoded v1, Z3Encoded v2) => v1 -> v2 -> Assertion
    Less  :: forall v1 v2. (Z3Encoded v1, Z3Encoded v2) => v1 -> v2 -> Assertion

-- instance Show Assertion where
--     show = \case
--         InMap k v m -> "in_map(" ++ show k ++ ", " ++ show v ++ ", " ++ show m ++ ")"
--         InSet v s   -> "in_set(" ++ show v ++ ", " ++ show s ++ ")"
--         Equal v1 v2 -> show v1 ++ " = " ++ show v2
--         Less v1 v2  -> show v1 ++ " <= " ++ show v2

instance Z3Encoded Assertion where
    encode (InMap k v m) = do
        kTm <- encode k
        vTm <- encode v
        mTm <- encode m
        lhs <- mkSelect mTm kTm
        mkEq lhs vTm
    encode (InSet e s) = do
        eTm <- encode e
        sTm <- encode s
        lhs <- mkSelect sTm eTm
        one <- (mkIntSort >>= mkInt 1)
        mkEq one lhs
    encode (Equal t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkEq a1 a2
    encode (Less t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkLe a1 a2
