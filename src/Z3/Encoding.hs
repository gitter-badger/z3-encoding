{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Z3.Encoding where

import Z3.Context
import Z3.Logic
import Z3.Monad hiding (mkMap)

import qualified Data.Map as M
import qualified Data.Set as S

instance (Z3Sorted t, Z3Sorted ty, Z3Encoded a) => Z3Encoded (Pred t ty a) where
    encode PTrue = mkTrue
    encode PFalse = mkFalse
    encode (PConj p1 p2) = do
        a1 <- encode p1
        a2 <- encode p2
        mkAnd [a1, a2]

    encode (PDisj p1 p2) = do
        a1 <- encode p1
        a2 <- encode p2
        mkOr [a1, a2]
    encode (PNeg p) = encode p >>= mkNot

    encode (PForAll x ty p) = do
        sym <- mkStringSymbol x
        xsort <- sort ty
        idx <- mkBound 0 xsort
        local $ do
            bindQualified x idx xsort
            a <- encode p
            mkForall [] [sym] [xsort] a

    encode (PExists x ty p) = do
        sym <- mkStringSymbol x
        xsort <- sort ty
        idx <- mkBound 0 xsort
        local $ do
            bindQualified x idx xsort
            a <- encode p
            mkExists [] [sym] [xsort] a

    encode (PImpli p1 p2) = do
        a1 <- encode p1
        a2 <- encode p2
        mkImplies a1 a2

    encode (PAssert a) = encode a

instance (Z3Sorted v, Z3Encoded v) => Z3Encoded (S.Set v) where
    encode s = do
        setSort <- sort s
        fid <- genFreshId
        arr <- mkFreshConst ("set" ++ "_" ++ show fid) setSort
        mapM_ (\e -> do
            ast <- encode e
            sel <- mkSelect arr ast
            one <- (mkIntSort >>= mkInt 1)
            mkEq sel one >>= assert) (S.toList s)
        arrValueDef <- mkArrayDefault arr
        zero <- (mkIntSort >>= mkInt 0)
        mkEq zero arrValueDef >>= assert
        return arr

instance Z3Sorted v => Z3Sorted (S.Set v) where
    sortPhantom _ = do
        sortElem <- sortPhantom (Z3Sort :: Z3Sort v)
        intSort <- mkIntSort
        mkArraySort sortElem intSort

instance (Z3Sorted k, Z3Encoded k, Z3Sorted v, Z3Reserved v) => Z3Encoded (M.Map k v) where
    encode m = do
        fid <- genFreshId
        arrSort <- sort m
        arr <- mkFreshConst ("map" ++ "_" ++ show fid) arrSort
        mapM_ (\(k, v) -> do
            kast <- encode k
            vast <- encode v
            sel <- mkSelect arr kast
            mkEq sel vast >>= assert) (M.toList m)
        arrValueDef <- mkArrayDefault arr
        vdef <- encode (def :: v)
        mkEq arrValueDef vdef >>= assert
        return arr

instance (Z3Sorted k, Z3Sorted v) => Z3Sorted (M.Map k v) where
    sortPhantom _ = do
        sk <- sortPhantom  (Z3Sort :: Z3Sort k)
        sv <- sortPhantom  (Z3Sort :: Z3Sort v)
        mkArraySort sk sv

instance Z3Reserved Int where
    def = -1 -- XXX: Magic number

instance Z3Sorted Int where
    sortPhantom _ = mkIntSort

instance Z3Encoded Int where
    encode i = mkIntSort >>= mkInt i

instance Z3Reserved Double where
    def = -1.0 -- XXX: Magic number

instance Z3Sorted Double where
    sortPhantom _ = mkRealSort

instance Z3Encoded Double where
    encode = mkRealNum

instance Z3Reserved Bool where
    def = False -- XXX: Magic number

instance Z3Sorted Bool where
    sortPhantom _ = mkBoolSort

instance Z3Encoded Bool where
    encode = mkBool
