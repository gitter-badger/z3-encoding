module Z3.Core.Encoding where

import Z3.Core.Context
import Z3.Core.Logic
import Z3.Monad hiding (mkMap)

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
