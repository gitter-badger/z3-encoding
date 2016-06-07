{-# LANGUAGE RankNTypes, GADTs #-}

module Z3.Assertion where

import Z3.Context
import Z3.Encoding

data Assertion where
    InMap :: forall k v. (Z3Sorted k, Z3Reserved v) => k -> v -> M.Map k v -> Assertion
    InSet :: forall v. Z3Sorted v => v -> S.Set v -> Assertion
    Equal :: forall v. (Eq v, Z3Encoded v) => v -> v -> Assertion
    Less  :: forall v. (Ord v, Z3Encoded v) => v -> v -> Assertion

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

-- DON'T DELETE YET: might be useful to encode embedded ADT

-- instance Z3Encoded Term where
--     encode (TmVar v) = do
--         ctx <- get
--         case M.lookup v (_bindings ctx) of
--             Just val -> encode val
--             Nothing  -> case M.lookup v (_qualifierContext ctx) of
--                 Just idx -> return idx
--                 Nothing -> do -- zero arity func
--                     decl <- mkFunc v
--                     mkApp decl []
--     encode (TmVal pval) = encode pval
--     encode (TmApp f tms) = mkAppAST f tms

-- mkFunc :: String -> SMT FuncDecl
-- mkFunc fname = do
--     ctx <- get
--     case M.lookup fname (_funcContext ctx) of
--         Just ty -> do
--             let tys = flattenApp ty
--             paramSorts <- mapM tyToSort (init tys)
--             retSort <- tyToSort (last tys)
--             sym <- mkStringSymbol fname
--             mkFuncDecl sym paramSorts retSort
--         Nothing -> throwError $ "Unbound variable: " ++ fname

-- mkAppAST :: String -> [Term] -> SMT AST
-- mkAppAST fname args = do
--     argASTs <- mapM encode args
--     decl <- mkFunc fname
--     mkApp decl argASTs

-- flattenApp :: Type -> [Type]
-- flattenApp (TyApp t1 t2) = flattenApp t1 ++ flattenApp t2
-- flattenApp other = [other]

