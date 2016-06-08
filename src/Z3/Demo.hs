-- Demo of how to instantiate the Pred

module Z3.Demo where

import Z3.Core.Logic
import Z3.Core.Context
import Z3.Assertion
import Z3.Monad

import qualified Data.Map as M

data Term = TmVar String

data Type = TyBool
          | TyInt
          | TyDouble
          | TyMap Type Type
          | TySet Type
          -- | TyApp Type Type
          -- | TyADT String

type Z3Pred = Pred Term Type Assertion

instance Z3Encoded Term where
    encode (TmVar x) = do
        ctx <- getQualifierCtx
        case M.lookup x ctx of
            Just (idx, _) -> return idx
            Nothing -> smtError $ "Can't find variable " ++ x

instance Z3Sorted Term where
    sort (TmVar x) = do
        ctx <- getQualifierCtx
        case M.lookup x ctx of
            Just (_, s) -> return s
            Nothing -> smtError $ "Can't find variable " ++ x

instance Z3Sorted Type where
    sort TyBool     = mkBoolSort
    sort TyInt      = mkIntSort
    sort TyDouble   = mkRealSort
    sort (TyMap ty1 ty2) = do
        s1 <- sort ty1
        s2 <- sort ty2
        mkArraySort s1 s2
    sort (TySet ty) = do
        s <- sort ty
        intSort <- mkIntSort
        mkArraySort s intSort
