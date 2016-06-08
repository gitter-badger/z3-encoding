{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, GADTs #-}

module Z3.Logic where

import Z3.Context

import qualified Data.Map as M

data Pred t a where
    PTrue   :: Pred t a
    PFalse  :: Pred t a
    PConj   :: Pred t a -> Pred t a -> Pred t a
    PDisj   :: Pred t a -> Pred t a -> Pred t a
    PNeg    :: Pred t a -> Pred t a
    PForAll :: Binder -> Pred t a -> Pred t a
    PExists :: Binder -> Pred t a -> Pred t a
    PImpli  :: Pred t a -> Pred t a -> Pred t a
    PAssert :: a -> Pred t a

data Binder where
    Binder :: forall ty. Z3Sorted ty => String -> Binder

instance Z3Encoded Binder where
    encode (Binder x) = do
        ctx <- getQualifierCtx
        case M.lookup x ctx of
            Just idx -> return idx
            Nothing -> smtError $ "Can't find binder " ++ show x
            -- Nothing -> do -- zero arity func
                -- decl <- mkFunc v
                -- mkApp decl []
