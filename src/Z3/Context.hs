{-# LANGUAGE ScopedTypeVariables #-}

module Z3.Context (
    Z3Encoded,
    Z3Sorted,
    Z3Sort(..),
    Z3Reserved,
    Z3Sort,
    sortPhantom,
    encode,
    sort,
    def,
    SMT,
    runSMT,
    smtError,
    genFreshId,
    getQualifierCtx,
    bindQualified
) where

import Z3.Monad

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

data Z3Sort a = Z3Sort

class Z3Encoded a where
    encode :: a -> SMT AST

class Z3Sorted a where
    sort :: a -> SMT Sort
    sort _ = sortPhantom (Z3Sort :: Z3Sort a)

    sortPhantom :: Z3Sort a -> SMT Sort
    sortPhantom _ = smtError "sort error"

class Z3Encoded a => Z3Reserved a where
    def :: a

data SMTContext = SMTContext {
    -- Globally free bindings
    -- _bindings :: M.Map String Value,
    -- Functions
    -- _funcContext :: M.Map String Type,
    -- Bind local variables introduced by qualifiers to de brujin index in Z3
    _qualifierContext :: M.Map String (AST, Sort),
    -- From name to Z3 sort
    -- _datatypeCtx :: M.Map String Sort,
    -- Counter used to generate globally unique ID
    _counter :: Int
} deriving (Show, Eq)

type SMT = ExceptT String (StateT SMTContext Z3)

instance MonadZ3 SMT where
  getSolver  = lift (lift getSolver)
  getContext = lift (lift getContext)

genFreshId :: SMT Int
genFreshId = do
    i <- _counter <$> get
    modify (\ctx -> ctx { _counter = i + 1 })
    return i

runSMT :: SMT a -> IO (Either String a)
runSMT smt = evalZ3With Nothing opts m
    where
        opts = opt "MODEL" True
        m = evalStateT (runExceptT smt)
                       (SMTContext M.empty 0)

bindQualified :: String -> AST -> Sort -> SMT ()
bindQualified x idx s = modify $ \ctx ->
        ctx { _qualifierContext = M.insert x (idx, s) (_qualifierContext ctx) }


getQualifierCtx :: SMT (M.Map String (AST, Sort))
getQualifierCtx = _qualifierContext <$> get

smtError :: String -> SMT a
smtError = throwError
