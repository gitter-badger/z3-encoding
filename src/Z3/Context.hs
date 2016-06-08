module Z3.Context (
    Z3Sort(..),
    Z3Encoded,
    Z3Sorted,
    Z3Reserved,
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

-- data Decls = Decls {
    -- _valBindings  :: M.Map String Value --,
    -- _datatypes :: [(String, [(String, [(String, Type)])])]
-- } deriving (Show, Eq)


data Z3Sort t = Z3Sort

class Z3Encoded a where
    encode :: a -> SMT AST

class Z3Encoded a => Z3Sorted a where
    sort :: Z3Sort a -> SMT Sort

class Z3Sorted a => Z3Reserved a where
    def :: a

data SMTContext = SMTContext {
    -- Globally free bindings
    -- _bindings :: M.Map String Value,
    -- Functions
    -- _funcContext :: M.Map String Type,
    -- Bind local variables introduced by qualifiers to de brujin index in Z3
    _qualifierContext :: M.Map String AST,
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

runSMT :: {- Decls -> -} SMT a -> IO (Either String a)
runSMT {- decls -} smt = evalZ3With Nothing opts m
    where
        opts = opt "MODEL" True
        -- funcs = constructFuncs (_datatypes decls)
        -- smt' = do
            -- sorts <- mapM initDataType (_datatypes decls)
            -- let datatypeCtx = M.fromList (zip (map fst (_datatypes decls)) sorts)
            -- modify $ \ctx -> ctx { _funcContext = funcs,
            --                        _datatypeCtx = datatypeCtx }
            -- smt
        m = evalStateT (runExceptT smt)
                       (SMTContext M.empty 0) --(_valBindings decls) M.empty M.empty M.empty 0)

-- initDataType :: (String, [(String, [(String, Type)])]) -> SMT Sort
-- initDataType (tyName, alts) = do
--     constrs <- mapM (\(consName, fields) -> do
--                         consSym <- mkStringSymbol consName
--                         recogSym <- mkStringSymbol ("is_" ++ consName)
--                         flds <- flip mapM fields $ \(fldName, fldTy) -> do
--                             symFld <- mkStringSymbol fldName
--                             sort <- tyToSort fldTy
--                             return (symFld, Just sort, -1) -- XXX: non-rec
--                         mkConstructor consSym recogSym flds
--                     ) alts
--     sym <- mkStringSymbol tyName
--     sort <- mkDatatype sym constrs
--     return sort

-- constructFuncs :: [(String, [(String, [(String, Type)])])] -> M.Map String Type
-- constructFuncs = M.fromList . concat . concatMap f
--     where
--         f (tyName, alts) =
--             flip map alts $ \(consName, fields) ->
--                 let ftys = map snd fields
--                     ty   = TyADT tyName
--                     cons = (consName, consApp ftys ty)
--                     dess = map (\(desName, fty) -> (desName, TyApp ty fty)) fields
--                 in cons : dess

bindQualified :: String -> AST -> SMT ()
bindQualified x idx = modify $ \ctx ->
        ctx { _qualifierContext = M.insert x idx (_qualifierContext ctx) }


getQualifierCtx :: SMT (M.Map String AST)
getQualifierCtx = _qualifierContext <$> get

smtError :: String -> SMT a
smtError = throwError
