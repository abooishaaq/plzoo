{-# LANGUAGE LambdaCase #-}

module Eval where

import Control.Monad.Except (Except, ExceptT, MonadError (throwError), MonadIO (liftIO), runExcept, runExceptT)
import Control.Monad.Identity (Identity)
import Control.Monad.State (MonadState (get, put), State, StateT, evalState, foldM, modify, runState)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import Data.Maybe (isJust, isNothing)
import Syntax

data Value = Value
    { as_int :: Maybe Int
    , as_bool :: Maybe Bool
    , as_func :: Maybe Closure
    , fields :: Fields
    }
    deriving (Eq)

type Fields = Map.Map String Value

showInt :: Show a => Maybe a -> String
showInt (Just a) = show a
showInt Nothing = ""

showBool :: Maybe Bool -> [Char]
showBool (Just True) = "true"
showBool (Just False) = "false"
showBool _ = ""

showObj :: Fields -> String
showObj m =
    let l = Map.toList m
     in if null l
            then ""
            else "{" ++ foldl (\acc (k, v) -> acc ++ " " ++ k ++ "=" ++ show v ++ ",") "" l ++ " }"

instance Show Value where
    show (Value i b f o) =
        foldl
            (\x acc -> if not (null acc) then acc ++ (if not (null x) then " with " ++ x else "") else x)
            (showInt i)
            [ showBool b
            , if isJust f then "<fun>" else ""
            , showObj o
            ]

data Closure = Closure {this :: Maybe Value, arg :: String, env :: Env, expr :: Expr}
    deriving (Show, Eq)

type Env = Map.Map String Value

data EvalError = UnboundVariable String | FieldNotFound String | TypeExpected Type | InvalidThis
    deriving (Show)

data Type = TInt | TBool | TFunc | TObj
    deriving (Show)

emptyEnv :: Env
emptyEnv = Map.empty

unit_obj :: Value
unit_obj = Value{as_int = Nothing, as_bool = Nothing, as_func = Nothing, fields = Map.empty}

mkInt :: Int -> Value
mkInt i = Value{as_int = Just i, as_bool = Nothing, as_func = Nothing, fields = Map.empty}

mkBool :: Bool -> Value
mkBool b = Value{as_int = Nothing, as_bool = Just b, as_func = Nothing, fields = Map.empty}

mkFunc :: Maybe Closure -> Value
mkFunc f = Value{as_int = Nothing, as_bool = Nothing, as_func = f, fields = Map.empty}

mkObj :: Map.Map String Value -> Value
mkObj ob = Value{as_int = Nothing, as_bool = Nothing, as_func = Nothing, fields = ob}

arithOp :: ArithOp -> (Int -> Int -> Int)
arithOp Plus = (+)
arithOp Minus = (-)
arithOp Times = (*)
arithOp Divide = div
arithOp Remainder = rem

cmpOp :: Eq a => CmpOp -> a -> a -> Bool
cmpOp Equal = (==)
cmpOp Unequal = (/=)
cmpOp _ = error "expected Equal of Uneqaul"

boolOp :: BoolOp -> (Bool -> Bool -> Bool)
boolOp And = (&&)
boolOp Or = (||)

join :: Maybe a -> Maybe a -> Maybe a
join v1 v2 = case (v1, v2) of
    (_, v3@(Just v)) -> v3
    (v4, _) -> v4

modifyObj :: Fields -> Value -> [String] -> Except EvalError Fields
modifyObj f v [] = return f
modifyObj f v [x] = if Map.member x f then return (Map.update (const (Just v)) x f) else throwError $ FieldNotFound x
modifyObj f v (x : xs) = do
    if Map.member x f
        then do
            let v2 = f Map.! x
            let f1 = fields v2
            f2 <- modifyObj f1 v xs
            return $ Map.update (const (Just (v2{fields = f2}))) x f
        else throwError $ FieldNotFound x

eval :: Maybe Value -> Expr -> ExceptT EvalError (State Env) Value
eval this = \case
    Int i -> return $ mkInt i
    Bool b -> return $ mkBool b
    Skip -> return unit_obj
    Var v -> do
        env <- get
        if Map.member v env then return (env Map.! v) else throwError $ UnboundVariable v
    Negate e -> do
        v1 <- eval this e
        case as_int v1 of
            Just i -> return $ mkInt $ - i
            _ -> throwError $ TypeExpected TInt
    ArithOp op e1 e2 -> do
        v1 <- eval this e1
        v2 <- eval this e2
        case (as_int v1, as_int v2) of
            (Just i1, Just i2) -> return $ mkInt $ arithOp op i1 i2
            _ -> throwError $ TypeExpected TInt
    CmpOp op e1 e2 -> do
        v1 <- eval this e1
        v2 <- eval this e2
        case op of
            Less -> case (as_int v1, as_int v2) of
                (Just i1, Just i2) -> return $ mkBool $ i1 < i2
                _ -> throwError $ TypeExpected TInt
            More -> case (as_int v1, as_int v2) of
                (Just i1, Just i2) -> return $ mkBool $ i1 > i2
                _ -> throwError $ TypeExpected TInt
            _ -> return $ mkBool $ cmpOp op v1 v2
    Not e -> do
        v <- eval this e
        case as_bool v of
            (Just b) -> return $ mkBool $ not b
            _ -> throwError $ TypeExpected TBool
    BoolOp op e1 e2 -> do
        v1 <- eval this e1
        v2 <- eval this e2
        case (as_bool v1, as_bool v2) of
            (Just b1, Just b2) -> return $ mkBool $ boolOp op b1 b2
            _ -> throwError $ TypeExpected TBool
    If cond tr fl -> do
        v <- eval this cond
        case as_bool v of
            Just b -> if b then eval this tr else eval this fl
            _ -> throwError $ TypeExpected TBool
    Seq e1 e2 ->
        eval this e1 >> eval this e2
    Let x e1 e2 -> do
        v1 <- eval this e1
        env <- get
        put (Map.insert x v1 env)
        v2 <- eval this e2
        put env
        return v2
    App e1 e2 -> do
        v1 <- eval this e1
        v2 <- eval this e2
        case as_func v1 of
            Nothing -> throwError $ TypeExpected TFunc
            Just (Closure this1 arg env1 e2) -> do
                env <- get
                put (Map.insert arg v2 env1)
                v3 <- eval this1 e2
                put env
                return v3
    Fun x e -> do
        env <- get
        return $ mkFunc $ Just (Closure this x env e)
    This -> case this of
        Nothing -> throwError InvalidThis
        Just v -> return v
    Object lst -> do
        let keys = map fst lst
        let exs = map snd lst
        let this1 = Just $ mkObj Map.empty
        vs <- mapM (eval this1) exs
        return $ mkObj $ Map.fromList $ zip keys vs
    With e1 e2 -> do
        v1 <- eval this e1
        v2 <- eval this e2
        return $
            Value
                { as_int = join (as_int v1) (as_int v2)
                , as_bool = join (as_bool v1) (as_bool v2)
                , as_func = join (as_func v1) (as_func v2)
                , fields = Map.union (fields v2) (fields v1)
                }
    Project e x -> do
        obj <- eval this e
        let f = fields obj
        if Map.member x f
            then
                ( do
                    let v = f Map.! x
                    return $ case as_func v of
                        Nothing -> v
                        Just (Closure this1 arg env1 e2) -> mkFunc $ Just (Closure (Just obj) arg env1 e2)
                )
            else throwError $ FieldNotFound x
    Assign name xs ex1 -> do
        env <- get
        if Map.member name env
            then
                ( do
                    let v = env Map.! name
                        f = fields v
                    v2 <- eval this ex1
                    let res = runExcept (modifyObj f v2 xs)
                    case res of
                        (Left err) -> throwError err
                        Right f1 -> do
                            let v3 = v{fields = f1}
                            modify $ Map.update (const (Just v3)) name
                            return v2
                )
            else throwError $ UnboundVariable name

evalTop :: [TopLevel] -> StateT Env IO ()
evalTop [] = return ()
evalTop (t : ts) = do
    env <- get
    case t of
        Expr e -> do
            let (res, env1) = runState (runExceptT (eval Nothing e)) env
            put env1
            case res of
                Right v -> liftIO $ print v
                Left e -> liftIO $ print e
        Def name e -> do
            let (res, env1) = runState (runExceptT (eval Nothing e)) env
            put env1
            case res of
                Right v -> do
                    put (Map.insert name v env)
                    liftIO $ putStrLn $ name ++ " = " ++ show v
                Left e -> liftIO $ print e
    evalTop ts
