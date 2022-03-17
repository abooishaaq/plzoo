{-# LANGUAGE LambdaCase #-}

module Eval where

import Control.Monad.Except (lift)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import Syntax
import Type (Type)

type Thunk = () -> IO Value

data Value
    = VInt Int
    | VBool Bool
    | VList Type
    | VPair Value Value
    | VClosure (Thunk -> IO Value)
    | VRecursive  (Thunk -> IO Value)

instance Show Value where
    show (VInt i) = show i
    show (VBool True) = "true"
    show (VBool False) = "false"
    show (VPair v1 v2) = "(" ++ show v1 ++ "," ++ show v2 ++ ")"
    show (VList ty) = "[" ++ show ty ++ "]"
    show (VClosure _) = "<closure>"
    show (VRecursive _) = "<recursive>"

type Env = Map.Map String (IORef Thunk)

update :: IORef Thunk -> Value -> IO ()
update ref v = do
    writeIORef ref (\() -> return v)
    return ()

force :: IORef Thunk -> IO Value
force ref = do
    th <- readIORef ref
    v <- th ()
    update ref v
    return v

mkThunk :: Env -> String -> Expr -> (Thunk -> IO Value)
mkThunk env x body a = do
    a' <- newIORef a
    eval (Map.insert x a' env) body

eval :: Env -> Expr -> IO Value
eval env = do
    \case
        Int i -> return $ VInt i
        Bool b -> return $ VBool b
        Var v -> force $ env Map.! v
        List ty -> return $ VList ty
        Neg e -> do
            v <- eval env e
            case v of
                VInt i -> return (VInt (- i))
                vv -> error $ "expected int, got " ++ show vv
        Times e1 e2 -> do
            v1 <- eval env e1
            v2 <- eval env e2
            case (v1, v2) of
                (VInt i1, VInt i2) -> return (VInt (i1 * i2))
                vv -> error $ "expected int, got " ++ show vv
        Minus e1 e2 -> do
            v1 <- eval env e1
            v2 <- eval env e2
            case (v1, v2) of
                (VInt i1, VInt i2) -> return (VInt (i1 - i2))
                vv -> error $ "expected int, got " ++ show vv
        Plus e1 e2 -> do
            v1 <- eval env e1
            v2 <- eval env e2
            case (v1, v2) of
                (VInt i1, VInt i2) -> return (VInt (i1 + i2))
                vv -> error $ "expected int, got " ++ show vv
        Divide e1 e2 -> do
            v1 <- eval env e1
            v2 <- eval env e2
            case (v1, v2) of
                (VInt i1, VInt i2) -> return (VInt (i1 `div` i2))
                vv -> error $ "expected int, got " ++ show vv
        Mod e1 e2 -> do
            v1 <- eval env e1
            v2 <- eval env e2
            case (v1, v2) of
                (VInt i1, VInt i2) -> return (VInt (i1 `rem` i2))
                vv -> error $ "expected int, got " ++ show vv
        Less e1 e2 -> do
            v1 <- eval env e1
            v2 <- eval env e2
            case (v1, v2) of
                (VInt i1, VInt i2) -> return (VBool (i1 < i2))
                vv -> error $ "expected int, got " ++ show vv
        Equal e1 e2 -> do
            v1 <- eval env e1
            v2 <- eval env e2
            case (v1, v2) of
                (VInt i1, VInt i2) -> return (VBool (i1 == i2))
                (VBool i1, VBool i2) -> return (VBool (i1 == i2))
                vv -> error $ "expected int, got " ++ show vv
        If e1 e2 e3 -> do
            v1 <- eval env e1
            case v1 of
                (VBool True) -> eval env e2
                (VBool False) -> eval env e3
                _ -> error "expected bool"
        Pair e1 e2 -> do
            v1 <- eval env e1
            v2 <- eval env e2
            return (VPair v1 v2)
        Fst e -> do
            v <- eval env e
            case v of
                VPair v1 v2 -> return v1
                _ -> error "expected pair"
        Snd e -> do
            v <- eval env e
            case v of
                VPair v1 v2 -> return v2
                _ -> error "expected pair"
        Fun x _ e -> do
            return $ VClosure (mkThunk env x e)
        Rec x _ e -> do
            return $ VRecursive (mkThunk env x e)
        Apply e1 e2 -> do
            v1 <- eval env e1
            v2 <- eval env e2
            case v1 of
                (VClosure th) -> th (\() -> return v2)
                (VRecursive th) -> do
                    v <- th (\() -> return v1)
                    case v of 
                        (VClosure th2) -> th2 (\() -> return v2)
                        _ -> error "expected closure"
                _ -> error "expected closure"
