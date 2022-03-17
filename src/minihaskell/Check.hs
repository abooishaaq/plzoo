{-# LANGUAGE LambdaCase #-}

module Check where

import Control.Monad.Except (Except, MonadError (throwError))
import Control.Monad.State (StateT)
import qualified Data.Map as Map
import Syntax
import Type

type TypeEnv = Map.Map String Type

data TypeError = TypeError {expected :: [Type], got :: Type} | UnboundVariable String
    deriving (Show)

check :: TypeEnv -> Type -> Expr -> Except TypeError ()
check env t1 expr = do
    t2 <- typeOf env expr
    if t1 == t2 then return () else throwError TypeError{expected = [t1], got = t2}

typeOf :: TypeEnv -> Expr -> Except TypeError Type
typeOf env =
    \case
        Int i -> return TInt
        Bool b -> return TBool
        List t -> return (TList t)
        Var v ->
            if Map.member v env
                then return (env Map.! v)
                else throwError $ UnboundVariable v
        Neg e -> do
            check env TInt e
            return TInt
        Times e1 e2 -> do
            check env TInt e1
            check env TInt e2
            return TInt
        Plus e1 e2 -> do
            check env TInt e1
            check env TInt e2
            return TInt
        Minus e1 e2 -> do
            check env TInt e1
            check env TInt e2
            return TInt
        Divide e1 e2 -> do
            check env TInt e1
            check env TInt e2
            return TInt
        Mod e1 e2 -> do
            check env TInt e1
            check env TInt e2
            return TInt
        Equal e1 e2 -> do
            t1 <- typeOf env e1
            t2 <- typeOf env e2
            if (t1 == TInt && t2 == TInt) || (t1 == TBool && t2 == TBool)
                then return TBool
                else throwError (TypeError{expected = [TInt, TBool], got = t2})
        Less e1 e2 -> do
            check env TInt e1
            check env TInt e2
            return TBool
        Cons e1 e2 -> do
            t <- typeOf env e1
            check env (TList t) e2
            return (TList t)
        If e1 e2 e3 -> do
            check env TBool e1
            t1 <- typeOf env e2
            check env t1 e3
            return t1
        Pair e1 e2 -> do
            t1 <- typeOf env e1
            t2 <- typeOf env e2
            return (TTimes t1 t2)
        Fst e -> do
            t <- typeOf env e
            case t of
                TTimes t1 t2 -> return t1
                _ -> throwError (TypeError{expected = [TTimes (TParam (TV "a")) (TParam (TV "b"))], got = t})
        Snd e -> do
            t <- typeOf env e
            case t of
                TTimes t1 t2 -> return t2
                _ -> throwError (TypeError{expected = [TTimes (TParam (TV "a")) (TParam (TV "b"))], got = t})
        Fun x t1 e -> do
            t2 <- typeOf (Map.insert x t1 env) e
            return (TArrow t1 t2)
        Rec x t1 e -> do
            check (Map.insert x t1 env) t1 e
            return t1
        Match e1 t1 e2 x y e3 -> do
            t2 <- typeOf env e1
            case t2 of
                TList t3 -> do
                    t5 <- typeOf env e2
                    check (Map.insert x t1 (Map.insert x (TList t1) env)) t5 e3
                    return t5
                t4 -> throwError (TypeError{expected = [TList (TParam (TV "a"))], got = t4})
        Apply e1 e2 -> do
            t1 <- typeOf env e1
            case t1 of
                TArrow t2 t3 -> do
                    check env t2 e2
                    return t3
                t4 -> throwError (TypeError{expected = [TArrow (TParam (TV "a")) (TParam (TV "b"))], got = t4})
