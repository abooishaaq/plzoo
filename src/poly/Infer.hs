{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Infer where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import Syntax
import Syntax (Expr (Fst))
import Type (TVar (TV), Type (..))

type Infer a =
    ( StateT
        InferState -- Inference state
        ( Except -- Inference errors
            TypeError
        )
        a -- Result
    )

newtype InferState = InferState {count :: Int}

initInfer :: InferState
initInfer = InferState{count = 0}

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

type Solve a = ExceptT TypeError Identity a

data TypeError
    = UnificationFail Type Type
    | InfiniteType TVar Type
    | UnboundVariable String
    | Ambigious [Constraint]
    | UnificationMismatch [Type] [Type]
    deriving (Eq)

instance Show TypeError where
    show (UnificationFail a b) =
        concat ["Cannot unify types: \n\t", show a, "\nwith \n\t", show b]
    show (UnificationMismatch a b) =
        concat ["Unification mismatch: \n\t", show a, "\nwith \n\t", show b]
    show (InfiniteType (TV a) b) =
        concat ["Cannot construct the infinite type: ", a, " = ", show b]
    show (Ambigious cs) =
        concat ["Cannot not match expected type: '" ++ show a ++ "' with actual type: '" ++ show b ++ "'\n" | (a, b) <- cs]
    show (UnboundVariable a) = "Not in scope: " ++ a

newtype Subst = Subst (Map.Map TVar Type)
    deriving (Eq, Ord, Show, Monoid, Semigroup)

newtype Env = Env (Map.Map String Type)
    deriving (Eq, Ord, Show, Monoid, Semigroup)

emptySubst :: Subst
emptySubst = mempty

emptyEnv :: Env
emptyEnv = Env Map.empty

extend :: Env -> (String, Type) -> Env
extend (Env env) (x, s) = Env $ Map.insert x s env

compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (tsubst (Subst s1)) s2 `Map.union` s1

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

fresh :: Infer Type
fresh = do
    s <- get
    put s{count = count s + 1}
    return $ TParam $ TV (letters !! count s)

occurs :: TVar -> Type -> Bool
occurs k =
    \case
        TInt -> False
        TBool -> False
        TParam t -> t == k
        TArrow t1 t2 -> occurs k t1 || occurs k t2
        TTimes t1 t2 -> occurs k t1 || occurs k t2
        TList t -> occurs k t

tsubst :: Subst -> Type -> Type
tsubst s@(Subst m) =
    \case
        TInt -> TInt
        TBool -> TBool
        t@(TParam k) -> if Map.member k m then m Map.! k else t
        TTimes t1 t2 -> TTimes (tsubst s t1) (tsubst s t2)
        TArrow t1 t2 -> TArrow (tsubst s t1) (tsubst s t2)
        TList t -> TList (tsubst s t)

csubst :: Subst -> Constraint -> Constraint
csubst s c = (tsubst s fir, tsubst s sec)
  where
    fir = fst c
    sec = snd c

lookupEnv :: Env -> String -> Infer Type
lookupEnv (Env env) x = do
    if Map.member x env
        then return $ env Map.! x
        else throwError $ UnboundVariable x

arithOp :: Type
arithOp = TInt `TArrow` (TInt `TArrow` TInt)

retType :: Type -> Type
retType (TArrow t1 t2) = t2
retType t = t

infer :: Env -> Expr -> Infer (Type, [Constraint])
infer env = do
    \case
        Int _ -> return (TInt, [])
        Bool _ -> return (TBool, [])
        List -> do
            tv <- fresh
            return (TList tv, [])
        Var v -> do
            t1 <- lookupEnv env v
            return (t1, [])
        Neg e -> do
            (t, c) <- infer env e
            return (t, (t, TInt) : c)
        Plus e1 e2 -> do
            (t1, c1) <- infer env e1
            (t2, c2) <- infer env e2
            tv <- fresh
            let u1 = t1 `TArrow` (t2 `TArrow` tv)
            return (tv, c1 ++ c2 ++ [(u1, arithOp)])
        Times e1 e2 -> do
            (t1, c1) <- infer env e1
            (t2, c2) <- infer env e2
            tv <- fresh
            let u1 = t1 `TArrow` (t2 `TArrow` tv)
            return (tv, c1 ++ c2 ++ [(u1, arithOp)])
        Minus e1 e2 -> do
            (t1, c1) <- infer env e1
            (t2, c2) <- infer env e2
            tv <- fresh
            let u1 = t1 `TArrow` (t2 `TArrow` tv)
            return (tv, c1 ++ c2 ++ [(u1, arithOp)])
        Divide e1 e2 -> do
            (t1, c1) <- infer env e1
            (t2, c2) <- infer env e2
            tv <- fresh
            let u1 = t1 `TArrow` (t2 `TArrow` tv)
            return (tv, c1 ++ c2 ++ [(u1, arithOp)])
        Mod e1 e2 -> do
            (t1, c1) <- infer env e1
            (t2, c2) <- infer env e2
            tv <- fresh
            let u1 = t1 `TArrow` (t2 `TArrow` tv)
            return (tv, c1 ++ c2 ++ [(u1, arithOp)])
        Equal e1 e2 -> do
            (t1, c1) <- infer env e1
            (t2, c2) <- infer env e2
            tv1 <- fresh
            tv2 <- fresh
            let u1 = t1 `TArrow` (t2 `TArrow` tv1)
                u2 = tv2 `TArrow` (tv2 `TArrow` TBool)
            return (tv1, c1 ++ c2 ++ [(u1, arithOp)])
        Less e1 e2 -> do
            (t1, c1) <- infer env e1
            (t2, c2) <- infer env e2
            tv <- fresh
            let u1 = t1 `TArrow` (t2 `TArrow` tv)
                u2 = TInt `TArrow` (TInt `TArrow` TBool)
            return (tv, c1 ++ c2 ++ [(u1, u2)])
        Cons e1 e2 -> do
            (t1, c1) <- infer env e1
            (t2, c2) <- infer env e2
            tv <- fresh
            let t3 = TList t1
                t4 = TList tv
            return (t4, c1 ++ c2 ++ [(t3, t2), (t3, t4)])
        Pair e1 e2 -> do
            (t1, c1) <- infer env e1
            (t2, c2) <- infer env e2
            return (TTimes t1 t2, c1 ++ c2)
        Fst e -> do
            tv1 <- fresh
            tv2 <- fresh
            (t1, c1) <- infer env e
            return (tv1, (t1, TTimes tv1 tv2) : c1)
        Snd e -> do
            tv1 <- fresh
            tv2 <- fresh
            (t1, c1) <- infer env e
            return (tv2, (t1, TTimes tv1 tv2) : c1)
        If cond tr fl -> do
            (t1, c1) <- infer env cond
            (t2, c2) <- infer env tr
            (t3, c3) <- infer env fl
            return (t2, c1 ++ c2 ++ c3 ++ [(t1, TBool), (t2, t3)])
        Apply e1 e2 -> do
            (t1, c1) <- infer env e1
            (t2, c2) <- infer env e2
            tv <- fresh
            return (tv, (t1, t2 `TArrow` tv) : c1 ++ c2)
        Rec x e -> do
            tv <- fresh
            (t1, c1) <- infer (extend env (x, tv)) e
            return (tv, (t1, tv) : c1)
        Fun x e -> do
            tv <- fresh
            (t1, c1) <- infer (extend env (x, tv)) e
            return (tv `TArrow` t1, c1)
        Match e1 e2 x y e3 -> do
            tv <- fresh
            (t1, c1) <- infer env e1
            (t2, c2) <- infer env e2
            (t3, c3) <- infer (env `extend` (x, tv) `extend` (y, TList tv)) e3
            return (t2, c1 ++ c2 ++ c3 ++ [(t1, TList tv), (t2, t3)])

inferTop :: Env -> [TopLevel] -> Either TypeError Env
inferTop env [] = Right env
inferTop env ((Expr ex) : xs) = case inferExpr env ex of
    Left err -> Left err
    Right ty -> inferTop env xs
inferTop env ((Def name ex) : xs) = case inferExpr env ex of
    Left err -> Left err
    Right ty -> inferTop (extend env (name, ty)) xs

inferExpr :: Env -> Expr -> Either TypeError Type
inferExpr env ex = case runInfer env (infer env ex) of
    Left err -> Left err
    Right (ty, cs) -> case runSolve cs of
        Left err -> Left err
        Right subst -> Right $ tsubst subst ty

runInfer :: Env -> Infer (Type, [Constraint]) -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalStateT m initInfer

runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solver (emptySubst, cs)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) = do
    su1 <- unifies t1 t2
    su2 <- unifyMany (map (tsubst su1) ts1) (map (tsubst su1) ts2)
    return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (TParam v) t = v `bind` t
unifies t (TParam v) = v `bind` t
unifies (TArrow t1 t2) (TArrow t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TList t1) (TList t2) = unifies t1 t2
unifies (TTimes t1 t2) (TTimes t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

solver :: Unifier -> Solve Subst
solver (su, cs) =
    case cs of
        [] -> return su
        ((t1, t2) : cs0) -> do
            su1 <- unifies t1 t2
            solver (su1 `compose` su, map (csubst su1) cs0)

bind :: TVar -> Type -> Solve Subst
bind a t
    | t == TParam a = return emptySubst
    | occurs a t = throwError $ InfiniteType a t
    | otherwise = return (Subst $ Map.singleton a t)
