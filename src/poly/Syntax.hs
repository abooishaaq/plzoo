{-# LANGUAGE LambdaCase #-}

module Syntax where

import qualified Data.Map as Map

data Expr
    = Var String
    | Int Int
    | Bool Bool
    | Neg Expr
    | Times Expr Expr
    | Divide Expr Expr
    | Mod Expr Expr
    | Plus Expr Expr
    | Minus Expr Expr
    | Equal Expr Expr
    | Less Expr Expr
    | Cons Expr Expr
    | Pair Expr Expr
    | If Expr Expr Expr
    | Fun String Expr
    | Apply Expr Expr
    | Rec String Expr
    | List
    | Fst Expr
    | Snd Expr
    | Match Expr Expr String String Expr
    deriving (Show, Eq)

data TopLevel = Expr Expr | Def String Expr
    deriving (Show, Eq)

subst :: Map.Map String Expr -> Expr -> Expr
subst m = \case
    e@(Var x) -> if Map.member x m then m Map.! x else e
    e@List -> e
    e@(Int _) -> e
    e@(Bool _) -> e
    Neg e -> Neg (subst m e)
    Mod e1 e2 -> Mod (subst m e1) (subst m e2)
    Divide e1 e2 -> Times (subst m e1) (subst m e2)
    Times e1 e2 -> Times (subst m e1) (subst m e2)
    Minus e1 e2 -> Minus (subst m e1) (subst m e2)
    Plus e1 e2 -> Plus (subst m e1) (subst m e2)
    Less e1 e2 -> Less (subst m e1) (subst m e2)
    Cons e1 e2 -> Cons (subst m e1) (subst m e2)
    Equal e1 e2 -> Equal (subst m e1) (subst m e2)
    Apply e1 e2 -> Apply (subst m e1) (subst m e2)
    Pair e1 e2 -> Pair (subst m e1) (subst m e2)
    Fst e -> Fst (subst m e)
    Snd e -> Snd (subst m e)
    If e1 e2 e3 -> If (subst m e1) (subst m e2) (subst m e3)
    Fun x e -> let i = Map.findIndex x m in Fun x (subst (Map.deleteAt i m) e)
    Rec x e -> let i = Map.findIndex x m in Fun x (subst (Map.deleteAt i m) e)
    Match ex1 ex2 x y ex3 ->
        let i = Map.findIndex x m
         in let j = Map.findIndex y m
             in Match (subst m ex1) (subst m ex2) x y (subst (Map.deleteAt j (Map.deleteAt i m)) ex3)
