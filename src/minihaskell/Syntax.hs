{-# LANGUAGE LambdaCase #-}

module Syntax where

import Type
import qualified Data.Map as Map

data Expr
    = Var String
    | Int Integer
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
    | Fun String Type Expr
    | Apply Expr Expr
    | Rec String Type Expr
    | List Type
    | Fst Expr
    | Snd Expr
    | Match Expr Type Expr String String Expr
    deriving (Show, Eq)

data TopLevel = Expr Expr | Def String Expr
    deriving (Show, Eq)
