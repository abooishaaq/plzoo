module Syntax where

data ArithOp = Plus | Minus | Times | Divide | Remainder
    deriving (Show, Eq)

data BoolOp = And | Or
    deriving (Show, Eq)

data CmpOp = Less | Equal | Unequal
    deriving (Show, Eq)

data Expr
    = Var String
    | Bool Bool
    | Int Int
    | Negate Expr
    | ArithOp ArithOp Expr Expr
    | Not Expr
    | CmpOp CmpOp Expr Expr
    | BoolOp BoolOp Expr Expr
    | If Expr Expr Expr
    | Skip
    | Seq Expr Expr
    | Let String Expr Expr
    | App Expr Expr
    | Fun String Expr
    | This
    | Object [(String, Expr)]
    | With Expr Expr
    | Project Expr String
    | Assign String [String] Expr
    deriving (Show, Eq)
    -- | Copy Expr

data TopLevel = Expr Expr | Def String Expr
    deriving (Show, Eq)