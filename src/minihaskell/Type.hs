module Type where

import Data.Map ( (!), member, Map )

data Type
    = TInt
    | TBool
    | TParam TVar
    | TTimes Type Type
    | TArrow Type Type
    | TList Type
  deriving (Eq, Ord)

newtype TVar = TV String
  deriving (Show, Eq, Ord)

isArrow :: Type -> Bool
isArrow (TArrow _ _) = True 
isArrow _ = False 

isList :: Type -> Bool
isList (TList _) = True 
isList _ = False 

parens :: [Char] -> [Char]
parens str = "( " ++ str ++ " )"

instance Show Type where
    show TInt = "int"
    show TBool = "bool"
    show (TParam (TV k)) = k
    show (TList t) = (if isList t then parens (show t) else show t )++ " list"
    show (TTimes t1 t2) =  show t1 ++ " * " ++ show t2
    show (TArrow t1 t2) = (if isArrow t1 then parens (show t1) else show t1) ++ " -> " ++ show t2
