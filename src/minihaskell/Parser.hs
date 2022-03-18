{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Parser where

import Control.Applicative (Alternative (some))
import Control.Monad.State (evalState, runState)
import Lexer
import Syntax
import Text.Parsec (ParseError, char, eof, many, many1, optional, parse, sepBy, space, spaces, string, try, (<|>))
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Type hiding (parens)

prefix s f = Ex.Prefix (reservedOp s >> return f)

binary s f = Ex.Infix (reservedOp s >> return f)

table =
    [
        [ prefix "-" Neg
        ]
    ,
        [ binary "%" Mod Ex.AssocLeft
        , binary "*" Times Ex.AssocLeft
        , binary "/" Divide Ex.AssocLeft
        ]
    ,
        [ binary "+" Plus Ex.AssocLeft
        , binary "-" Minus Ex.AssocLeft
        ]
    ,
        [ binary "<" Less Ex.AssocLeft
        ]
    ,
        [ binary "=" Equal Ex.AssocLeft
        ]
    ]

int :: Parser Expr
int = Int . fromInteger <$> natural

bool :: Parser Expr
bool =
    (reserved "true" >> return (Bool True))
        <|> (reserved "false" >> return (Bool False))

variable :: Parser Expr
variable = Var <$> identifier

pair :: Parser Expr
pair = do
    reservedOp "("
    ex1 <- expr
    optional spaces
    reservedOp ","
    optional spaces
    ex2 <- expr
    reservedOp ")"
    return $ Pair ex1 ex2

ifthen :: Parser Expr
ifthen = do
    reserved "if"
    ex1 <- expr
    reserved "then"
    ex2 <- expr
    reserved "else"
    ex3 <- expr
    return $ If ex1 ex2 ex3

tySimple :: Parser Type
tySimple =
    (reserved "int" >> return TInt)
        <|> (reserved "bool" >> return TBool)
        <|> parens ty

tyList :: Parser Type
tyList =
    tySimple >>= \x ->
        ( sepBy (reserved "list") spaces
            >>= \y -> return (foldr ($) x (replicate (length y) TList))
        )
            <|> return x

tyTimes :: Parser Type
tyTimes =
    tyList >>= \x ->
        ( many1
            (reservedOp "*" >> tyList)
            >>= \xs -> return (foldl TTimes x xs)
        )
            <|> return x

ty :: Parser Type
ty =
    tyTimes >>= \x ->
        ( many1
            (reservedOp "->" >> ty)
            >>= \xs -> return (foldl TArrow x xs)
        )
            <|> return x

list :: Parser Type
list = do
    reservedOp "["
    typ <- ty
    reservedOp "]"
    return typ

fun :: Parser Expr
fun = do
    reserved "fun"
    v <- identifier
    reservedOp ":"
    typ <- ty
    reservedOp "=>"
    Fun v typ <$> expr

recis :: Parser Expr
recis = do
    reserved "rec"
    v <- identifier
    reservedOp ":"
    typ <- ty
    reserved "is"
    Rec v typ <$> expr

match :: Parser Expr
match = do
    reserved "match"
    ex1 <- expr
    reserved "with"
    t <- list
    reservedOp "=>"
    ex2 <- expr
    reservedOp "|"
    v1 <- identifier
    reservedOp "::"
    v2 <- identifier
    reservedOp "=>"
    ex3 <- expr
    return $ Match ex1 t ex2 v1 v2 ex3

fstt :: Parser Expr
fstt = do
    reserved "fst"
    Fst <$> expr

sndd :: Parser Expr
sndd = do
    reserved "snd"
    Fst <$> expr

aexp :: Parser Expr
aexp =
    try (parens expr)
        <|> pair
        <|> ifthen
        <|> bool
        <|> int
        <|> variable
        <|> fun
        <|> recis
        <|> match
        <|> fstt
        <|> sndd
        <|> (List <$> list)

cons :: Parser Expr
cons =
    aexp >>= \x ->
        ( many1
            (reservedOp "::" >> aexp)
            >>= \xs -> return (foldl Cons x xs)
        )
            <|> return x

app :: Parser Expr
app =
    cons >>= \x ->
        (many1 aexp >>= \xs -> return (foldl Apply x xs))
            <|> return x

term :: Parser Expr
term = app

expr :: Parser Expr
expr =
    Ex.buildExpressionParser table term

exprTop :: Parser TopLevel
exprTop = do
    Expr <$> expr

defTop :: Parser TopLevel
defTop = do
    reserved "let"
    name <- identifier
    reservedOp "="
    Def name <$> expr

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

toplevel :: Parser [TopLevel]
toplevel = some $ do
    t <- try defTop <|> exprTop
    optional $ reservedOp ";;"
    return t

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError [TopLevel]
parseToplevel = parse (contents toplevel) "<stdin>"
