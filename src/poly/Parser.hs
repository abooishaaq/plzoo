module Parser where

import Control.Applicative (Alternative (some))
import Lexer
import Syntax
import Text.Parsec (ParseError, char, eof, many, many1, optional, parse, sepBy, space, spaces, string, try, (<|>))
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

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
        <|> (reserved "false" >> return (Bool True))

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

fun :: Parser Expr
fun = do
    reserved "fun"
    v <- identifier
    reservedOp "=>"
    Fun v <$> expr

recis :: Parser Expr
recis = do
    reserved "rec"
    v <- identifier
    reserved "is"
    Rec v <$> expr

match :: Parser Expr
match = do
    reserved "match"
    ex1 <- expr
    reserved "with"
    list
    reservedOp "=>"
    ex2 <- expr
    reservedOp "|"
    v1 <- identifier
    reservedOp "::"
    v2 <- identifier
    reservedOp "=>"
    ex3 <- expr
    return $ Match ex1 ex2 v1 v2 ex3

list :: Parser Expr
list = do
    reservedOp "["
    reservedOp "]"
    return List

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
        <|> list

cons :: Parser Expr
cons =
    aexp >>= \x ->
        ( many1
            (spaces >> reservedOp "::" >> spaces >> aexp)
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
