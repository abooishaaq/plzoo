{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Parser where

import Control.Applicative (Alternative (some), optional)
import Lexer
import Syntax
import Text.Parsec (ParseError, char, eof, many, many1, parse, sepBy, space, spaces, string, try, (<|>))
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Prelude hiding (seq)

prefix s c = Ex.Prefix (reservedOp s >> return c)

binary s c f = Ex.Infix (reservedOp s >> return (c f))

table =
    [
        [ prefix "-" Negate
        ]
    ,
        [ binary "%" ArithOp Remainder Ex.AssocLeft
        , binary "*" ArithOp Times Ex.AssocLeft
        , binary "/" ArithOp Divide Ex.AssocLeft
        ]
    ,
        [ binary "+" ArithOp Plus Ex.AssocLeft
        , binary "-" ArithOp Minus Ex.AssocLeft
        ]
    ,
        [ binary "<" CmpOp Less Ex.AssocLeft
         , binary ">" CmpOp More Ex.AssocLeft
        ]
    ,
        [ binary "=" CmpOp Equal Ex.AssocLeft
        , binary "<>" CmpOp Unequal Ex.AssocLeft
        ]
    ,
        [ prefix "not" Not
        ]
    ,
        [ binary "and" BoolOp And Ex.AssocLeft
        ]
    ,
        [ binary "or" BoolOp Or Ex.AssocLeft
        ]
    ]

int :: Parser Expr
int = do
    Int . fromInteger <$> natural

bool :: Parser Expr
bool =
    (reserved "true" >> return (Bool True))
        <|> (reserved "false" >> return (Bool False))

variable :: Parser Expr
variable = do
    Var <$> identifier

this :: Parser Expr
this = do
    reserved "this"
    return This

skip :: Parser Expr
skip = do
    reserved "skip"
    return Skip

field :: Parser (String, Expr)
field = do
    f <- identifier
    reservedOp "="
    e <- expr
    return (f, e)

obj :: Parser Expr
obj = do
    reservedOp "{"
    fs <- commaSep field <|> return []
    reservedOp "}"
    return $ Object fs

nonapp :: Parser Expr
nonapp = do
    parens expr
        <|> try bool
        <|> try int
        <|> variable
        <|> obj
        <|> this
        <|> skip

-- <|> copy

-- copy :: Parser Expr
-- copy = do
--     reserved "copy"
--     Copy <$> with

ifthen :: Parser Expr
ifthen = do
    reserved "if"
    ex1 <- expr
    reserved "then"
    ex2 <- expr
    reserved "else"
    ex3 <- expr
    return (If ex1 ex2 ex3)

fun :: Parser Expr
fun = do
    reserved "fun"
    v <- identifier
    reservedOp "->"
    Fun v <$> seqq

letin :: Parser Expr
letin = do
    reserved "let"
    v <- identifier
    reservedOp "="
    ex <- expr
    reserved "in"
    Let v ex <$> expr

proj :: Parser Expr
proj =
    nonapp >>= \x ->
        ( many1
            (reservedOp "." >> identifier)
            >>= \xs -> return (foldl Project x xs)
        )
            <|> return x

with :: Parser Expr
with =
    proj >>= \x ->
        ( many1
            (spaces >> reserved "with" >> spaces >> proj)
            >>= \xs -> return (foldl With x xs)
        )
            <|> return x

app :: Parser Expr
app =
    with >>= \x ->
        ( many1 with
            >>= \xs -> return (foldl App x xs)
        )
            <|> return x

assign :: Parser Expr
assign = do
    v <- identifier
    x <- many1 (reservedOp "." >> identifier)
    reserved ":="
    Assign v x <$> expr

term :: Parser Expr
term =
    try assign
        <|> app
        <|> fun
        <|> ifthen
        <|> letin

expr :: Parser Expr
expr =
    Ex.buildExpressionParser table term

seqq :: Parser Expr
seqq =
    expr >>= \x ->
        try
            ( many1 (reservedOp ";" >> seqq)
                >>= \xs ->
                    return (foldl Seq x xs)
            )
            <|> return x

exprTop :: Parser TopLevel
exprTop = do
    ex <- seqq
    reservedOp ";;"
    return $ Expr ex

defTop :: Parser TopLevel
defTop = do
    reserved "let"
    name <- identifier
    reservedOp "="
    ex <- seqq
    reservedOp ";;"
    return $ Def name ex

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

toplevel :: Parser [TopLevel]
toplevel = some $ do
    try defTop <|> exprTop

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError [TopLevel]
parseToplevel = parse (contents toplevel) "<stdin>"
