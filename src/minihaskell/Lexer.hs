module Lexer where

import Data.Bool (Bool)
import Text.Parsec (char, letter, (<|>), ParsecT, many1, try)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Control.Monad.State
import Control.Monad.Identity (Identity)

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["->", "=>", "::", ";;", "%" , "(" , ")" , "*" , "+" , "," , "-" , "/" , ":" , "<" , "=" , "[" , "]" , "|" ]
    names = ["bool", "else", "false", "fst", "fun", "if", "int", "is", "let", "list", "match", "rec", "snd", "then", "true", "with"]
    style =
        emptyDef
            { Tok.commentLine = "--"
            , Tok.reservedOpNames = ops
            , Tok.reservedNames = names
            , Tok.identStart = letter <|> char '_'
            }


natural :: Parser Integer
natural = Tok.natural lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep1 lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep1 lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

count p = do
    many1 (try p >> modify (+1))
    get
