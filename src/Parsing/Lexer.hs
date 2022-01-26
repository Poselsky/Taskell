module Parsing.Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok
import Text.Parsec.Token (GenLanguageDef(caseSensitive, commentStart, commentEnd))
import Parsing.Syntax (possibleDataTypesInString, ExprState)
import Text.Parsec
import Control.Monad.Identity

lexer :: Tok.TokenParser ExprState 
lexer = Tok.makeTokenParser style
  where
    ops = ["+","*","-",";", "<", ">", "==", "!=", ">=", "<="]
    names = ["def","extern", "ret", "if", "else"] 
    style = emptyDef {
               Tok.reservedOpNames = ops
             , Tok.reservedNames = names
             , caseSensitive = True
             , commentStart = "{-"
             , commentEnd = "-}"
             , Tok.commentLine = "#"
            }


type CustomParsec a = ParsecT String ExprState Identity a

integer :: CustomParsec Integer
integer = Tok.integer lexer

float :: CustomParsec Double
float = Tok.float lexer

parens :: CustomParsec a -> CustomParsec a
parens = Tok.parens lexer

commaSep :: CustomParsec a -> CustomParsec [a]
commaSep = Tok.commaSep lexer

semiSep :: CustomParsec a -> CustomParsec [a]
semiSep = Tok.semiSep lexer

identifier :: CustomParsec String
identifier = Tok.identifier lexer

reserved :: String -> CustomParsec ()
reserved = Tok.reserved lexer

reservedOp :: String -> CustomParsec ()
reservedOp = Tok.reservedOp lexer

stringLiteral:: CustomParsec String
stringLiteral = Tok.stringLiteral lexer

braces:: CustomParsec a -> CustomParsec a
braces = Tok.braces lexer

