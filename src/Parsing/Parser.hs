{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Parsing.Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import qualified Data.Int as I

import Parsing.Lexer
import Parsing.Syntax
import Debug.Trace
import Data.Functor.Identity

binary :: String -> Op -> Ex.Assoc -> Ex.Operator String () Identity Expr 
binary s f assoc = Ex.Infix (reservedOp s >> return (BinaryOp f)) assoc

table :: [[Ex.Operator String () Identity Expr ]]
table = [[binary "*" (fromStringToOperator "*") Ex.AssocLeft,
          binary "/" (fromStringToOperator "/") Ex.AssocLeft]
        ,[binary "+" (fromStringToOperator "+") Ex.AssocLeft,
          binary "-" (fromStringToOperator "-") Ex.AssocLeft]]

int :: Parser Expr
int = do
  n <- integer
  return $ Int n

floating :: Parser Expr 
floating = do
  n <- float
  return $ Float n

expr :: Parser Expr 
expr = Ex.buildExpressionParser table factor

variable :: Parser Expr
variable = do
  var <- identifier
  -- TODO: Parse types
  return $ Var var var 

function :: Parser Expr 
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many variable
  -- TODO:: Maybe use args as a string or Double instead of whole Expr 
  -- let argumentNames = map getVarName args
  body <- expr
  return $ Function "Int" name args body

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many variable
  return $ Extern name args 

call :: Parser Expr  
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

factor :: Parser Expr 
factor =
    do
      try call
      <|> try function
      <|> try call
      <|> try variable
      <|> try floating
      <|> try int
      <|> parens expr

defn :: Parser Expr 
defn = 
  try extern
  <|> try function
  <|> expr
    
contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s