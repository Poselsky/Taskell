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
table = [[binary "*" Times Ex.AssocLeft,
          binary "/" Divide Ex.AssocLeft]
        ,[binary "+" Plus Ex.AssocLeft,
          binary "-" Minus Ex.AssocLeft]]

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
  return $ Var var "Void"

function :: Parser Expr 
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many variable
  let argumentNames = map show args
  body <- expr
  return $ Function "Int" name argumentNames body

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many variable
  let argumentNames = map show args
  return $ Extern name argumentNames

call :: Parser Expr  
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name (map show args)

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