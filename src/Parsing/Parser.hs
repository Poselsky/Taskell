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
import Control.Monad.State 

-- data FunctionBody =  

binary :: String -> Op -> Ex.Assoc -> Ex.Operator String () Identity Expr
binary s f assoc = Ex.Infix (reservedOp s >> return (BinaryOp f)) assoc

table :: [[Ex.Operator String () Identity Expr]]
table  = [[binary "*" (fromStringToOperator "*") Ex.AssocLeft,
          binary "/" (fromStringToOperator "/") Ex.AssocLeft]
        ,[binary "+" (fromStringToOperator "+") Ex.AssocLeft,
          binary "-" (fromStringToOperator "-") Ex.AssocLeft]]

int :: Parser (ExprS ExprState)
int = do
  n <- integer
  return $ intExpr n
    where
      intExpr:: Integer -> ExprS ExprState 
      intExpr n = do
        let returnInt = Int $ Just n 
        modify (\s -> s { currentExpr = returnInt })  
        get 
  
floating :: Parser (ExprS ExprState)
floating = do
  n <- float
  return $ floatExpr n
    where
      floatExpr:: Double -> ExprS ExprState 
      floatExpr n = do
        let returnFloat = Float $ Just n 
        modify (\s -> s { currentExpr = returnFloat })  
        get 

stringing:: Parser (ExprS ExprState)
stringing = do
  n <- stringLiteral
  return $ stringExpr n 
  where
    stringExpr:: String -> ExprS ExprState 
    stringExpr s = do
      let returnString = String $ Just s 
      modify (\s -> s { currentExpr = returnString })  
      get 

parseVar:: Parser (ExprS ExprState)
parseVar = do
  -- int somevar
  lookAhType <- choice parsecPossibleVarTypes
  -- reserved lookAhType
  spaces
  varName <- identifier
  return $ varExpr lookAhType varName
  where
    (x:xs) = possibleDataTypesInString
    parsecPossibleVarTypes = map string possibleDataTypesInString 
    toReserved = map reserved possibleDataTypesInString
    varExpr:: String -> String -> ExprS ExprState 
    varExpr t s = do
      let returnString = Var (fromStringToDataType t) s 
      modify (\s -> s { currentExpr = returnString })  
      get 

expr :: Parser (ExprS ExprState)
expr = do
  let evaluatedFactor = fmap (\x -> currentExpr $ evalState (runExprS x) emptyExprState) factor
  -- Building the table for expression parser
  let exparser  = Ex.buildExpressionParser table evaluatedFactor 
  x <- exparser
  return $ expr' x
    where
      expr':: Expr -> ExprS ExprState
      expr' x = do 
        modify (\s -> s { currentExpr = x})  
        get 

variable :: Parser (ExprS ExprState)
variable = parseVar 

function :: Parser (ExprS ExprState)
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many variable
  -- TODO:: Maybe use args as a string or Double instead of whole Expr 
  -- let argumentNames = map getVarName args
  -- We can start with empty expr
  let convertedArgs = map (\arg -> currentExpr $ evalState (runExprS arg) emptyExprState) args
  body <- fmap (\x -> currentExpr $ evalState (runExprS x) emptyExprState) expr
  return $ functionExpr name convertedArgs body
    where
      functionExpr:: String -> [Expr] -> Expr-> ExprS ExprState 
      functionExpr name args body = do
        let returningFunction = Function "Int" name args body
        modify (\s -> s { currentExpr = returningFunction })  
        get 

extern :: Parser (ExprS ExprState)
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many variable
  let convertedArgs = map (\arg -> currentExpr $ evalState (runExprS arg) emptyExprState) args
  return $ externExpr name convertedArgs
  where 
    externExpr:: String -> [Expr] -> ExprS ExprState 
    externExpr name convertedArgs = do
      let returningExtern = Extern name convertedArgs 
      modify (\s -> s { currentExpr = returningExtern })  
      get 

call :: Parser (ExprS ExprState)
call = do
  name <- identifier
  args <- parens $ commaSep expr
  let convertedArgs = map (\arg -> currentExpr $ evalState (runExprS arg) emptyExprState) args
  return $ callExpr name convertedArgs
  where
    callExpr:: String -> [Expr] -> ExprS ExprState 
    callExpr name args = do
      let returningCall = Call name args 
      modify (\s -> s { currentExpr = returningCall })  
      get 

factor :: Parser (ExprS ExprState)
factor =
    do
      try call
      <|> try function
      <|> try variable
      <|> try floating
      <|> try int
      <|> parens expr

defn :: Parser (ExprS ExprState)
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

toplevel :: Parser [ExprS ExprState]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = do
  parsedExpr <- parse (contents expr) "<stdin>" s
  let evaluated = currentExpr $ evalState (runExprS parsedExpr) emptyExprState
  return evaluated


parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = do
  parsedExprs <- parse (contents toplevel) "<stdin>" s
  let evaluated = fmap (\parsedExpr -> currentExpr $ evalState (runExprS parsedExpr) emptyExprState) parsedExprs
  return evaluated