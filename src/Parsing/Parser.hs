{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Parsing.Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import qualified Data.Int as I
import qualified Data.Map as Map

import Parsing.Lexer
import Parsing.Syntax
import Debug.Trace
import Data.Functor.Identity
import Control.Monad (guard)

binary :: String -> Op -> Ex.Assoc -> Ex.Operator String ExprState Identity Expr
binary s f assoc = Ex.Infix (reservedOp s >> return (BinaryOp f)) assoc

table :: [[Ex.Operator String ExprState Identity Expr]]
table  = [[binary "*" (fromStringToOperator "*") Ex.AssocLeft,
          binary "/" (fromStringToOperator "/") Ex.AssocLeft]
        ,[binary "+" (fromStringToOperator "+") Ex.AssocLeft,
          binary "-" (fromStringToOperator "-") Ex.AssocLeft]]

int :: CustomParsec Expr
int = do
  n <- integer
  let returnInt = Int $ Just n
  updateParserState (\s -> s { stateUser = emptyExprState })
  return $ returnInt

floating :: CustomParsec Expr
floating = do
  n <- float
  return $ Float $ Just n

stringing:: CustomParsec Expr
stringing = do
  n <- stringLiteral
  return $ String $ Just n

parseVar:: CustomParsec Expr
parseVar = do
  varType <- choice parsecPossibleVarTypes
  spaces
  varName <- identifier
  typeMap <- blockTypes <$> getState
  case Map.lookup varName typeMap of
    Just nameFromMap -> do
      unexpected $ "Variable " ++ varName ++ " has already type"
    Nothing -> do
      return $ Var (fromStringToDataType varType) varName
  where
    parsecPossibleVarTypes = map string possibleDataTypesInString

parseVarWithExistingType:: CustomParsec Expr
parseVarWithExistingType = do
  spaces
  varName <- identifier
  --if varName is datatype - then fail
  guard $ varName `notElem` possibleDataTypesInString
  typeMap <- blockTypes <$> getState
  case Map.lookup varName typeMap of
    Just nameFromMap -> do
      return $ Var (fromStringToDataType nameFromMap) varName
    Nothing -> do
      unexpected $ "Non-existing variable " ++ varName ++ " should be assigned with type"


expr :: CustomParsec Expr 
expr = Ex.buildExpressionParser table factor

variable:: CustomParsec Expr
variable = parseVarWithExistingType <|> parseVar

function:: CustomParsec Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ (parseVar `sepBy` between spaces spaces (string ",")) 
  -- TODO:: Maybe use args as a string instead of whole Expr 
  -- Map is in form:: (varName, varType)
  updateParserState (\s@State{ stateUser = estate } -> s { stateUser = estate { blockTypes = Map.fromList (map (\a-> (getVarName a, getVarType a)) args) }})
  body <- expr
  -- return $ functionExpr bodyState name convertedArgs body
  return $ Function "int" name args body

extern :: CustomParsec Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many parseVar
  return $ Extern name args

call :: CustomParsec Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

factor :: CustomParsec Expr
factor =
    do
      try call
      <|> try function
      <|> try variable
      <|> try floating
      <|> try int
      <|> parens expr

defn :: CustomParsec Expr
defn =
  try extern
  <|> try function
  <|> expr

contents :: CustomParsec a -> CustomParsec a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: CustomParsec [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def

-- parseExpr :: String -> Either ParseError Expr
-- parseExpr s = do
--   parsedExpr <- runParser (contents expr) "<stdin>" s
--   return parsedExpr
--   -- let evaluated = currentExpr $ evalState (runExprS parsedExpr) emptyExprState
--   -- return evaluated


parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = do
  -- parsedExprs <- parse (contents toplevel) "<stdin>" s
  runParser (contents toplevel) emptyExprState "stdin" s
