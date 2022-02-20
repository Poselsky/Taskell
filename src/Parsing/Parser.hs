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
import Debug.Trace
import Data.Functor.Identity
import Control.Monad (guard, when)
import Parsing.Syntax
import Parsing.ParserStateHelpers (appendParentTracerState, appendVariable)
import GHC.Stack
import Data.Maybe (isJust)
import Data.Dynamic (Dynamic(Dynamic), toDyn)
import Parsing.DataTypeParsingHelper (possibleAssignParser, fromDataExpressionToExpr)
import Parsing.ParsingHelpers ((><))
import Parsing.OperatorTable
import Parsing.IndividualExpressions.VariableParser

int :: CustomParsec Expr
int = do
  n <- integer
  let returnInt = Int $ Just n
  return $ returnInt

floating :: CustomParsec Expr
floating = do
  n <- float
  return $ Float $ Just n

stringing:: CustomParsec Expr
stringing = do
  n <- stringLiteral
  return $ String $ Just n


--TODO: case where var; = 0 -> shouldn't be acceptable
variable:: CustomParsec Expr
variable = try (try emptyVarDeclaration <|> varWithAssignment) 

expr :: CustomParsec Expr
expr = Ex.buildExpressionParser (table ++ [return $ binary "=" (fromStringToOperator "=") Ex.AssocLeft]) factor

function:: CustomParsec Expr
function = do
  name <- identifier
  args <- choice [parens $ (varDeclaration `sepBy` between spaces spaces (string ",")), parens $ (return Void `sepBy` spaces)] 
  -- TODO:: Maybe use args as a string instead of whole Expr 
  -- Map is in form:: (varName, varType)
  updateParserState (\s@State{ stateUser = estate } -> s { stateUser = estate { blockTypes = Map.fromList (map (\a-> (getVarName a, getVarType a)) args)}})
  updateParserState (appendParentTracerState name)
  spaces `sepBy1` reservedOp ":"
  dataTypeInStr <- choice $ map string possibleDataTypesInString
  spaces
  body <- braces $ many expr
  -- return $ functionExpr bodyState name convertedArgs body
  return $ Function dataTypeInStr name args body

extern :: CustomParsec Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many varDeclaration 
  return $ Extern name args


ifexpr :: CustomParsec Expr
ifexpr = do
  reserved "if"
  spaces
  boolExpr <- between (char '(') (char ')') expr
  spaces
  tr <- braces expr
  -- TODO: ELIF statement here
  reserved "else"
  spaces
  fl <- braces expr
  return $ If boolExpr tr fl


factor :: CustomParsec Expr
factor = do
    try ifexpr
      <|> try variable
      <|> try (try function <|> call)
      <|> try (try floating <|> int)
      <|> parens expr

defn :: CustomParsec Expr
defn = do
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
