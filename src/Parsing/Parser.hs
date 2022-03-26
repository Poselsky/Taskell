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
import Text.Parsec.Expr

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


variable:: CustomParsec Expr
variable = do
  --Parsing order is important
  var <- (try varWithAssignment <|> varDeclaration )
  return var

expr :: CustomParsec Expr
expr = do
  fac <- exprParserWithAssign factor 
  return $ ExprList [fac]

exprParserWithAssign:: CustomParsec Expr -> CustomParsec Expr
exprParserWithAssign= Ex.buildExpressionParser (table ++ [return $ binary "=" (fromStringToOperator "=") Ex.AssocLeft]) 

exprParser:: CustomParsec Expr -> CustomParsec Expr
exprParser= Ex.buildExpressionParser table 

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
  body <- braces bodyExpr 
  -- return $ functionExpr bodyState name convertedArgs body
  return $ Function (fromStringToDataType dataTypeInStr) name (ExprList args) body

extern :: CustomParsec Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many varDeclaration 
  return $ Extern name (ExprList args)

ifexpr :: CustomParsec Expr
ifexpr = do
  reserved "if"
  spaces
  boolExpr <- between (char '(') (char ')') variableAfterAssignment
  spaces
  tr <- braces bodyExpr
  -- TODO: ELIF statement here
  reserved "else"
  spaces
  fl <- braces bodyExpr 
  --If expression should return two lists of expressions
  return $ If boolExpr tr fl 

--TODO: Refactor this
factor :: CustomParsec Expr
factor = do
    try ifexpr
    --var needs to end with ;, otherwise errors occur
    <|> (do var <- try variable; reservedOp ";"; return var) 
    <|> try (try function <|> (do callEx <- call; reservedOp ";"; return callEx))
    <|> parens expr

--TODO: we can add return expression 
bodyExpr:: CustomParsec Expr
bodyExpr = do
  -- Parse body seperated by ';'
  body <- many $ choice [
    try $ do var <- exprParser variable <|> getExistingVar; reservedOp ";"; return var
    , try $ do c <- exprParser call; reservedOp ";"; return c
    , ifexpr
    ] 

  return $ ExprList body 

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

toplevel :: CustomParsec Expr
toplevel = do
  a <- many defn
  return $ ExprList a

-- parseExpr :: String -> Either ParseError Expr
-- parseExpr s = do
--   parsedExpr <- runParser (contents expr) "<stdin>" s
--   return parsedExpr
--   -- let evaluated = currentExpr $ evalState (runExprS parsedExpr) emptyExprState
--   -- return evaluated


parseToplevel :: String -> Either ParseError Expr
parseToplevel s = do
  -- parsedExprs <- parse (contents toplevel) "<stdin>" s
  runParser (contents toplevel) emptyExprState "stdin" s
