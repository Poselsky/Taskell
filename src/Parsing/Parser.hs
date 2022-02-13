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

binary :: String -> Op -> Ex.Assoc -> Ex.Operator String ExprState Identity Expr
binary s f assoc = Ex.Infix (reservedOp s >> return (BinaryOp f)) assoc

unary :: String -> Op -> Ex.Operator String ExprState Identity Expr
unary s f = Ex.Prefix (reservedOp s >> return (UnaryOp f))

table :: [[Ex.Operator String ExprState Identity Expr]]
table  = [
          [
            binary "*" (fromStringToOperator "*") Ex.AssocLeft,
            binary "/" (fromStringToOperator "/") Ex.AssocLeft
          ],
          [
            binary "+" (fromStringToOperator "+") Ex.AssocLeft,
            binary "-" (fromStringToOperator "-") Ex.AssocLeft
          ],
          [
            binary "==" (fromStringToOperator "==") Ex.AssocLeft,
            binary "!=" (fromStringToOperator "!=") Ex.AssocLeft
          ],
          [
            binary ">" (fromStringToOperator ">") Ex.AssocLeft,
            binary "<" (fromStringToOperator "<") Ex.AssocLeft,
            binary "<=" (fromStringToOperator "<=") Ex.AssocLeft,
            binary ">=" (fromStringToOperator ">=") Ex.AssocLeft
          ]
        ]

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

parseVar:: HasCallStack => CustomParsec Expr
parseVar = do
  let res = fmap reserved possibleDataTypesInString
  let reservedDataTypes = foldr (<|>) (head res) (tail res)
  varType <- lookAhead $ choice parsecPossibleVarTypes
  reservedDataTypes
  spaces
  varName <- identifier
  typeMap <- blockTypes <$> getState
  -- Return bare variable
  case Map.lookup varName typeMap of
    Just nameFromMap -> do
      unexpected $ "Variable " ++ varName ++ " has already type"
    Nothing -> do
      let returningExpr = Var (fromStringToDataType varType) varName
      updateParserState (appendVariable returningExpr)
      return returningExpr
  where
    parsecPossibleVarTypes = map string possibleDataTypesInString

parseVarWithExistingType:: HasCallStack => CustomParsec Expr
parseVarWithExistingType = do
  varName <- identifier >< "parsing varname"
  --if varName is datatype - then fail
  (guard $ varName `notElem` possibleDataTypesInString) >< "Not passing guard"
  typeMap <- blockTypes <$> getState
  case Map.lookup varName typeMap of
    Just nameFromMap -> do
      return $ Var (fromStringToDataType nameFromMap) varName
    Nothing -> do
      unexpected $ "Non-existing variable " ++ "\"" ++ varName ++ "\"" ++ " should be assigned with type"

parseAssign:: HasCallStack => Expr -> CustomParsec Expr
parseAssign var@(Var t name) = do
  spaces >< "parsing spaces"
  reservedOp "=" >< "parsing =" 
  spaces  
  -- All possible values after assignment
  val <- choice $ fromDataExpressionToExpr possibleAssignParser
  --TODO: Guard types
  return $ BinaryOp Assign var val
  where
    assignExpr = Ex.buildExpressionParser table $ choice $ fromDataExpressionToExpr possibleAssignParser ++ [try call, try variable]

parseAssign nonVar = trace "Can't assign value to non variable types " $ error $ show nonVar

parseVarWithAssign:: HasCallStack => CustomParsec Expr
parseVarWithAssign = do
  a <- parseVar
  parseAssign a

parseExistingVarWithAssign:: HasCallStack => CustomParsec Expr
parseExistingVarWithAssign = do 
  a <- parseVarWithExistingType >>= return . trace "parsing var with existing type"
  parseAssign a >>= return . trace "parsing assign in parse existing var with assign"

variable:: HasCallStack => CustomParsec Expr
variable = do
  spaces
  ret <- try (
    try parseVarWithExistingType  >< "parse var with existing type"
    <|> try parseExistingVarWithAssign >< "parse existing var with assign"
    <|> try parseVarWithAssign >< "parse var with assign"
    <|> parseVar >< "parse var")

  try $ reservedOp ";"
  return ret

expr :: CustomParsec Expr
expr = Ex.buildExpressionParser (table ++ [return $ binary "=" (fromStringToOperator "=") Ex.AssocLeft]) factor

function:: CustomParsec Expr
function = do
  name <- identifier
  args <- choice [parens $ (parseVar `sepBy` between spaces spaces (string ",")), parens $ (return Void `sepBy` spaces)] 
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
  args <- parens $ many parseVar
  return $ Extern name args

call :: CustomParsec Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  reservedOp ";"
  return $ Call name args

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
