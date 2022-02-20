module Parsing.IndividualExpressions.VariableParser where
import Parsing.Lexer
import Parsing.Syntax
import qualified Data.Map as Map
import Text.Parsec
import Control.Monad
import Data.Maybe
import Text.Parsec.Expr
import Parsing.OperatorTable
import Parsing.IndividualExpressions.CallParser
    ( callParametrized )
import Parsing.ParserStateHelpers


--TODO: Completely refactor parseVar
{-
  1.Var declaration
  2.Var assignment
    a. assign previously existing vars
    b. function calls
    c. functions (do it with pointers? Need to research this)
  3.Var type parsing
  4.Type checking
  5.After assign operator -
    a. parse correctly binary operators so it will chain
    b. last statement should end with ';'
-}

-- int num
varDeclaration:: CustomParsec Expr
varDeclaration = do
    --look for varType, save it and then reserve types
    varType <- lookAhead $ choice parsecPossibleVarTypes 
    choice $ reserved <$> possibleDataTypesInString
    spaces
    varName <- identifier  

    --Can't name vars by their datatypes
    guard $ varName `notElem` possibleDataTypesInString 
    --Can't redefine variable type if it already exists in state
    --Look up must result into nothing
    s <- getState
    guard $ isNothing $ getVariableTypeByName varName s 

    --Insert variable into cache
    let returningExpr = Var (fromStringToDataType varType) varName 
    updateParserState (appendVariable returningExpr)

    return returningExpr
    where
        parsecPossibleVarTypes = string <$> possibleDataTypesInString

-- Example
-- int num = 3;
-- num = 5;
getExistingVar:: CustomParsec Expr
getExistingVar = do
  varName <- identifier  

  --Can't name vars by their datatypes
  guard $ varName `notElem` possibleDataTypesInString 
  --Can't redefine variable type if it already exists in state
  --Look up must result into nothing
  s <- getState
  let maybeVarType = getVariableTypeByName varName s 

  case maybeVarType of 
    Nothing -> fail ("VarName: " ++ varName ++ " wasn't previously declared.")
    Just str -> return $ Var (fromStringToDataType str) varName

emptyVarDeclaration:: CustomParsec Expr
emptyVarDeclaration = do
  declaredVar <- varDeclaration 
  reservedOp ";"
  return declaredVar 

variableAfterAssignment:: CustomParsec Expr
variableAfterAssignment = do
  let possibleAssignments = [getExistingVar, call 
    --This is pretty neat, it will convert to correct var automatically
       , toDataTypeExpr <$> stringLiteral
       , toDataTypeExpr <$> integer
       , toDataTypeExpr <$> float]
  buildExpressionParser table $ choice possibleAssignments 

varWithAssignment:: CustomParsec Expr
varWithAssignment = do
  -- Get variable
  varLeft <- try (try varDeclaration 
    <|> try getExistingVar)
  -- Equal sign
  between spaces spaces $ reservedOp "=" 

  -- Everything else after assignment
  -- Name tree because ((Var int a + (Var int b + Var int c)) - Var int d)
  treeRight <- variableAfterAssignment
  reservedOp ";"
  return $ BinaryOp Assign varLeft treeRight 

call:: CustomParsec Expr
call = callParametrized getExistingVar