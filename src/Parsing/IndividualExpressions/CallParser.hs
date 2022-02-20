module Parsing.IndividualExpressions.CallParser where
import Parsing.Lexer (CustomParsec, identifier, parens, commaSep, reservedOp)
import Parsing.Syntax

callParametrized :: CustomParsec Expr -> CustomParsec Expr
callParametrized varParser = do
  name <- identifier
  args <- parens $ commaSep varParser 
  reservedOp ";"
  return $ Call name args