module Parsing.OperatorTable where
import qualified Text.Parsec.Expr as Ex
import Parsing.Syntax
import Control.Monad.Identity
import Parsing.Lexer

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