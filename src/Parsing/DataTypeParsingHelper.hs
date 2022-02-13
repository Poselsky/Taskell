module Parsing.DataTypeParsingHelper where
import Parsing.Lexer (CustomParsec, integer, float, stringLiteral)
import Parsing.Syntax (ToDataTypeExpression, createToDataExpression, fromDataExpression, Expr)
import Debug.Trace
import Text.Parsec (try)


{-
Parser parsing all possible data type values after assigning
-}
possibleAssignParser :: [CustomParsec ToDataTypeExpression]
possibleAssignParser =
    [ 
        converter $ try float,
        converter $ try integer,
        converter $ try stringLiteral
    ]
    where 
        -- Helper funtion to convert to ToDataTypeExpression
        converter a = a >>= \x -> return $ createToDataExpression x


fromDataExpressionToExpr :: [CustomParsec ToDataTypeExpression] -> [CustomParsec Expr]
--TODO: Cleanup with Compose
fromDataExpressionToExpr = (fmap.fmap) fromDataExpression 