module Parsing.DataTypeParsingHelper where
import Parsing.Lexer (CustomParsec, integer, float, stringLiteral)
import Parsing.Syntax (ToDataTypeExpression, createToDataExpression)
import Debug.Trace
import Text.Parsec (try)


possibleValueParser :: [CustomParsec ToDataTypeExpression]
possibleValueParser =
    [ 
        converter $ try float,
        converter $ try integer,
        converter $ try stringLiteral
    ]
    where 
        -- Helper funtion to convert to ToDataTypeExpression
        converter a = a >>= \x -> return $ createToDataExpression x