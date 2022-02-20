module Parsing.ParserStateHelpers where

import Parsing.Syntax 
import Text.Parsec (State(State, stateUser), optionMaybe) 
import qualified Data.Map as Map
import Parsing.Lexer (CustomParsec)

appendParentTracerState:: String -> State String ExprState -> State String ExprState 
appendParentTracerState appendingString s@State{ stateUser = estate@ExprState{ parentTracer = a}} = 
    s { stateUser = estate { parentTracer = a ++ "( " ++ appendingString ++ " )"}} 

appendVariable:: Expr -> State String ExprState -> State String ExprState 
appendVariable expression s@State{ stateUser = estate@ExprState{ blockTypes = ts }} = 
    s { stateUser = estate { blockTypes = Map.insert (getVarName expression) (getVarType expression) ts }} 

getVariableTypeByName:: String -> ExprState -> Maybe String 
getVariableTypeByName varName ExprState{ blockTypes = ts } = Map.lookup varName ts 