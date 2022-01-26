module Parsing.ParserStateHelpers where

import Parsing.Syntax 
import Text.Parsec (State(State, stateUser)) 

appendParentTracerState:: String -> State String ExprState -> State String ExprState 
appendParentTracerState appendingString s@State{ stateUser = estate@ExprState{ parentTracer = a}} = 
    s { stateUser = estate { parentTracer = a ++ "( " ++ appendingString ++ " )"}} 
