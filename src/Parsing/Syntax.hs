{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parsing.Syntax where

import qualified Data.Text as T
import Data.Word
import Debug.Trace (trace)
import Control.Monad.State
import qualified Data.Int as I
import qualified Data.Map as Map

type Name = String 
type Arg = Name
type IInt = I.Int64

-- TODO: Make pretty printer
instance Show Expr where
    show var@(Var t name) = "Var " ++ show t ++ " " ++ name  
    show (Call name args) = "Call " ++ show name ++ " " ++ show args 
    show (Function t n args body) = "Function " ++ show t ++ " " ++ n ++ " " ++ show args ++ " { " ++ show body ++ " } "
    show (Extern n na) = "Extern " ++ n ++ " " ++ show na 
    show (BinaryOp op expr1 expr2) = "( BinaryOp " ++ show op ++ " " ++ show expr1 ++ " " ++ show expr2 ++ " )"
    show (UnaryOp op expr) = "( UnaryOp " ++ show op ++ " " ++ show expr ++ " )"
    show (Float a) = "Float " ++ show a
    show (Int a) = "Int " ++ show a
    show (I64 a) = "I64 " ++ show a 
    show (I32 a) = "I32" ++ show a 
    show (I16 a) = "I16 " ++ show a
    show (UI64 a) = "UI64 " ++ show a
    show (UI32 a) = "UI32 " ++ show a
    show (UI16 a) = "UI16 " ++ show a
    show (String a) = "String " ++ show a 
    show Void = "Void "

data Expr
    = Var Expr Name 
    | Call Name [Expr] 
    | Extern Name [Expr] 
    | Function String Name [Expr] Expr 
    | BinaryOp Op Expr Expr
    | UnaryOp Op Expr 
    | Float (Maybe Double)
    | Int (Maybe Integer)
    | I64 (Maybe I.Int64) 
    | I32 (Maybe I.Int32) 
    | I16 (Maybe I.Int16)
    | UI64 (Maybe Word64) 
    | UI32 (Maybe Word32)
    | UI16 (Maybe Word16)
    | String (Maybe String)
    | Void
    deriving (Ord, Eq)


fromStringToDataType:: String -> Expr 
fromStringToDataType "int"    = trace "returnin int" $ Int Nothing
fromStringToDataType "i64"    = I64 Nothing
fromStringToDataType "i32"    = I32 Nothing
fromStringToDataType "i16"    = I16 Nothing
fromStringToDataType "ui64"   = UI64 Nothing
fromStringToDataType "ui32"   = UI32 Nothing
fromStringToDataType "ui16"   = UI16 Nothing
fromStringToDataType "string" = String Nothing
fromStringToDataType input    = error $ "Can't convert to data type: " ++ input

possibleDataTypesInString:: [String]
possibleDataTypesInString = ["int", "i64", "i32", "i16", "ui64", "ui32", "ui16", "string"]

data Op
    = Addition 
    | Subtraction 
    | Multiplication 
    | Division
    | GreaterThan
    | LessThan
    | Assign 
    deriving (Eq, Ord)


fromStringToOperator:: String -> Op
fromStringToOperator "+" = Addition 
fromStringToOperator "-" = Subtraction
fromStringToOperator "*" = Multiplication 
fromStringToOperator "/" = Division 
fromStringToOperator "=" = Assign
fromStringToOperator ">" = GreaterThan 
fromStringToOperator "<" = LessThan 
fromStringToOperator _   = error "Unknown operator"


instance Show Op where
    show Addition = "+" 
    show Subtraction = "-"
    show Multiplication = "*" 
    show Division = "/" 
    show GreaterThan = ">" 
    show LessThan = "<" 
    show Assign = "=" 

getVarName:: Expr -> String 
getVarName (Var _ name) = name
getVarName _ = error "Can't get name from non var types"


data ExprState 
    = ExprState {
        currentExpr :: Expr
    ,   blockTypes  :: Map.Map String String
    } deriving Show

newtype ExprS a = ExprS { runExprS :: State ExprState a }
  deriving (Functor, Applicative, Monad, MonadState ExprState)

emptyExprState:: ExprState
emptyExprState = ExprState {
    currentExpr = Void,
    blockTypes  = Map.empty
}