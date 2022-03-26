{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
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
    show var@(Var t name) = "( Var " ++ name ++ " " ++ show t ++ " )"
    show (Call name args) = "Call " ++ show name ++ " " ++ show args 
    show (Function t n args body) = "Function " ++ dataTypeToString t ++ " " ++ n ++ " " ++ show args ++ " { " ++ show body ++ " } "
    show (Extern n na) = "Extern " ++ n ++ " " ++ show na 
    show (If boolExpr ifTrue ifFalse) = "If( " ++ show boolExpr ++  "){ " ++ show ifTrue ++ " } else " ++ "{ "++ show ifFalse++" }"
    show (BinaryOp op expr1 expr2) = "( BinaryOp " ++ show op ++ " " ++ show expr1 ++ " " ++ show expr2 ++ " )"
    show (UnaryOp op expr) = "( UnaryOp " ++ show op ++ " " ++ show expr ++ " )"
    show (ExprList expr) = "[ " ++ foldr (\x acc -> show x ++ acc ) "" expr ++ " ]"
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

type FunctionBody = Expr
type ExprList = Expr

data Expr
    -- Var should refer to Data type and name
    = Var Expr Name 
    | Call Name ExprList 
    | Extern Name ExprList
    | Function Expr Name ExprList FunctionBody
    | BinaryOp Op Expr Expr
    | UnaryOp Op Expr 
    | If Expr Expr Expr
    | ExprList [Expr]
    -- Data types
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


-------------- Smart constructor
class ToDataTypeExpression_ a where
    toDataTypeExpr :: a -> Expr 

instance ToDataTypeExpression_ Double where
    toDataTypeExpr a = Float $ Just a

instance ToDataTypeExpression_ Integer where
    toDataTypeExpr a = Int $ Just a

instance (a ~ Char) => ToDataTypeExpression_ [a] where
    toDataTypeExpr a = String $ Just a

data ToDataTypeExpression = 
    forall a. ToDataTypeExpression_ a => ToDataTypeExpession a

createToDataExpression:: ToDataTypeExpression_ a => a -> ToDataTypeExpression 
createToDataExpression a = ToDataTypeExpession (a)

fromDataExpression:: ToDataTypeExpression -> Expr
fromDataExpression (ToDataTypeExpession a) = toDataTypeExpr a

fromStringToDataType:: String -> Expr 
fromStringToDataType "int"    = Int Nothing
fromStringToDataType "i64"    = I64 Nothing
fromStringToDataType "i32"    = I32 Nothing
fromStringToDataType "i16"    = I16 Nothing
fromStringToDataType "ui64"   = UI64 Nothing
fromStringToDataType "ui32"   = UI32 Nothing
fromStringToDataType "ui16"   = UI16 Nothing
fromStringToDataType "string" = String Nothing
fromStringToDataType "float"  = Float Nothing
fromStringToDataType input    = error $ "Can't convert to data type: " ++ input


dataTypeToString:: Expr -> String 
dataTypeToString (Int _)    = "int" 
dataTypeToString (I64 _)    = "i64"   
dataTypeToString (I32 _)    = "i32"   
dataTypeToString (I16 _)    = "i16"   
dataTypeToString (UI64 _)   = "ui64"  
dataTypeToString (UI32 _)   = "ui32"  
dataTypeToString (UI16 _)   = "ui16"  
dataTypeToString (String _) = "string"
dataTypeToString (Float _)  = "float"
dataTypeToString input      = error $ "Can't convert from data type to string : " ++ show input

possibleDataTypesInString:: [String]
possibleDataTypesInString = ["int", "float", "i64", "i32", "i16", "ui64", "ui32", "ui16", "string"]

data Op
    = Addition 
    | Subtraction 
    | Multiplication 
    | Division
    | GreaterThan
    | LessThan
    | Equality
    | GreaterOrEqualThan
    | LessOrEqualThan
    | NonEquality
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
fromStringToOperator "==" = Equality
fromStringToOperator ">=" = GreaterOrEqualThan 
fromStringToOperator "<=" = LessOrEqualThan 
fromStringToOperator "!=" = NonEquality 
fromStringToOperator _   = error "Unknown operator"


instance Show Op where
    show Addition = "+" 
    show Subtraction = "-"
    show Multiplication = "*" 
    show Division = "/" 
    show GreaterThan = ">" 
    show LessThan = "<" 
    show Assign = "="
    show Equality = "=="
    show NonEquality = "!="
    show GreaterOrEqualThan = ">="
    show LessOrEqualThan = "<="

getVarName:: Expr -> String 
getVarName (Var _ name) = name
getVarName _ = error "Can't get name from non var types"

getVarType:: Expr -> String 
getVarType(Var t _) = dataTypeToString t 
getVarType _ = error "Can't get name from non var types"

data ExprState 
    = ExprState {
        currentExpr  :: Expr
    ,   blockTypes   :: Map.Map String String 
    ,   parentExpr   :: Expr
    ,   parentTracer :: String
    } deriving Show

newtype ExprS a = ExprS { runExprS :: State ExprState a }
  deriving (Functor, Applicative, Monad, MonadState ExprState)

emptyExprState:: ExprState
emptyExprState = ExprState {
    currentExpr = Void,
    parentExpr  = Void,
    parentTracer = "",
    blockTypes  = Map.empty
}