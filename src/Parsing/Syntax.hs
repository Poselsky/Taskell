module Parsing.Syntax where

import qualified Data.Text as T
import Data.Word
import qualified Data.Int as I

type Name = String 
type Arg = Name
type IInt = I.Int64

-- TODO: Make pretty printer
instance Show Expr where
    show var@(Var t name) = "Var " ++ t ++ " " ++ name  
    show (Call name args) = "Call " ++ show name ++ " " ++ show args 
    show (Function t n args body) = "Function " ++ show t ++ " " ++ n ++ " " ++ show args ++ " { " ++ show body ++ " } "
    show (Extern n na) = "Extern " ++ n ++ " " ++ show na 
    show (BinaryOp op expr1 expr2) = "( BinaryOp " ++ show op ++ " " ++ show expr1 ++ " " ++ show expr2 ++ " )"
    show (UnaryOp op expr) = "( UnaryOp " ++ show op ++ " " ++ show expr ++ " )"
    show rest = show rest

data Expr
    = Var String Name 
    | Call Name [Expr] 
    | Extern Name [Expr] 
    | Function String Name [Expr] Expr 
    | BinaryOp Op Expr Expr
    | UnaryOp Op Expr 
    | Float Double
    | Int Integer
    | I64 I.Int64 
    | I32 I.Int32 
    | I16 I.Int16
    | UI64 Word64 
    | UI32 Word32
    | UI16 Word16
    | String String
    | Void
    deriving (Ord, Eq)


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