module LLVMIR.ExprTypesToLLVMTypes where 

import LLVM.AST.Type as T
import qualified Parsing.Syntax as C

exprTypeToLLVMType:: C.Expr -> T.Type
exprTypeToLLVMType (C.Int _)  = T.IntegerType 32 
exprTypeToLLVMType (C.I64 _)  = T.IntegerType 64 
exprTypeToLLVMType (C.I32 _)  = T.IntegerType 32 
exprTypeToLLVMType (C.I16 _)  = T.IntegerType 16     
--TODO: Find a better way how to do this
exprTypeToLLVMType (C.UI64 _) = T.IntegerType 64 
exprTypeToLLVMType (C.UI32 _) = T.IntegerType 32 
exprTypeToLLVMType (C.UI16 _) = T.IntegerType 16 
exprTypeToLLVMType (C.String _) = T.ArrayType maxBound $ T.IntegerType 8
exprTypeToLLVMType (C.Float _)  = T.FloatingPointType T.FloatFP
exprTypeToLLVMType _ = error "not implemented"
  