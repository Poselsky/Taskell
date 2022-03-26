module LLVMIR.CodegenNew where

import LLVMIR.ExprTypesToLLVMTypes

import qualified Parsing.Syntax as S
import qualified LLVMIR.Codegen as C 
import qualified LLVMIR.Emit as Emit
import LLVM.AST (BasicBlock)

codeGen:: S.Expr -> [C.Codegen BasicBlock] 
codeGen func@(S.Function rtty name args bdy) = do
    error ""

codeGen (S.ExprList expr) = codeGen $ head expr
codeGen expr = error ""