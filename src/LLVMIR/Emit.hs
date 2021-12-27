module LLVMIR.Emit where

import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import LLVMIR.Codegen
import qualified Parsing.Syntax as S
import Data.String (fromString)
import Parsing.Syntax (Op(Assign), getVarName)
import Data.List.Split (splitOn)
import Debug.Trace (trace)

toSig :: [S.Expr] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name $ fromString $ getVarName x))

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function t name args body) = do
  define double name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM_ args $ \a -> do
        var <- alloca double
        store var (local (AST.Name $ fromString $ getVarName a))
        assign (getVarName a) var
      cgen body >>= ret

codegenTop (S.Extern name args) = do
  external double name fnargs
  where fnargs = toSig args

codegenTop exp = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

binops = Map.fromList [
      ("+", fadd)
    , ("-", fsub)
    , ("*", fmul)
    , ("/", fdiv)
    , ("<", lt)
  ]

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.UnaryOp op a) = do
  cgen $ S.Call ("unary" ++ show op) [a]
cgen (S.BinaryOp Assign (S.Var t var) val) = do
  a <- getvar var
  cval <- cgen val
  store a cval
  return cval
cgen (S.BinaryOp op a b) = do
  case Map.lookup (show op) binops of
    Just f  -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"
cgen (S.Var t var) = getvar var >>= load
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
-- TODO: This should be generated from integer and not double
cgen (S.Int n) = return $ cons $ C.Float (F.Double $ fromInteger n)
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name $ fromString fn)) largs
cgen S.Function{} = error "Function not implemented"
cgen S.Extern{} = error "Extern not implemented"
cgen expr = error $ "Rest in codegenerator is undefined " ++ show expr

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    mapM_ print $ splitOn "\\n" $ show llstr
    return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM mod modn