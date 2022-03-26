{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
import LLVM.AST (Operand(ConstantOperand))
import LLVM.IRBuilder.Instruction (phi)
import GHC.Stack
import GHC.IO (unsafePerformIO)
import qualified LLVM.AST as Ast
import LLVMIR.ExprTypesToLLVMTypes (exprTypeToLLVMType)

one = cons $ C.Float (F.Double 1.0)
zero = cons $ C.Float (F.Double 0.0)
false = zero
true = one

toSig :: S.Expr -> [(AST.Type, AST.Name)]
toSig (S.ExprList s) = map (\x -> (exprTypeToLLVMType x, AST.Name $ fromString $ getVarName x)) s
toSig _ = error "Can't map non list signature"

--Todo : I might need to generate another functions here
codegenTop :: S.Expr -> LLVM ()
codegenTop f@(S.Function t name args body) = do
  define (exprTypeToLLVMType t) name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen f >>= mapM ret

codegenTop (S.Extern name args) = do
  external double name fnargs
  where fnargs = toSig args

codegenTop exp = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= mapM ret


-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

gt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
gt a b = lt b a

binops :: Map.Map [Char] (Operand -> Operand -> Codegen Operand)
binops = Map.fromList [
      ("+", fadd)
    , ("-", fsub)
    , ("*", fmul)
    , ("/", fdiv)
    , ("<", lt)
    , (">", gt)
  ]

-- cgenBody:: S.Expr -> Codegen [Ast.BasicBlock]
-- cgenBody (S.ExprList l) = do
--   concat <$> traverse cgenBody l

-- cgenBody (S.If cond bodyTrue@(S.ExprList _) bodyFalse@(S.ExprList _)) = do


--   ifthen <- addBlock "if.then"
--   ifelse <- addBlock "if.else"
--   ifexit <- addBlock "if.exit"

--   -- %entry
--   ------------------
--   cond <- cgen cond
--   test <- fcmp FP.ONE false cond
--   cbr test ifthen ifelse -- Branch based on the condition

--   -- if.then
--   ------------------
--   setBlock ifthen
--   -- Generate code for the true branch
--   trval <- cgenInstructions bodyTrue
--   br ifexit              -- Branch to the merge block
--   ifthen <- getBlock

--   -- if.else
--   ------------------
--   setBlock ifelse
--   -- Generate code for the false branch
--   flval <- cgenInstructions bodyFalse
--   br ifexit              -- Branch to the merge block
--   ifelse <- getBlock

--   -- if.exit
--   ------------------
--   -- TODO: Here's potential of chaining if else
--   -- TODO: Check if you should have voidtype here
--   return $ AST.value $ AST.Phi VoidType [(trval, ifthen), (flval, ifelse)] []


-- cgenBody e = error "not implemented"


--TODO:This is temporary wrapper for easier tracing
cgen :: S.Expr -> Codegen [AST.Operand]
cgen a =
  --trace (show $ unsafePerformIO $ appendFile "/mnt/c/Users/omusijenko/Desktop/Taskell/log.txt" $ show a ++ "\n") 
  -- trace ( show a ) cgen' a
  cgen' a

cgen' :: S.Expr -> Codegen [AST.Operand]
-- cgen' (S.UnaryOp op a) = do
--   cgen' $ S.Call ("unary" ++ show op) [a]
cgen' (S.BinaryOp Assign var@(S.Var t varName) val) = do
  cval <- cgen val
  assign varName $ head $ trace ("showing cval" ++ show cval) cval
  -- TODO!!! Need to return operand
  return cval 

cgen' (S.BinaryOp op a b) = do
  case Map.lookup (show op) binops of
    Just f  -> do
      ca <- cgen a
      cb <- cgen b
      zipWithM f ca cb
    Nothing -> error "No such operator"

cgen' (S.Var t var) = fmap (: []) $ getvar var >>= load
cgen' (S.Float (Just n)) = return $ fmap (: []) cons $ C.Float (F.Double n)
-- TODO: This should be generated from integer and not double, related to llvm type todo
cgen' (S.Int Nothing) = return $ fmap (: []) cons $ C.Float (F.Double 0)
cgen' (S.Int (Just n)) = return $ fmap (: []) cons $ C.Float (F.Double $ fromInteger n)

cgen' (S.Call fn (S.ExprList args)) = do
  largs <- mapM cgen args
  sequence $ (: []) <$> call (externf (AST.Name $ fromString fn)) $ concat largs
cgen' (S.If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  -- %entry
  ------------------
  cond <- cgen cond
  test <- fcmp FP.ONE false $ head cond
  cbr test ifthen ifelse -- Branch based on the condition

  -- if.then
  ------------------
  setBlock ifthen
  trval <- cgen tr       -- Generate code for the true branch
  br ifexit              -- Branch to the merge block
  ifthen <- getBlock

  -- if.else
  ------------------
  setBlock ifelse
  flval <- cgen fl       -- Generate code for the false branch
  br ifexit              -- Branch to the merge block
  ifelse <- getBlock


  let trVals = map ( ,ifthen) trval
  let flVals = map ( ,ifelse) flval

  -- if.exit
  ------------------
  -- TODO: Here's potential of chaining if else
  return $ [AST.value $ AST.Phi AST.VoidType (trVals ++ flVals) []]

cgen' (S.Function t name (S.ExprList args) (S.ExprList body)) = do
  entry <- addBlock entryBlockName
  setBlock entry
  forM_ args $ \a -> do
    var <- alloca double
    store var (local (AST.Name $ fromString $ getVarName a))
    assign (getVarName a) var
  --TODO: cgen full body
  something <- concat <$> sequence (cgen <$> body)
  let v = trace ("Showing" ++ show something) something
  return v

cgen' S.Void = return $ fmap (: []) ConstantOperand $ C.Null Ast.VoidType

-- cgen' (S.Extern _ _) = error "Extern not implemented"
cgen' (S.ExprList l) = concat <$> mapM cgen' l
cgen' expr = error $ "Rest in codegenerator is undefined " ++ show expr

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