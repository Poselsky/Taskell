module Main where

import Parsing.Parser
import Parsing.Syntax
import LLVMIR.Emit
import LLVMIR.Codegen

import Control.Monad.Trans
import System.Console.Haskeline
import System.Environment
import System.IO
import Text.Parsec

import qualified LLVM.AST as AST

initModule :: AST.Module
initModule = emptyModule "my cool jit"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right (ExprList ex)-> do
      print res
      ast <- codegen modo ex
      return $ Just ast
    Right _ -> error "Not possible"

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
  loop mod = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        modn <- liftIO $ process mod input
        case modn of
          Just modn -> loop modn
          Nothing -> loop mod

runBasicCase:: IO ()
runBasicCase = do
  -- b <- process initModule "add(int a,int b,int c) : string { a > b };"
  a <- process initModule "int a = 0; a; = 0"
  -- a <- process initModule "int c; c = 2;"
  return ()

main :: IO ()
main = do
  runBasicCase
  -- args <- getArgs
  -- case args of
  --   []      -> repl
  --   [fname] -> processFile fname >> return ()