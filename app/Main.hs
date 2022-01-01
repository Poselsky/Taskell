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
    Right ex -> do
      print res
      ast <- codegen modo ex
      return $ Just ast

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
  a <- process initModule "def add(int a,int b,int c) a + b + c;"
  print a

main :: IO ()
main = do
  runBasicCase
  -- args <- getArgs
  -- case args of
  --   []      -> repl
  --   [fname] -> processFile fname >> return ()