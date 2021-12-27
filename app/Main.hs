module Main where

import Parser
import Syntax

import Control.Monad.Trans
import System.Console.Haskeline
import Text.Parsec

process :: String -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> do print err
    Right ex -> do 
        print $ length ex
        mapM_ print ex

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop