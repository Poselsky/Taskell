module Parsing.ParsingHelpers where
import Debug.Trace (trace)

--Debugging helper
--TODO: This should get build args
(><):: (Monad m, Show a) => m a -> String -> m a 
(><) m str 
    | False = m >>= \x -> return $ trace (str ++ " " ++ show x) x 
    | otherwise = m