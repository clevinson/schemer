module Schemer.Repl where

import System.Console.Haskeline
import Control.Monad (liftM)

import Schemer.Types (extractValue, trapError)
import Schemer.Parser (readExpr)
import Schemer.Evaluator (eval)

evalString :: String -> String
evalString expr = extractValue $ trapError (liftM show $ readExpr expr >>= eval)

runRepl :: IO ()
runRepl = runInputT defaultSettings loop
    where
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine "🍂  "
            case minput of
                 Nothing -> return ()
                 Just "quit" -> return ()
                 Just "" -> loop
                 Just input -> do outputStrLn $ evalString input
                                  loop
