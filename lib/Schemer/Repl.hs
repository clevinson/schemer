module Schemer.Repl where

import System.Console.Haskeline
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import Schemer.Parser (readExpr)
import Schemer.Evaluator (eval)
import Schemer.Variables

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

runRepl :: IO ()
runRepl = nullEnv >>= (runInputT defaultSettings) . loop
    where
        loop :: Env -> InputT IO ()
        loop env = do
            minput <- getInputLine "🍂  "
            case minput of
                 Nothing -> return ()
                 Just "quit" -> return ()
                 Just "" -> loop env
                 Just input -> do outputStrLn =<< (liftIO $ evalString env input)
                                  loop env
