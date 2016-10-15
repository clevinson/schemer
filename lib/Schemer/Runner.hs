module Schemer.Runner where

import System.Console.Haskeline
import System.IO
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import Schemer.Parser (readExpr)
import Schemer.Evaluator (eval)
import Schemer.Variables
import Schemer.Types
import Schemer.Primitives (primitives)
import Schemer.IOPrimitives (ioPrimitives)

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc PrimitiveFunc) primitives
                                               ++ map (makeFunc IOFunc) ioPrimitives)
  where makeFunc constructor (var, func) = (var, constructor func)

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", LispList $ map LispString $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (LispList [LispAtom "load", LispString (args !! 0)]))
        >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= (runInputT defaultSettings) . loop
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
