module Main where

import System.Environment
import Schemer.Repl
import Schemer.Primitives (primitiveBindings)

main :: IO ()
main = do args <- getArgs
          case length args of
              0 -> runRepl
              1 -> putStrLn =<< (primitiveBindings >>= ((flip evalString) $ args !! 0))
              _ -> putStrLn "Schemer takes only 0 or 1 argument"
