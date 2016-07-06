module Main where

import System.Environment
import Schemer.Repl

main :: IO ()
main = do args <- getArgs
          case length args of
              0 -> runRepl
              1 -> putStrLn (evalString $ args !! 0)
              _ -> putStrLn "Schemer takes only 0 or 1 argument"


