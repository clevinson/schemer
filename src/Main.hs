module Main where

import System.Environment

import Schemer.Runner (runOne, runRepl)

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne args
