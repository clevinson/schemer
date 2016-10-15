{-# LANGUAGE ExistentialQuantification #-}
module Schemer.IOPrimitives where

import Control.Monad.Except (throwError)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import System.IO

import Schemer.Types
import Schemer.Parser (readExpr)
import Schemer.Evaluator (apply, load)

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-file", closePort),
                ("close-output-file", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, LispList args] = apply func args
applyProc (func : args) = apply func args
applyProc badArgs = liftThrows $ throwError (NumArgs 1 badArgs)

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [LispString filename] = liftM Port $ liftIO $ openFile filename mode
makePort _ [notString] = liftThrows $ throwError (TypeMismatch "string" notString)
makePort _ badArgs = liftThrows $ throwError (NumArgs 1 badArgs)

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ LispBool True)
closePort _ = return $ LispBool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr
readProc [notPort] = liftThrows $ throwError (TypeMismatch "port" notPort)
readProc badArgs = liftThrows $ throwError (NumArgs 1 badArgs)

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ LispBool True)
writeProc [_, notPort] = liftThrows $ throwError (TypeMismatch "port" notPort)
writeProc badArgs = liftThrows $ throwError (NumArgs 2 badArgs)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [LispString filename] = liftM LispString $ liftIO $ readFile filename
readContents [notString] = liftThrows $ throwError (TypeMismatch "string" notString)
readContents badArgs = liftThrows $ throwError (NumArgs 1 badArgs)

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [LispString filename] = liftM LispList $ load filename
readAll [notString] = liftThrows $ throwError (TypeMismatch "string" notString)
readAll badArgs = liftThrows $ throwError (NumArgs 1 badArgs)




























