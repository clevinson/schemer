module Schemer.String where

import Schemer.Types
import Control.Monad.Except

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [LispString str] = return $ LispNumber $ fromIntegral (length str)
stringLength [badArg] = throwError $ TypeMismatch "String" badArg
stringLength badList = throwError $ NumArgs 1 badList

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [LispString str, LispNumber n]
    | fromIntegral n < length str = return $ LispString [str !! (fromIntegral n)]
    | otherwise                   = throwError $ Default "Index too large, at string-ref"
stringRef [badArg1, badArg2] = throwError $ TypeMismatch "(String, Number)" (LispList [badArg1, badArg2])
stringRef badList = throwError $ NumArgs 2 badList


