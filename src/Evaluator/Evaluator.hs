module Evaluator.Evaluator where

import Control.Monad.Error (throwError)

import Types.LispVal
import Error.LispError
import Evaluator.Primitives


eval :: LispVal -> ThrowsError LispVal
eval val@(LispString _) = return val
eval val@(LispNumber _) = return val
eval val@(LispBool _) = return val
eval val@(LispAtom _) = return val
eval (LispList [LispAtom "quote", val]) = return val
eval (LispList [LispAtom "if", pred, conseq, alt]) =
    do result <- eval pred
       case result of
         LispBool False -> eval alt
         LispBool True -> eval conseq
         otherwise -> throwError $ TypeMismatch "Bool" pred
eval (LispList (LispAtom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognied primitive function args" func)
                        ($ args)
                        (lookup func primitives)
