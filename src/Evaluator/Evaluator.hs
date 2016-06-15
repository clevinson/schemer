module Evaluator.Evaluator where

import Control.Monad.Except (throwError)

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
eval (LispList (LispAtom "cond" : condExprs)) = cond condExprs
--eval (LispList (LispAtom "case" : caseExprs)) = lispCase condExprs
eval (LispList (LispAtom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


cond :: [LispVal] -> ThrowsError LispVal
cond [LispList (LispAtom "else" : elseExpr)] = lastEval elseExpr
cond (LispList (pred : conseq) : xs) = 
    do result <- eval pred
       case result of
        LispBool False -> cond xs
        LispBool True -> lastEval conseq
        otherwise -> throwError $ TypeMismatch "Bool" pred
cond (x : xs) = throwError $ BadSpecialForm "Expected list with two elements" x

--lispCase :: [LispVal] -> ThrowsError LispVal
--lispCase (key : datums) = lispCaseH key datums
--
--lispCaseH :: LispVal -> [LispVal] -> ThrowsError LispVal
--lispCaseH expr [LispList (LispAtom "else" : elseExpr)] = lastEval elseExpr
--lispCaseH expr (clause : xs) =
--    do result <- (eqv (eval expr) (


lastEval :: [LispVal] -> ThrowsError LispVal
lastEval [] = throwError $ Default "Code error. list cannot be empty yo!"
lastEval [x] = eval x
lastEval (x:xs) = eval x >> lastEval xs

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognied primitive function args" func)
                        ($ args)
                        (lookup func primitives)
