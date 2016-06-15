module Schemer.Evaluator where

import Control.Monad.Except (throwError)
import Control.Monad ((>=>))

import Schemer.Types
import Schemer.Primitives


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
         _ -> throwError $ TypeMismatch "Bool" pred
eval (LispList (LispAtom "cond" : condExprs)) = cond condExprs
eval (LispList (LispAtom "case" : keyExpr : clauses)) = lispCase keyExpr clauses
eval (LispList (LispAtom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


cond :: [LispVal] -> ThrowsError LispVal
cond [LispList (LispAtom "else" : elseExpr)] = lastEval elseExpr
cond (LispList (pred : conseq) : xs) = 
    eval pred >>= \result ->
       case result of
        LispBool False -> cond xs
        LispBool True -> lastEval conseq
        _ -> throwError $ TypeMismatch "Bool" result
cond _ = throwError $ Default "Bad implementation of `cond`"

lispCase :: LispVal -> [LispVal] -> ThrowsError LispVal
lispCase _ [LispList (LispAtom "else" : conseq)] = lastEval conseq
lispCase keyExpr (LispList clause : clauses) =
       eval keyExpr >>= \evaledKey ->
           mapM (eval >=> eq evaledKey) datumList >>= \lispBools ->
             if or (map unLispBool lispBools)
             then lastEval conseq
             else lispCase keyExpr clauses
  where
    unLispBool (LispBool b) = b
    unLispBool _ = False
    eq x y = eqv [x, y]
    LispList datumList : conseq = clause
lispCase _ _ = throwError $ Default "Bad form for `case`"


lastEval :: [LispVal] -> ThrowsError LispVal
lastEval [] = throwError $ Default "Code error. list cannot be empty yo!"
lastEval [x] = eval x
lastEval (x:xs) = eval x >> lastEval xs

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognied primitive function args" func)
                        ($ args)
                        (lookup func primitives)
