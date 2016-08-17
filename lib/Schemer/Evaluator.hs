module Schemer.Evaluator where

import Control.Monad.Except (throwError)
import Control.Monad()
--import Control.Monad ((>=>))


import Schemer.Types
import Schemer.Primitives
import Schemer.Variables

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(LispString _) = return val
eval _ val@(LispNumber _) = return val
eval _ val@(LispFloat _) = return val
eval _ val@(LispBool _) = return val
eval env (LispAtom id) = getVar env id
eval _ (LispList [LispAtom "quote", val]) = return val
eval env (LispList [LispAtom "if", pred, conseq, alt]) =
    do result <- eval env pred
       case result of
         LispBool False -> eval env alt
         LispBool True -> eval env conseq
         _ -> throwError $ TypeMismatch "Bool" pred
--eval env (LispList (LispAtom "cond" : condExprs)) = cond condExprs
--eval env (LispList (LispAtom "case" : keyExpr : clauses)) = lispCase keyExpr clauses
eval env (LispList [LispAtom "set!", LispAtom var, form]) =
    eval env form >>= setVar env var
eval env (LispList [LispAtom "define!", LispAtom var, form]) =
    eval env form >>= defineVar env var
eval env (LispList (LispAtom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


--cond :: Env -> [LispVal] -> IOThrowsError LispVal
--cond env [LispList (LispAtom "else" : elseExpr)] = lastEval elseExpr
--cond env (LispList (pred : conseq) : xs) = 
--    eval env pred >>= \result ->
--       case result of
--        LispBool False -> cond xs
--        LispBool True -> lastEval conseq
--        _ -> throwError $ TypeMismatch "Bool" result
--cond env _ = throwError $ Default "Bad implementation of `cond`"
--
--lispCase :: Env -> LispVal -> [LispVal] -> ThrowsError LispVal
--lispCase _ [LispList (LispAtom "else" : conseq)] = lastEval conseq
--lispCase env keyExpr (LispList clause : clauses) =
--       eval keyExpr >>= \evaledKey ->
--           mapM (eval >=> eq evaledKey) datumList >>= \lispBools ->
--             if or (map unLispBool lispBools)
--             then lastEval conseq
--             else lispCase keyExpr clauses
--  where
--    unLispBool (LispBool b) = b
--    unLispBool _ = False
--    eq x y = eqv [x, y]
--    LispList datumList : conseq = clause
--lispCase _ _ _ = throwError $ Default "Bad form for `case`"


lastEval :: Env -> [LispVal] -> IOThrowsError LispVal
lastEval _ [] = throwError $ Default "Code error. list cannot be empty yo!"
lastEval env [x] = eval env x
lastEval env (x:xs) = eval env x >> lastEval env xs

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognied primitive function args" func)
                        ($ args)
                        (lookup func primitives)
