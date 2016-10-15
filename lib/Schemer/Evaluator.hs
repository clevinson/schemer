module Schemer.Evaluator where

import Control.Monad.Except (throwError)
import Control.Monad (liftM)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)

import Schemer.Types
import Schemer.Parser (readExprList)
import Schemer.Variables
import Schemer.Primitives (eqv)

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
eval env (LispList (LispAtom "cond" : condExprs)) = cond env condExprs
eval env (LispList (LispAtom "case" : keyExpr : clauses)) = lispCase env keyExpr clauses
eval env (LispList [LispAtom "set!", LispAtom var, form]) =
    eval env form >>= setVar env var
eval env (LispList [LispAtom "define", LispAtom var, form]) =
    eval env form >>= defineVar env var
eval env (LispList (LispAtom "define" : LispList (LispAtom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (LispList (LispAtom "define" : LispDottedList (LispAtom var : params) varargs : body)) =
    makeVarArgs varargs env params body >>= defineVar env var
eval env (LispList (LispAtom "lambda" : LispList params : body)) =
    makeNormalFunc env params body
eval env (LispList (LispAtom "lambda" : LispDottedList params varargs : body)) =
    makeVarArgs varargs env params body
eval env (LispList (LispAtom "lambda" : varargs@(LispAtom _) : body)) =
    makeVarArgs varargs env [] body
eval env (LispList [LispAtom "load", LispString filename]) =
    load filename >>= liftM last . mapM (eval env)
eval env (LispList (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

cond :: Env -> [LispVal] -> IOThrowsError LispVal
cond env [LispList (LispAtom "else" : elseExpr)] = lastEval env elseExpr
cond env (LispList (pred : conseq) : xs) =
    eval env pred >>= \result ->
       case result of
        LispBool False -> cond env xs
        LispBool True -> lastEval env conseq
        _ -> throwError $ TypeMismatch "Bool" result
cond _ _ = throwError $ Default "Bad implementation of `cond`"

lispCase :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
lispCase env _ [LispList (LispAtom "else" : conseq)] = lastEval env conseq
lispCase env keyExpr (LispList clause : clauses) =
       eval env keyExpr >>= \evaledKey ->
           mapM ((eval env) >=> eq evaledKey) datumList >>= \lispBools ->
             if or (map unLispBool lispBools)
             then lastEval env conseq
             else lispCase env keyExpr clauses
  where
    unLispBool (LispBool b) = b
    unLispBool _ = False
    eq x y = liftThrows $ eqv [x, y]
    LispList datumList : conseq = clause
lispCase _ _ _ = throwError $ Default "Bad form for `case`"

lastEval :: Env -> [LispVal] -> IOThrowsError LispVal
lastEval _ [] = throwError $ Default "Code error. list cannot be empty yo!"
lastEval env [x] = eval env x
lastEval env (x:xs) = eval env x >> lastEval env xs

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (num args) args
    else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, LispList $ remainingArgs)]
          Nothing -> return env
apply (IOFunc func) args = func args
apply func _ = throwError $ NotFunction "Unrecognized function type" (show func)

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

-- Helpers for making Function types
makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . showVal
