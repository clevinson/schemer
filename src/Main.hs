module Main where

import Control.Monad
import Control.Monad.Error
import System.Environment
import Text.ParserCombinators.Parsec hiding ( spaces )
import Error.LispError
import Types.LispVal

main :: IO ()
main = do args <- getArgs
          evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
          putStrLn $ extractValue $ trapError evaled

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val


escapedChar :: Parser Char
escapedChar = do char '\\'
                 (char '"'
                  <|> char '\\'
                  <|> (char 'r' >> return '\r')
                  <|> (char 'n' >> return '\n')
                  <|> (char 't' >> return '\t'))

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (escapedChar <|> noneOf "\"")
                 char '"'
                 return $ LispString x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                          "#t" -> LispBool True
                          "#f" -> LispBool False
                          otherwise -> LispAtom atom

parseNumber :: Parser LispVal
parseNumber = liftM (LispNumber . read) $ many1 digit
-- parseNumber = many1 digit >>= return . LispNumber . read
-- parseNumber = do num <- many1 digit
--                  return $ (LispNumber . read) num

parseList :: Parser LispVal
parseList = liftM LispList $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do 
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ LispDottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ LispList [LispAtom "quote", x]


parseExpr :: Parser LispVal
parseExpr = parseAtom
          <|> parseString
          <|> parseNumber
          <|> parseQuoted
          <|> do char '('
                 x <- (try parseList) <|> parseDottedList
                 char ')'
                 return x


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

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", isKindOf "symbol"),
              ("string?", isKindOf "string"),
              ("number?", isKindOf "number"),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              ("<=", numBoolBinop (<=)),
              (">=", numBoolBinop (>=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string>?", strBoolBinop (>)),
              ("string<?", strBoolBinop (<)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . LispNumber . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (LispNumber n) = return n
unpackNum (LispString n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ LispString n
                             else return $ fst $ parsed !! 0
unpackNum (LispList [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

isKindOf :: String -> [LispVal] -> ThrowsError LispVal 
isKindOf "symbol" [LispAtom _] = return $ LispBool True
isKindOf "symbol" _ = return $ LispBool False
isKindOf "string" [LispString _] = return $ LispBool True
isKindOf "string" _ = return $ LispBool False
isKindOf "number" [LispNumber _] = return $ LispBool True
isKindOf "number" _ = return $ LispBool False

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ LispBool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (LispString s) = return s
unpackStr (LispNumber s) = return $ show s
unpackStr (LispBool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (LispBool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car [LispList (x:xs)] = return x
car [LispDottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [LispList (x:xs)] = return $ LispList xs
cdr [LispDottedList [xs] x] = return x
cdr [LispDottedList (_:xs) x] = return $ LispDottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, LispList []] = return $ LispList [x1]
cons [x, LispList xs] = return $ LispList $ [x] ++ xs
cons [x, LispDottedList xs xlast] = return $ LispDottedList ([x] ++ xs) xlast
cons [x1, x2] = return $ LispDottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(LispBool arg1), (LispBool arg2)] = return $ LispBool $ arg1 == arg2
eqv [(LispNumber arg1), (LispNumber arg2)] = return $ LispBool $ arg1 == arg2
eqv [(LispString arg1), (LispString arg2)] = return $ LispBool $ arg1 == arg2
eqv [(LispAtom arg1), (LispAtom arg2)] = return $ LispBool $ arg1 == arg2
eqv [(LispDottedList xs x), (LispDottedList ys y)] = eqv [LispList $ xs ++ [x], LispList $ ys ++ [y]]
eqv [(LispList arg1), (LispList arg2)] = return $ LispBool $
  (length arg1 == length arg2) &&
  (and $ map eqvPair $ zip arg1 arg2)
  where eqvPair (x1, x2) = case eqv [x1, x2] of
                             Left err -> False
                             Right (LispBool val) -> val
eqv [_,_] = return $ LispBool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
-- liftM (liftM (==) (unpacker arg1)) (unpacker arg2)
  `catchError` (const $ return False)

unLispValBool :: LispVal -> Bool
unLispValBool (LispBool x) = x

tupToArr :: (a,a) -> [a]
tupToArr (a,b) = [a,b]

equal :: [LispVal] -> ThrowsError LispVal
equal [LispList l1, LispList l2] = do
  lispBools <- mapM equal $ map tupToArr (zip l1 l2)
  return $ LispBool $ and $ map (\(LispBool b) -> b) lispBools

equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
    [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ LispBool $ (primitiveEquals || let (LispBool x) = eqvEquals in x)

equal badArgList = throwError $ NumArgs 2 badArgList

