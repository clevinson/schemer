module Schemer.Parser where

import Control.Monad
import Control.Monad.Except (throwError)

import Schemer.Types
import Text.ParserCombinators.Parsec hiding ( spaces )

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
                          _ -> LispAtom atom

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

