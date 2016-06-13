module Types.LispVal where

data LispVal = LispAtom String
             | LispList [LispVal]
             | LispDottedList [LispVal] LispVal
             | LispNumber Integer
             | LispString String
             | LispBool Bool

showVal :: LispVal -> String
showVal (LispString contents) = "\"" ++ contents ++ "\""
showVal (LispAtom name) = name
showVal (LispNumber contents) = show contents
showVal (LispBool True) = "#t"
showVal (LispBool False) = "#f"
showVal (LispList contents) = "(" ++ unwordsList contents ++ ")"
showVal (LispDottedList head tail) = "(" ++ unwordsList head ++ " . " ++
                                 showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal
