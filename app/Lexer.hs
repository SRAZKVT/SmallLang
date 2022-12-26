module Lexer where

import Data.Char

type Line = Int
type Position = Int
type Lexeme = String

data Token = PAREN_LEFT
           | PAREN_RIGHT
           | BRACE_LEFT
           | BRACE_RIGHT
           | EQUAL
           | DOUBLE_EQUAL
           | PLUS
           | MINUS
           | STAR
           | SLASH
           | BANG
           | STRING String
           | INTEGER Integer
           | IDENTIFIER Lexeme
           | VAR
           | VAL
           | IF
           | ELIF
           | ELSE
           | SEMICOLON
           | EOF

instance Show Token where
    show PAREN_LEFT     = "TOK_PAREN_LEFT"
    show PAREN_RIGHT    = "TOK_PAREN_RIGHT"
    show BRACE_LEFT     = "TOK_BRACE_LEFT"
    show BRACE_RIGHT    = "TOK_BRACE_RIGHT"
    show EQUAL          = "TOK_EQUAL"
    show DOUBLE_EQUAL   = "TOK_DOUBLE_EQUAL"
    show PLUS           = "TOK_PLUS"
    show MINUS          = "TOK_MINUS"
    show STAR           = "TOK_STAR"
    show SLASH          = "TOK_SLASH"
    show BANG           = "TOK_BANG"
    show (STRING s)     = "TOK_STR: '" ++ s ++ "'"
    show (INTEGER i)    = "TOK_INT: '" ++ show i ++ "'"
    show (IDENTIFIER l) = "TOK_IDENTIFIER: '" ++ l ++ "'"
    show VAR            = "TOK_VAR"
    show VAL            = "TOK_VAL"
    show IF             = "TOK_IF"
    show ELIF           = "TOK_ELIF"
    show ELSE           = "TOK_ELSE"
    show SEMICOLON      = "TOK_SEMICOLON"
    show EOF            = "EOF"

instance Eq Token where
    (==) PAREN_LEFT PAREN_LEFT         = True
    (==) PAREN_RIGHT PAREN_RIGHT       = True
    (==) BRACE_LEFT BRACE_LEFT         = True
    (==) BRACE_RIGHT BRACE_RIGHT       = True
    (==) EQUAL EQUAL                   = True
    (==) DOUBLE_EQUAL DOUBLE_EQUAL     = True
    (==) PLUS PLUS                     = True
    (==) MINUS MINUS                   = True
    (==) STAR STAR                     = True
    (==) SLASH SLASH                   = True
    (==) BANG BANG                     = True
    (==) (STRING a) (STRING b)         = a == b
    (==) (INTEGER a) (INTEGER b)       = a == b
    (==) (IDENTIFIER a) (IDENTIFIER b) = a == b
    (==) VAR VAR                       = True
    (==) VAL VAL                       = True
    (==) IF IF                         = True
    (==) ELIF ELIF                     = True
    (==) ELSE ELSE                     = True
    (==) SEMICOLON SEMICOLON           = True
    (==) EOF EOF                       = True
    (==) _ _                           = False

lex :: String -> ([Token], [String])
lex [] = ([], [])
lex f  = let (tks, err) = lex' f 1 ([], [])
         in (reverse tks, reverse err)
    where lex' :: String -> Line -> ([Token], [String]) -> ([Token], [String])
          lex' []        _ (tks, err)  =           (EOF:tks, err)
          lex' (';':fs)  l (tks, err)  = lex' fs l (SEMICOLON:tks, err)
          lex' ('(':fs)  l (tks, err)  = lex' fs l (PAREN_LEFT:tks, err)
          lex' (')':fs)  l (tks, err)  = lex' fs l (PAREN_RIGHT:tks, err)
          lex' ('{':fs)  l (tks, err)  = lex' fs l (BRACE_LEFT:tks, err)
          lex' ('}':fs)  l (tks, err)  = lex' fs l (BRACE_RIGHT:tks, err)
          lex' ('+':fs)  l (tks, err)  = lex' fs l (PLUS:tks, err)
          lex' ('-':fs)  l (tks, err)  = lex' fs l (MINUS:tks, err)
          lex' ('*':fs)  l (tks, err)  = lex' fs l (STAR:tks, err)
          lex' ('!':fs)  l (tks, err)  = lex' fs l (BANG:tks, err)
          lex' ('/':fs)  l (tks, err)
              | head fs == '/'         = lex' (dropWhile (/='\n') fs) l (tks, err)
              | otherwise              = lex' fs l (SLASH:tks, err)
          lex' ('=':fs)  l (tks, err)
              | head fs == '='         = lex' (drop 1 fs) l (DOUBLE_EQUAL:tks, err)
              | otherwise              = lex' fs l (EQUAL:tks, err)
          lex' ('\n':fs) l (tks, err)  = lex' fs (l + 1) (tks, err)
          lex' ('"':fs)  l (tks, err)  = case stringSize fs of
                  Left size -> lex' (drop (size + 1) fs) l (STRING (take size fs):tks, err)
                  Right erro  -> lex' (dropWhile (/= '\n') fs) l (tks, err ++ [erro])
          lex' (x:fs) l (tks, err)
              | isDigit x = let s = length $ takeWhile isDigit fs
                            in lex' (drop s fs) l (INTEGER (read (x:take s fs)):tks, err)
              | isAlphaNumeric x = let i = x:take (identifierSize fs) fs
                                   in lex' (drop (identifierSize fs) fs) l
                                      (identifierToken i:tks,err)
              | isSpace x = lex' fs l (tks, err)
              | otherwise = lex' fs l (tks, unwords ["Unknown character:",
                                                     [x],
                                                     ":",
                                                     show (ord x)]:err)

identifierToken :: Lexeme -> Token
identifierToken "var"  = VAR
identifierToken "val"  = VAL
identifierToken "if"   = IF
identifierToken "elif" = ELIF
identifierToken "else" = ELSE
identifierToken l      = IDENTIFIER l

stringSize :: String -> Either Int String
stringSize []          = Right "String reaches end of file"
stringSize ('\n':_)    = Right "String reaches end of line"
stringSize ('"':_)     = Left 0
stringSize ('\\':_:xs) = case stringSize xs of
                              Left x    -> Left $ x + 2
                              Right err -> Right err
stringSize ('\\':[])   = Right "Nothing to escape"
stringSize (_:xs)      = case stringSize xs of
                        Left x    -> Left $ x + 1
                        Right err -> Right err

isAlphaNumeric :: Char -> Bool
isAlphaNumeric '_'        = True
isAlphaNumeric c
    | c `elem` ['a'..'z'] = True
    | c `elem` ['A'..'Z'] = True
    | otherwise           = False

identifierSize :: String -> Int
identifierSize []      = 0
identifierSize (c:cs)
    | isAlphaNumeric c = 1 + identifierSize cs
    | otherwise        = 0
