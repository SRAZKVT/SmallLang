module Lexer where

import Data.Char

type Line = Int
type Position = Int
type Lexeme = String

data TokenType = PAREN_LEFT
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

instance Show TokenType where
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

instance Eq TokenType where
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

data LexerState = LexerState String Line Position

data Token = Token TokenType Line Position Lexeme

tokenType :: Token -> TokenType
tokenType (Token tk _ _ _) = tk

newToken :: TokenType -> LexerState -> Lexeme -> Token
newToken tk (LexerState _ line pos) lexeme = (Token tk line pos lexeme)

getLexerContent :: LexerState -> String
getLexerContent (LexerState s _ _) = s

updateLexerState :: LexerState -> Int -> LexerState
updateLexerState (LexerState s l p) amt = LexerState (drop amt s) l (p + amt)

updateLexerLine :: LexerState -> LexerState
updateLexerLine (LexerState s l _) = LexerState (tail $ dropWhile (/= '\n') s) (l + 1) 1

lex :: String -> ([Token], [String])
lex [] = ([], [])
lex f  = let (tks, err, _) = lex' ([], [], LexerState f 1 1)
         in (reverse tks, reverse err)
    where lex' :: ([Token], [String], LexerState) -> ([Token], [String], LexerState)
          lex' (tks, err, l) =
              case getLexerContent l of
                  ("")        ->      (newToken EOF          l "End of file"  :tks, err, l)
                  ('/':'/':_) -> lex' (tks, err, updateLexerLine l)
                  ('\n':_)    -> lex' (tks, err, updateLexerLine l)
                  (' ':_)     -> lex' (tks, err, updateLexerState l 1)
                  ('\t':_)    -> lex' (tks, err, updateLexerState l 1)
                  ('\r':_)    -> lex' (tks, err, updateLexerState l 1)
                  (';':_)     -> lex' (newToken SEMICOLON    l ";" :tks, err, updateLexerState l 1)
                  ('(':_)     -> lex' (newToken PAREN_LEFT   l "(" :tks, err, updateLexerState l 1)
                  (')':_)     -> lex' (newToken PAREN_RIGHT  l ")" :tks, err, updateLexerState l 1)
                  ('{':_)     -> lex' (newToken BRACE_LEFT   l "{" :tks, err, updateLexerState l 1)
                  ('}':_)     -> lex' (newToken BRACE_RIGHT  l "}" :tks, err, updateLexerState l 1)
                  ('+':_)     -> lex' (newToken PLUS         l "+" :tks, err, updateLexerState l 1)
                  ('-':_)     -> lex' (newToken MINUS        l "-" :tks, err, updateLexerState l 1)
                  ('*':_)     -> lex' (newToken STAR         l "*" :tks, err, updateLexerState l 1)
                  ('!':_)     -> lex' (newToken BANG         l "!" :tks, err, updateLexerState l 1)
                  ('/':_)     -> lex' (newToken SLASH        l "/" :tks, err, updateLexerState l 1)
                  ('=':'=':_) -> lex' (newToken DOUBLE_EQUAL l "==":tks, err, updateLexerState l 2)
                  ('=':_)     -> lex' (newToken EQUAL        l "=" :tks, err, updateLexerState l 1)
                  ('"':fs)    ->
                      case string fs of
                          (Left s) -> lex' (newToken (STRING s) l ("\"" ++ s ++ "\""):tks,
                                            err, updateLexerState l (length s + 2))
                          (Right erro) -> (tks, erro:err, updateLexerLine l)
                  (x:fs)
                      | isDigit x ->
                          let f = takeWhile isDigit (x:fs)
                              s = length f
                              i = read f :: Integer
                          in lex' (newToken (INTEGER i) l f:tks, err, updateLexerState l s)
                      | isAlphaNumeric x ->
                          let f = takeWhile isAlphaNumeric (x:fs)
                              s = length f
                              i = identifierTokenType f
                          in lex' (newToken i l f:tks, err, updateLexerState l s)
                      | otherwise -> lex' (tks, unwords["Unknown Character", [x], ":", show (ord x)]:err,
                                      updateLexerState l 1)

identifierTokenType :: Lexeme -> TokenType
identifierTokenType "var"  = VAR
identifierTokenType "val"  = VAL
identifierTokenType "if"   = IF
identifierTokenType "elif" = ELIF
identifierTokenType "else" = ELSE
identifierTokenType l      = IDENTIFIER l

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

string :: String -> Either String String
string s =
    case stringSize s of
        (Left ssize) -> Left $ take ssize s
        (Right err)  -> Right err

isAlphaNumeric :: Char -> Bool
isAlphaNumeric '_'        = True
isAlphaNumeric c
    | c `elem` ['a'..'z'] = True
    | c `elem` ['A'..'Z'] = True
    | c `elem` ['0'..'9'] = True
    | otherwise           = False
