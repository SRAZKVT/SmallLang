module Parser where

import Lexer

type IdentifierName = String

data Stmt = ExprStmt               Expr
          | PrintStmt              Expr
          | VarStmt IdentifierName Expr
          | Block [Stmt]
          | IfStmt Expr Stmt (Maybe Stmt)
          | WhileStmt Expr Stmt
          | UnknownStmt

instance Show Stmt where
    show (ExprStmt  expr)                              = "ExprStmt: " ++ show expr
    show (PrintStmt expr)                              = "PrintStmt: " ++ show expr
    show (VarStmt name expr)                           = "VarDecStmt: '" ++ name ++ "' = " ++ show expr
    show (Block stmts)                                 = "Block: " ++ "{" ++ show stmts ++ "}"
    show (IfStmt expr thenBranch (Just elseBranch))    = unwords ["IfStmt:", show expr, show thenBranch,
                                                              "otherwise", show elseBranch]
    show (IfStmt expr thenBranch Nothing)              = unwords ["IfStmt:", show expr, show thenBranch]
    show (WhileStmt expr stmt)                         = unwords ["WhileStmt:", show expr, show stmt]
    show UnknownStmt                                   = "UnknownStmt"

data Expr = BinaryExpr Expr Symbol Expr
          | GroupingExpr Expr
          | LiteralExpr Literal
          | UnaryExpr Symbol Expr
          | VariableExpr IdentifierName
          | AssignementExpr IdentifierName Expr
          | LogicalExpr Expr Symbol Expr
          | UnknownExpr

instance Show Expr where
    show (BinaryExpr left operator right) = wrap $ unwords [show operator, show left, show right]
    show (GroupingExpr expr)              = wrap $ show expr
    show (LiteralExpr value)              = wrap $ show value
    show (UnaryExpr operator right)       = wrap $ unwords [show operator, show right]
    show (VariableExpr name)              = wrap $ unwords ["variable:", "\"" ++ name ++ "\""]
    show (AssignementExpr name expr)      = wrap $ unwords ["variable:", "\"" ++ name ++ "\"", "=",
                                                            show expr]
    show (LogicalExpr left sym right)     = wrap $ unwords [show left, show sym, show right]
    show UnknownExpr                      = "{unknown}"

data Symbol = ADD
            | SUB
            | MUL
            | DIV
            | EQU
            | NOT
            | LOR
            | LAND

instance Show Symbol where
    show ADD  = "+"
    show SUB  = "-"
    show MUL  = "*"
    show DIV  = "/"
    show EQU  = "=="
    show NOT  = "!"
    show LOR  = "||"
    show LAND = "&&"

instance Eq Symbol where
    (==) ADD ADD   = True
    (==) SUB SUB   = True
    (==) MUL MUL   = True
    (==) DIV DIV   = True
    (==) EQU EQU   = True
    (==) NOT NOT   = True
    (==) LOR LOR   = True
    (==) LAND LAND = True
    (==) _ _       = False

wrap :: String -> String
wrap s = "(" ++ s ++ ")"

data Literal = IntegerLiteral Integer | StringLiteral String | BooleanLiteral Bool

instance Show Literal where
    show (IntegerLiteral i) = show i
    show (StringLiteral  s) = "'" ++ s ++ "'"
    show (BooleanLiteral b) = show b

symbolFromToken :: Token -> Symbol
symbolFromToken tk = symbolFromTokenType $ tokenType tk

symbolFromTokenType :: TokenType -> Symbol
symbolFromTokenType PLUS         = ADD
symbolFromTokenType MINUS        = SUB
symbolFromTokenType STAR         = MUL
symbolFromTokenType SLASH        = DIV
symbolFromTokenType DOUBLE_EQUAL = EQU
symbolFromTokenType BANG         = NOT
symbolFromTokenType tk           = error $ unwords ["Symbol from unexpected token requested:", show tk]

parse :: [Token] -> ([Stmt], [String])
parse tks =
    let (stmts, _, err) = parse' ([], tks, [])
    in (reverse stmts, reverse err)
    where parse' :: ([Stmt], [Token], [String]) -> ([Stmt], [Token], [String])
          parse' (_, [], err) = error $ unlines err
          parse' (stmts, tk':tks', err)
              | tokenType tk' == EOF = (stmts, [], err)
              | otherwise =
                  let (stmt, tks'', err') = declaration (tk':tks')
                  in parse' (stmt:stmts, tks'', err' ++ err)

declaration :: [Token] -> (Stmt, [Token], [String])
declaration [] = error "Empty list of tokens"
declaration (tk:tks) =
    case tokenType tk of
        (VAR) -> varDeclaration tks
        (_)                -> statement (tk:tks)

varDeclaration :: [Token] -> (Stmt, [Token], [String])
varDeclaration [] = error "Empty list of tokens"
varDeclaration (tk:tks) =
    case tokenType tk of
        (IDENTIFIER s) ->
            (case tokenType $ head tks of
                (EQUAL    ) ->
                    let (expr, tks', err) = expression $ tail tks
                    in stmtConsume SEMICOLON $
                       (VarStmt s expr, tks', err)
                (SEMICOLON) -> (UnknownStmt, tail tks, [parseError (head tks) "Uninitialized variables are not supported"])
                (_        ) -> (UnknownStmt, tks, [parseError tk "Expected '=' to initialize variables"]))
        (_) -> (UnknownStmt, tks, [parseError tk "Expected valid variable name"])

statement :: [Token] -> (Stmt, [Token], [String])
statement [] = error "Empty list of tokens"
statement (tk:tks) =
    case tokenType tk of
        (IDENTIFIER "print") -> printStatement tks
        (BRACE_LEFT)         -> block tks
        (IF)                 -> ifStatement tks
        (WHILE)              -> whileStatement tks
        (_)                  -> exprStatement $ tk:tks

printStatement :: [Token] -> (Stmt, [Token], [String])
printStatement [] = error "Empty list of tokens"
printStatement (tk:tks) =
    case tokenType tk of
        (EOF)        -> (UnknownStmt, (tk:tks), [parseError tk "Print requires an expression encased in parenthesis to print"])
        (PAREN_LEFT) ->
            let (expr, tks', err) = expression tks
            in stmtConsume SEMICOLON $
               stmtConsume PAREN_RIGHT
               (PrintStmt expr, tks', err)
        (_)          -> (UnknownStmt, tks, [parseError tk "Print requires an expression encased in parenthesis to print"])

block :: [Token] -> (Stmt, [Token], [String])
block tks =
    let (tks', stmts, err) = block' (tks, [], [])
    in (Block $ reverse stmts, tks', err)
    where block' :: ([Token], [Stmt], [String]) -> ([Token], [Stmt], [String])
          block' ([], _, err) = error $ unlines err
          block' (tk':tks', stmts, err) =
              case tokenType tk' of
                  (EOF)         -> (tk':tks', stmts, (parseError tk' "Unclosed block"):err)
                  (BRACE_RIGHT) -> (tks', stmts, err)
                  (_)           ->
                      let (stmt, tks'', err') = declaration $ tk':tks'
                      in block' (tks'', stmt:stmts, err' ++ err)

ifStatement :: [Token] -> (Stmt, [Token], [String])
ifStatement [] = error "Empty list of tokens"
ifStatement (tk:tks) =
    case tokenType tk of
        (PAREN_LEFT) ->
            let (expr, tks', err) = exprConsume PAREN_RIGHT $ expression tks
            in 
               case statement tks' of
                   (_, [], _) -> error "Empty list of tokens"
                   (thenBranch, tk'':tks'', err') ->
                       case tokenType tk'' of
                           (ELSE) ->
                               let (elseBranch, tks''', err'') = statement tks''
                               in (IfStmt expr thenBranch (Just elseBranch), tks''', err'' ++ err' ++ err)
                           (_) -> (IfStmt expr thenBranch Nothing, tk'':tks'', err' ++ err)
        (_         ) -> (UnknownStmt, tk:tks, [parseError tk "An if statement require it's expression opened by a left parenthesis"])

whileStatement :: [Token] -> (Stmt, [Token], [String])
whileStatement [] = error "Empty list of tokens"
whileStatement (tk:tks) =
    case tokenType tk of
        (PAREN_LEFT) ->
            let (expr, tks', err) = exprConsume PAREN_RIGHT $ expression tks
                (stmt, tks'', err') = statement tks'
            in (WhileStmt expr stmt, tks'', err' ++ err)
        (_         ) -> (UnknownStmt, tk:tks, ["Require '(' after a 'while'"])

exprStatement :: [Token] -> (Stmt, [Token], [String])
exprStatement tks =
    let (expr, tks', err) = expression tks
    in stmtConsume SEMICOLON
       (ExprStmt expr, tks', err)

stmtConsume :: TokenType -> (Stmt, [Token], [String]) -> (Stmt, [Token], [String])
stmtConsume _ (_, [], _) = error "Empty list of tokens"
stmtConsume etk (stmt, (Token tk l p lexeme):tks, err)
    | etk == tk = (stmt, tks, err)
    | otherwise = (stmt, (Token tk l p lexeme:tks),
    parseError (Token tk l p lexeme) (unwords ["Expected:", show etk, "but got:", lexeme]):err)

exprConsume :: TokenType -> (Expr, [Token], [String]) -> (Expr, [Token], [String])
exprConsume _ (_, [], _) = error "Empty list of tokens"
exprConsume etk (expr, (Token tk l p lexeme):tks, err)
    | etk == tk = (expr, tks, err)
    | otherwise = (expr, (Token tk l p lexeme):tks,
    parseError (Token tk l p lexeme) (unwords ["Expected:", show etk, "but got:", lexeme]):err)

parseError :: Token -> String -> String
parseError (Token _ l p lexeme) message =
    "[" ++ show l ++ ":" ++ show p ++ "]: \"" ++ lexeme ++ "\" -> " ++ message


expression :: [Token] -> (Expr, [Token], [String])
expression tks = orExpr tks

orExpr :: [Token] -> (Expr, [Token], [String])
orExpr tks = let (expr, tks', err) = andExpr tks
             in
                 case tokenType $ head tks' of
                     (DOUBLE_PIPE) ->
                         let (expr', tks'', err') = andExpr $ tail tks'
                         in (LogicalExpr expr LOR expr', tks'', err' ++ err)
                     (_          ) -> (expr, tks', err)

andExpr :: [Token] -> (Expr, [Token], [String])
andExpr tks = let (expr, tks', err) = assignement tks
              in
                  case tokenType $ head tks' of
                      (DOUBLE_AMPERSAND) ->
                          let (expr', tks'', err') = assignement $ tail tks'
                          in (LogicalExpr expr LAND expr', tks'', err' ++ err)
                      (_               ) -> (expr, tks', err)

assignement :: [Token] -> (Expr, [Token], [String])
assignement tks =
    let (expr, tks', err) = equality tks
    in
        case tokenType $ head tks' of
            (EQUAL) ->
                case (expr, assignement $ tail tks') of
                    (VariableExpr name, (expr', tks'', err')) ->
                        (AssignementExpr name expr', tks'', err' ++ err)
                    (_, (_, tks'', err')) -> (UnknownExpr, tks'', (parseError (head tks') "Unassignable target"):(err' ++ err))
            (_    ) -> (expr, tks', err)

equality :: [Token] -> (Expr, [Token], [String])
equality tks = equality' $ comparison tks
    where equality' :: (Expr, [Token], [String]) -> (Expr, [Token], [String])
          equality' (_, [], _) = error "Empty list of tokens"
          equality' (expr, tk':tks', err)
              |  tokenType tk' `elem` [DOUBLE_EQUAL] =
                  let (expr', tks'', err') = comparison tks'
                  in equality' (BinaryExpr expr (symbolFromToken tk') expr', tks'', err' ++ err)
              | otherwise = (expr, tk':tks', err)

comparison :: [Token] -> (Expr, [Token], [String])
comparison tks = comparison' $ term tks
    where comparison' :: (Expr, [Token], [String]) -> (Expr, [Token], [String])
          comparison' (_, [], _) = error "Empty list of tokens"
          comparison' (expr, tk':tks', err)
              | tokenType tk' `elem` [] =
                  let (expr', tks'', err') = term tks'
                  in comparison' (BinaryExpr expr (symbolFromToken tk') expr', tks'', err' ++ err)
              | otherwise = (expr, tk':tks', err)

term :: [Token] -> (Expr, [Token], [String])
term tks = term' $ factor tks
    where term' :: (Expr, [Token], [String]) -> (Expr, [Token], [String])
          term' (_, [], _) = error "Empty list of tokens"
          term' (expr, tk':tks', err)
              | tokenType tk' `elem` [PLUS, MINUS] =
                  let (expr', tks'', err') = factor tks'
                  in term' (BinaryExpr expr (symbolFromToken tk') expr', tks'', err' ++ err)
              | otherwise = (expr, tk':tks', err)

factor :: [Token] -> (Expr, [Token], [String])
factor tks = factor' $ unary tks
    where factor' :: (Expr, [Token], [String]) -> (Expr, [Token], [String])
          factor' (_, [], _) = error "Empty list of tokens"
          factor' (expr, tk':tks', err)
              | tokenType tk' `elem` [STAR, SLASH] =
                  let (expr', tks'', err') = unary tks'
                  in factor' (BinaryExpr expr (symbolFromToken tk') expr', tks'', err' ++ err)
              | otherwise = (expr, tk':tks', err)

unary :: [Token] -> (Expr, [Token], [String])
unary [] = error "Empty list of tokens"
unary (tk:tks)
    | tokenType tk `elem` [MINUS, BANG] = let (expr, tks', err) = unary tks
                                in (UnaryExpr (symbolFromToken tk) expr, tks', err)
    | otherwise               = primary (tk:tks)

primary :: [Token] -> (Expr, [Token], [String])
primary [] = error "Empty list of tokens"
primary (tk:tks) =
    case tokenType tk of
        (EOF         ) -> (UnknownExpr, tk:tks, [parseError tk "Expected expression but reached end of file"])
        (FALSE       ) -> (LiteralExpr (BooleanLiteral False), tks, [])
        (TRUE        ) -> (LiteralExpr (BooleanLiteral  True), tks, [])
        (INTEGER    i) -> (LiteralExpr (IntegerLiteral     i), tks, [])
        (IDENTIFIER s) -> (VariableExpr s, tks, [])
        (STRING     s) -> (LiteralExpr (StringLiteral      s), tks, [])
        (PAREN_LEFT  ) ->
            case expression tks of
                (_, [], _) -> error "Empty list of tokens"
                (expr, (Token PAREN_RIGHT _ _ _):tks', err) -> (GroupingExpr expr, tks', err)
                (_, tk':tks', err) -> (UnknownExpr, tk':tks', (parseError tk ""):err)
        (_           ) -> (UnknownExpr, tks, [parseError tk "Expect expression"])
