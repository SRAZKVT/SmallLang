module Parser where

import Lexer

data Stmt =   ExprStmt  Expr
            | PrintStmt Expr

instance Show Stmt where
    show (ExprStmt  expr) = "ExprStmt: " ++ show expr
    show (PrintStmt expr) = "PrintStmt: " ++ show expr

data Expr =   BinaryExpr Expr Token Expr
            | GroupingExpr Expr
            | LiteralExpr Literal
            | UnaryExpr Token Expr
            | UnknownExpr

instance Show Expr where
    show (BinaryExpr left operator right) = wrap $ unwords [show operator, show left, show right]
    show (GroupingExpr expr)              = wrap $ show expr
    show (LiteralExpr value)              = show value
    show (UnaryExpr operator right)       = wrap $ unwords [show operator, show right]
    show UnknownExpr                      = "{unknown}"

wrap :: String -> String
wrap s = "(" ++ s ++ ")"

data Literal = IntegerLiteral Integer | StringLiteral String | BooleanLiteral Bool

instance Show Literal where
    show (IntegerLiteral i) = show i
    show (StringLiteral  s) = "'" ++ s ++ "'"
    show (BooleanLiteral b) = show b

parse :: [Token] -> ([Stmt], [String])
parse tks =
    let (stmts, _, err) = parse' ([], tks, [])
    in (reverse stmts, reverse err)
    where parse' :: ([Stmt], [Token], [String]) -> ([Stmt], [Token], [String])
          parse' (stmts, [EOF], err) = (stmts, [EOF], err)
          parse' (stmts, tks, err) =
              let (stmt, tks', err') = statement tks
              in parse' (stmt:stmts, tks', err' ++ err)
    
statement :: [Token] -> (Stmt, [Token], [String])
statement ((IDENTIFIER "print"):tks) = printStatement tks
statement tks                        = exprStatement tks

printStatement :: [Token] -> (Stmt, [Token], [String])
printStatement (PAREN_LEFT:tks) =
    let (expr, tks', err) = expression tks
    in stmtConsume SEMICOLON $
       stmtConsume PAREN_RIGHT
       (PrintStmt expr, tks', err)

exprStatement :: [Token] -> (Stmt, [Token], [String])
exprStatement tks =
    let (expr, tks', err) = expression tks
    in stmtConsume SEMICOLON
       (ExprStmt expr, tks', err)

stmtConsume :: Token -> (Stmt, [Token], [String]) -> (Stmt, [Token], [String])
stmtConsume tk (stmt, xs:tks, err)
    | tk == xs = (stmt, tks, err)
    | otherwise = (stmt, tks, unwords ["Unexpected token! Expected:", show tk,
                                       "but got:", show xs]:err)


expression :: [Token] -> (Expr, [Token], [String])
expression = equality

equality :: [Token] -> (Expr, [Token], [String])
equality tks = equality' $ comparison tks
    where equality' :: (Expr, [Token], [String]) -> (Expr, [Token], [String])
          equality' (expr, tks, err)
              | head tks `elem` [DOUBLE_EQUAL] =
                  let (expr', tks', err') = comparison (tail tks)
                  in equality' (BinaryExpr expr (head tks) expr', tks', err' ++ err)
              | otherwise                      = (expr, tks, err)

comparison :: [Token] -> (Expr, [Token], [String])
comparison tks = comparison' $ term tks
    where comparison' :: (Expr, [Token], [String]) -> (Expr, [Token], [String])
          comparison' (expr, tks, err)
              | head tks `elem` [] =
                  let (expr', tks', err') = term (tail tks)
                  in comparison' (BinaryExpr expr (head tks) expr', tks', err' ++ err)
              | otherwise                 = (expr, tks, err)

term :: [Token] -> (Expr, [Token], [String])
term tks = term' $ factor tks
    where term' :: (Expr, [Token], [String]) -> (Expr, [Token], [String])
          term' (expr, tks, err)
              | head tks `elem` [PLUS, MINUS] =
                  let (expr', tks', err') = factor (tail tks)
                  in term' (BinaryExpr expr (head tks) expr', tks', err' ++ err)
              | otherwise                 = (expr, tks, err)

factor :: [Token] -> (Expr, [Token], [String])
factor tks = factor' $ unary tks
    where factor' :: (Expr, [Token], [String]) -> (Expr, [Token], [String])
          factor' (expr, tks, err)
              | head tks `elem` [STAR, SLASH] =
                  let (expr', tks', err') = unary (tail tks)
                  in factor' (BinaryExpr expr (head tks) expr', tks', err' ++ err)
              | otherwise                 = (expr, tks, err)

unary :: [Token] -> (Expr, [Token], [String])
unary (tk:tks)
    | tk `elem` [MINUS, BANG] = let (expr, tks', err) = unary tks
                                in (UnaryExpr tk expr, tks', err)
    | otherwise               = primary (tk:tks)

primary :: [Token] -> (Expr, [Token], [String])
primary ((IDENTIFIER "false"):tks) = (LiteralExpr (BooleanLiteral False), tks, [])
primary ((IDENTIFIER "true"):tks) = (LiteralExpr (BooleanLiteral True), tks, [])
primary ((INTEGER i):tks) = (LiteralExpr (IntegerLiteral i), tks, [])
primary ((STRING s):tks) = (LiteralExpr (StringLiteral s), tks, [])
primary (PAREN_LEFT:tks) = let (expr, tks', err) = expression tks
                           in if head tks' == PAREN_RIGHT
                              then (GroupingExpr expr, tail tks', err)
                              else (GroupingExpr expr, tks', "Expect ')' after expression":err)
primary tks = (UnknownExpr, tks, ["Expect expression bug got: '" ++ show (head tks) ++ "'"])
