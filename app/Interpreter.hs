module Interpreter where

import Lexer
import Parser

data Value = StringValue String | IntegerValue Integer | BooleanValue Bool | ErrorValue String
data ValueType = StringType | IntegerType | BooleanType

instance Show Value where
    show (StringValue  s) = "\"" ++ s ++ "\""
    show (IntegerValue i) = show i
    show (BooleanValue b) = show b
    show (ErrorValue   e) = "[ERROR]: " ++ e

instance Show ValueType where
    show StringType = "String"
    show IntegerType = "Integer"
    show BooleanType = "Boolean"

instance Eq ValueType where
    (==) StringType StringType   = True
    (==) BooleanType BooleanType = True
    (==) IntegerType IntegerType = True
    (==) _ _                     = False

-- No interpreter state currently, that will come in later
interpret :: [Stmt] -> IO ()
interpret [] = putStr ""
interpret ((PrintStmt expr):stmts) = interpretPrintStmt expr >> interpret stmts
interpret ((ExprStmt expr):stmts)  = interpretExprStmt  expr >> interpret stmts

interpretPrintStmt :: Expr -> IO ()
interpretPrintStmt expr =
    case interpretExpr expr of
        (StringValue  s) -> putStrLn s
        (IntegerValue i) -> print i
        (BooleanValue b) -> print b
        (ErrorValue   e) -> putStrLn e

interpretExprStmt :: Expr -> IO ()
interpretExprStmt expr =
    case interpretExpr expr of
        (ErrorValue e) -> putStrLn e
        (_)            -> putStr ""

interpretExpr :: Expr -> Value
interpretExpr (LiteralExpr l) = interpretExprLiteral l
interpretExpr (GroupingExpr expr) = interpretExpr expr
interpretExpr (UnaryExpr op expr) = interpretExprUnary op $ interpretExpr expr
interpretExpr (BinaryExpr left op right) =
    interpretExprBinary op (interpretExpr left) (interpretExpr right)

interpretExprLiteral :: Literal -> Value
interpretExprLiteral (StringLiteral  s) = StringValue  s
interpretExprLiteral (IntegerLiteral i) = IntegerValue i
interpretExprLiteral (BooleanLiteral b) = BooleanValue b

interpretExprUnary :: Token -> Value -> Value
interpretExprUnary _ (ErrorValue s)       = ErrorValue s
interpretExprUnary MINUS (IntegerValue i) = IntegerValue (negate i)
interpretExprUnary BANG (BooleanValue b)  = BooleanValue (not b)
interpretExprUnary op val                 = ErrorValue (unwords
                                                        ["Unknown unary expression",
                                                        show op, "with type",
                                                        "'" ++ show (typeOfValue val) ++ "'"])

interpretExprBinary :: Token -> Value -> Value -> Value
interpretExprBinary _ (ErrorValue s1) (ErrorValue s2)       = ErrorValue (s1 ++ s2)
interpretExprBinary _ (ErrorValue s) _                      = ErrorValue s
interpretExprBinary _ _ (ErrorValue s)                      = ErrorValue s
interpretExprBinary PLUS  (IntegerValue a) (IntegerValue b) = IntegerValue (a + b)
interpretExprBinary MINUS (IntegerValue a) (IntegerValue b) = IntegerValue (a - b)
interpretExprBinary STAR  (IntegerValue a) (IntegerValue b) = IntegerValue (a * b)
interpretExprBinary SLASH (IntegerValue a) (IntegerValue b) = IntegerValue (a `div` b)
interpretExprBinary DOUBLE_EQUAL val1 val2                  = isEqual val1 val2
interpretExprBinary PLUS  (StringValue  a) (StringValue  b) = StringValue (a ++ b)
interpretExprBinary PLUS  (StringValue  s) val              =
    let (StringValue s') = cast val StringType
    in StringValue (s ++ s')
interpretExprBinary op val1 val2                            = ErrorValue (unwords
                                                        ["Unknown binary expression",
                                                        show op, "between types",
                                                        "'" ++ show (typeOfValue val1) ++ "'",
                                                        "and",
                                                        "'" ++ show (typeOfValue val2) ++ "'"])

isEqual :: Value -> Value -> Value
isEqual val1 val2
    | typeOfValue val1 /= typeOfValue val2 = BooleanValue False
    | otherwise =
        case (val1, val2) of
            (StringValue  a, StringValue  b) -> BooleanValue (a == b)
            (IntegerValue a, IntegerValue b) -> BooleanValue (a == b)
            (BooleanValue a, BooleanValue b) -> BooleanValue (a == b)

cast :: Value -> ValueType -> Value
cast (IntegerValue i) StringType  = StringValue (show i)
cast (BooleanValue b) StringType  = StringValue (show b)

typeOfValue :: Value -> ValueType
typeOfValue (StringValue  _) = StringType
typeOfValue (IntegerValue _) = IntegerType
typeOfValue (BooleanValue _) = BooleanType
