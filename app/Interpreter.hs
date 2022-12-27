module Interpreter where

import qualified Data.Map.Lazy as Map
import Data.Maybe

import Lexer
import Parser

data Value = StringValue String | IntegerValue Integer | BooleanValue Bool | ErrorValue String
data ValueType = StringType | IntegerType | BooleanType

data Environment = Environment (Map.Map String Value) (Maybe Environment)

instance Show Environment where
    show (Environment map _) = show map

newEnv :: Environment
newEnv = Environment Map.empty Nothing

newEnvWithParent :: Environment -> Environment
newEnvWithParent e = Environment Map.empty (return e)

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
interpret :: [Stmt] -> Environment -> (IO (), Environment)
interpret stmts env =
    let (_, io, envi) = interpret' (stmts, putStr "", env)
    in (io, envi)
    where interpret' :: ([Stmt], IO (), Environment) -> ([Stmt], IO (), Environment)
          interpret' ([],                      io, e)  = ([], io, e)
          interpret' (PrintStmt expr:stmts,    io, e)  =
              let (out, env) = interpretPrintStmt expr e
              in interpret' (stmts, io >> out, env)
          interpret' (ExprStmt expr:stmts,     io, e)  =
              let (out, env) = interpretExprStmt  expr e
              in interpret' (stmts, io >> out, env)
          interpret' (VarStmt name expr:stmts, io, e) =
              let (out, env) = interpretVarDef name expr e
              in interpret' (stmts, io >> out, env)
          interpret' (UnknownStmt:stmts,       io, e)  =  interpret' (stmts, io, e)

interpretPrintStmt :: Expr -> Environment -> (IO (), Environment)
interpretPrintStmt expr env =
    case interpretExpr expr env of
        (StringValue  s, envi) -> (putStrLn s, envi)
        (IntegerValue i, envi) -> (print i, envi)
        (BooleanValue b, envi) -> (print b, envi)
        (ErrorValue   e, envi) -> (putStrLn e, envi)

interpretExprStmt :: Expr -> Environment -> (IO (), Environment)
interpretExprStmt expr env =
    case interpretExpr expr env of
        (ErrorValue e, envi) -> (putStrLn e, envi)
        (_,            envi) -> (putStrLn "", envi)

interpretVarDef :: String -> Expr -> Environment -> (IO (), Environment)
interpretVarDef name expr env =
    case interpretExpr expr env of
        (ErrorValue e, envi) -> (putStrLn e, envi)
        (val, envi)          -> (putStr "", envSetVar name val envi)

envSetVar :: String -> Value -> Environment -> Environment
envSetVar name val (Environment map env) = Environment (Map.insert name val map) env

envGetVar :: String -> Environment -> Value
envGetVar name (Environment map _) =
    fromMaybe (ErrorValue (unwords ["variable", name, "is not defined"])) (Map.lookup name map)

interpretExpr :: Expr -> Environment -> (Value, Environment)
interpretExpr (LiteralExpr l)            env = (interpretExprLiteral l, env)
interpretExpr (GroupingExpr expr)        env = interpretExpr expr env
interpretExpr (UnaryExpr op expr)        env =
    let (val, envi) = interpretExpr expr env
    in (interpretExprUnary op val, envi)
interpretExpr (VariableExpr name)        env = (envGetVar name env, env)
interpretExpr (BinaryExpr left op right) env =
    let (val1, env1) = interpretExpr left  env
        (val2, env2) = interpretExpr right env1
    in (interpretExprBinary op val1 val2, env)

interpretExprLiteral :: Literal -> Value
interpretExprLiteral (StringLiteral  s) = StringValue  s
interpretExprLiteral (IntegerLiteral i) = IntegerValue i
interpretExprLiteral (BooleanLiteral b) = BooleanValue b

interpretExprUnary :: Symbol -> Value -> Value
interpretExprUnary _ (ErrorValue s)     = ErrorValue s
interpretExprUnary SUB (IntegerValue i) = IntegerValue (negate i)
interpretExprUnary NOT (BooleanValue b) = BooleanValue (not b)
interpretExprUnary op val               = ErrorValue (unwords
                                                        ["Unknown unary expression",
                                                        show op, "with type",
                                                        "'" ++ show (typeOfValue val) ++ "'"])

interpretExprBinary :: Symbol -> Value -> Value -> Value
interpretExprBinary _ (ErrorValue s1) (ErrorValue s2)       = ErrorValue (s1 ++ s2)
interpretExprBinary _ (ErrorValue s) _                      = ErrorValue s
interpretExprBinary _ _ (ErrorValue s)                      = ErrorValue s
interpretExprBinary ADD (IntegerValue a) (IntegerValue b)   = IntegerValue (a + b)
interpretExprBinary SUB (IntegerValue a) (IntegerValue b)   = IntegerValue (a - b)
interpretExprBinary MUL  (IntegerValue a) (IntegerValue b)  = IntegerValue (a * b)
interpretExprBinary DIV _                (IntegerValue 0)   = ErrorValue "Division by zero."
interpretExprBinary DIV (IntegerValue a) (IntegerValue b)   = IntegerValue (a `div` b)
interpretExprBinary EQU val1 val2                           = isEqual val1 val2
interpretExprBinary ADD  (StringValue  a) (StringValue  b)  = StringValue (a ++ b)
interpretExprBinary ADD  (StringValue  s) val               =
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
