module Interpreter where

import qualified Data.Map.Lazy as Map
import Data.Maybe

import Lexer()
import Parser

data Value = StringValue String | IntegerValue Integer | BooleanValue Bool | ErrorValue String
data ValueType = StringType | IntegerType | BooleanType

data Environment = Environment (Map.Map String Value) (Maybe Environment)

instance Show Environment where
    show (Environment vars (Just env)) = show vars ++ " { " ++ show env ++ " } "
    show (Environment vars _)          = show vars

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

interpret :: [Stmt] -> Environment -> (IO (), Environment, Bool)
interpret stmts' env' =
    let (stmt, io, envi) = interpret' (stmts', putStr "", env')
    in (io, envi, null stmt)
    where interpret' :: ([Stmt], IO (), Environment) -> ([Stmt], IO (), Environment)
          interpret' ([],                      io, e)  = ([], io, e)
          interpret' (PrintStmt expr:stmts,    io, e)  =
              let (out, env, success) = interpretPrintStmt expr e
                  ret = (stmts, io >> out, env)
              in if success then interpret' ret else ret
          interpret' (ExprStmt expr:stmts,     io, e)  =
              let (out, env, success) = interpretExprStmt  expr e
                  ret = (stmts, io >> out, env)
              in if success then interpret' ret else ret
          interpret' (VarStmt name expr:stmts, io, e) =
              let (out, env, success) = interpretVarDef name expr e
                  ret = (stmts, io >> out, env)
              in if success then interpret' ret else ret
          interpret' (Block stmt:stmts,        io, e) =
              let (out, Environment _ en, success) = interpret stmt $ newEnvWithParent e
                  ret = (stmts, io >> out, fromMaybe (error "Root environment not found") en)
              in if success then interpret' ret else ret
          interpret' (IfStmt expr thenBranch elseBranch:stmts, io, e) =
              let (out, env, success) = interpretIfStmt expr thenBranch elseBranch e
                  ret = (stmts, io >> out, env)
              in if success then interpret' ret else ret
          interpret' (WhileStmt expr body:stmts, io, e) =
              let (out, env, success, loop) = interpretWhileStmt expr body e
                  ret = if loop then (WhileStmt expr body:stmts, io >> out, env)
                                else (stmts, io >> out, env)
              in if success then interpret' ret else ret
          interpret' (UnknownStmt:stmts,       io, e)  = interpret' (stmts, io, e)

interpretPrintStmt :: Expr -> Environment -> (IO (), Environment, Bool)
interpretPrintStmt expr env =
    case interpretExpr expr env of
        (StringValue  s, envi) -> (putStrLn s, envi, True)
        (IntegerValue i, envi) -> (print i, envi, True)
        (BooleanValue b, envi) -> (print b, envi, True)
        (ErrorValue   e, envi) -> (putStrLn e, envi, False)

interpretExprStmt :: Expr -> Environment -> (IO (), Environment, Bool)
interpretExprStmt expr env =
    case interpretExpr expr env of
        (ErrorValue e, envi) -> (putStrLn e, envi, False)
        (_,            envi) -> (putStr "", envi, True)

interpretVarDef :: String -> Expr -> Environment -> (IO (), Environment, Bool)
interpretVarDef name expr env =
    case envVarExists name env of
        (False) ->
            case interpretExpr expr env of
                (ErrorValue e, envi) -> (putStrLn e, envi, False)
                (val, envi)          -> (putStr "", envSetVar name val envi, True)
        (True)  -> (putStrLn $ unwords ["Variable", "'" ++ name ++ "'", "is already defined"], env, False)

interpretIfStmt :: Expr -> Stmt -> Maybe Stmt -> Environment -> (IO (), Environment, Bool)
interpretIfStmt expr ifBranch elseBranch env =
    case interpretExpr expr env of
        (ErrorValue      e,  envi) -> (putStrLn e, envi, False)
        (BooleanValue True,  envi) -> interpret [ifBranch] envi
        (BooleanValue False, envi) ->
            case elseBranch of
                (Just branch) -> interpret [branch] envi
                (Nothing    ) -> (putStr "", envi, True)
        (_,                  envi) -> (putStrLn "An if statement require a boolean result to its expression", envi, False)

interpretWhileStmt :: Expr -> Stmt -> Environment -> (IO (), Environment, Bool, Bool)
interpretWhileStmt expr body env =
    case interpretExpr expr env of
        (BooleanValue True, envi) ->
            let (io, e, success) = interpret [body] envi
            in (io, e, success, True)
        (BooleanValue False, envi) -> (putStr "", envi, True, False)
        (_, _) -> (putStr "", env, False, True)

envSetVar :: String -> Value -> Environment -> Environment
envSetVar name val (Environment vars env) = Environment (Map.insert name val vars) env

envSetExistingVar :: String -> Value -> Environment -> (Value, Environment)
envSetExistingVar name val (Environment vars env) =
    case Map.lookup name vars of
        (Just  prev) -> if typeOfValue prev == typeOfValue val
                        then (val, Environment (Map.insert name val vars) env)
                        else (ErrorValue "Changing type of a variable isn't allowed, you need to explicitly cast it", Environment vars env)
        (Nothing) ->
            case env of
                (Just  e) ->
                    let (v, en) = envSetExistingVar name val e
                    in (v, Environment vars (Just en))
                (Nothing) ->
                    (ErrorValue $ unwords ["Variable", "'" ++ name ++ "'", "isn't defined"],
                    Environment vars Nothing)


envVarExists :: String -> Environment -> Bool
envVarExists name (Environment vars env) =
    case Map.lookup name vars of
        (Just  _) -> True
        (Nothing) -> maybe False (envVarExists name) env

envGetVar :: String -> Environment -> Value
envGetVar name (Environment vars env) =
    case Map.lookup name vars of
        (Just val) -> val
        (Nothing ) -> maybe (ErrorValue $ unwords ["Variable", "'" ++ name ++ "'", "isn't defined"]) (envGetVar name) env

interpretExpr :: Expr -> Environment -> (Value, Environment)
interpretExpr UnknownExpr                 env = (ErrorValue "Unknown expressions cannot be evaluated", env)
interpretExpr (LiteralExpr l)             env = (interpretExprLiteral l, env)
interpretExpr (GroupingExpr expr)         env = interpretExpr expr env
interpretExpr (UnaryExpr op expr)         env =
    let (val, envi) = interpretExpr expr  env
    in (interpretExprUnary op val, envi)
interpretExpr (VariableExpr name)         env = (envGetVar name env, env)
interpretExpr (AssignementExpr name expr) env =
    let (result, envi) = interpretExpr expr env
    in envSetExistingVar name result envi
interpretExpr (BinaryExpr left op right)  env =
    let (val1, env1) = interpretExpr left env
        (val2, env2) = interpretExpr right env1
    in (interpretExprBinary op val1 val2, env2)
interpretExpr (LogicalExpr left op right) env = interpretExprLogical left op right env

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
    case cast val StringType of
        (StringValue s') -> StringValue (s ++ s')
        (_             ) -> ErrorValue $ "Error when casting value " ++ show val ++ "  to a StringValue"
interpretExprBinary op val1 val2                            = ErrorValue (unwords
                                                        ["Unknown binary expression",
                                                        show op, "between types",
                                                        "'" ++ show (typeOfValue val1) ++ "'",
                                                        "and",
                                                        "'" ++ show (typeOfValue val2) ++ "'"])


interpretExprLogical :: Expr -> Symbol -> Expr -> Environment -> (Value, Environment)
interpretExprLogical left LOR right env =
    case interpretExpr left env of
        (BooleanValue True,  en) -> (BooleanValue True, en)
        (BooleanValue False, en) -> interpretExpr right en
        (val               , en) -> (ErrorValue $ unwords ["Left side of a logical expression have to be a boolean, got:", show val], en)
interpretExprLogical left LAND right env =
    case interpretExpr left env of
        (BooleanValue True,  en) -> interpretExpr right en
        (BooleanValue False, en) -> (BooleanValue False, en)
        (val               , en) -> (ErrorValue $ unwords ["Left side of a logical expression have to be a boolean, got:", show val], en)
interpretExprLogical _ sym _ _ = error $ "Unexpected logical operation: " ++ show sym

isEqual :: Value -> Value -> Value
isEqual val1 val2
    | typeOfValue val1 /= typeOfValue val2 = BooleanValue False
    | otherwise =
        case (val1, val2) of
            (StringValue  a, StringValue  b) -> BooleanValue (a == b)
            (IntegerValue a, IntegerValue b) -> BooleanValue (a == b)
            (BooleanValue a, BooleanValue b) -> BooleanValue (a == b)
            (_             ,              _) -> ErrorValue "Unhandled equality operation"

cast :: Value -> ValueType -> Value
cast (IntegerValue i) StringType  = StringValue (show i)
cast (BooleanValue b) StringType  = StringValue (show b)
cast val typ = ErrorValue $ unwords ["Casting value:", show val, "to", show typ, "is not currently supported"]

typeOfValue :: Value -> ValueType
typeOfValue (StringValue  _) = StringType
typeOfValue (IntegerValue _) = IntegerType
typeOfValue (BooleanValue _) = BooleanType
typeOfValue (_)              = error "Unknown type of value"
