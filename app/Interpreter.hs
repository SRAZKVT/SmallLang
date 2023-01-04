module Interpreter where

import qualified Data.Map.Lazy as Map
import Data.Maybe

import Lexer()
import Parser

data Value = StringValue String
           | IntegerValue Integer
           | BooleanValue Bool
           | ErrorValue String
           | FunctionValue [IdentifierName] [Stmt]
           | NoValue

data ValueType = StringType
               | IntegerType
               | BooleanType
               | FunctionType
               deriving (Eq)

data Environment = Environment {
    variables :: Map.Map String Value,
    parentEnv :: Maybe Environment,
    ioOps     :: IO (),
    succesful :: Bool
}

instance Show Environment where
    show (Environment { variables = vars, parentEnv = Just env }) = show vars ++ " { " ++ show env ++ " } "
    show (Environment { variables = vars})          = show vars

envUnsuccesful :: Environment -> Environment
envUnsuccesful env = env { succesful = False,
                           parentEnv = envUnsuccesful <$> parentEnv env }

envAddIO :: IO () -> Environment -> Environment
envAddIO io env =
    case parentEnv env of
        (Nothing) -> env { ioOps = (ioOps env) >> io }
        (Just en) -> env { parentEnv = Just $ envAddIO io en }

newEnv :: Environment
newEnv = Environment Map.empty Nothing (putStr "") True

newEnvWithParent :: Environment -> Environment
newEnvWithParent e = newEnv { parentEnv = Just e }

envRoot :: Environment -> Environment
envRoot (Environment { parentEnv = Just env }) = envRoot env
envRoot env = env

instance Show Value where
    show (StringValue  s)         = "\"" ++ s ++ "\""
    show (IntegerValue i)         = show i
    show (BooleanValue b)         = show b
    show (FunctionValue args _ ) = "<<function " ++ show args ++ ">>"
    show (ErrorValue   e)         = "[ERROR]: " ++ e
    show (NoValue       )         = "NoValue"

instance Show ValueType where
    show StringType = "String"
    show IntegerType = "Integer"
    show BooleanType = "Boolean"
    show FunctionType = "Function"

interpret :: [Stmt] -> Environment -> (Environment, Bool, Maybe Value)
interpret stmts' env' =
    let (_, envi, val) = interpret' (stmts', env', Nothing)
    in (envi, succesful envi, val)
    where interpret' :: ([Stmt], Environment, Maybe Value) -> ([Stmt], Environment, Maybe Value)
          interpret' ([],                      e, _)  = ([], e, Nothing)
          interpret' (PrintStmt expr:stmts,    e, _)  =
              let env = interpretPrintStmt expr e
                  ret = (stmts, env, Nothing)
                  success = succesful env
              in if success then interpret' ret else ret
          interpret' (ExprStmt expr:stmts,     e, _)  =
              let env = interpretExprStmt expr e
                  ret = (stmts, env, Nothing)
                  success = succesful env
              in if success then interpret' ret else ret
          interpret' (VarStmt name expr:stmts, e, _) =
              let env = interpretVarDef name expr e
                  ret = (stmts, env, Nothing)
                  success = succesful env
              in if success then interpret' ret else ret
          interpret' (Block stmt:stmts,        e, _) =
              let (Environment {ioOps = io, parentEnv = en}, success, val) = interpret stmt
                                                                             $ newEnvWithParent e
                  updParentEnv = (envAddIO io $ fromMaybe (error "Root environment not found") en)
                                      {succesful = success}
                  ret = (stmts, updParentEnv, val)
              in if success && isNothing val then interpret' ret else ret
          interpret' (IfStmt expr thenBranch elseBranch:stmts, e, _) =
              let (env, val) = interpretIfStmt expr thenBranch elseBranch e
                  ret = (stmts, env, val)
                  success = succesful env
              in if success && isNothing val then interpret' ret else ret
          interpret' (WhileStmt expr body:stmts, e, _) =
              let (env, val, loop) = interpretWhileStmt expr body e
                  ret = if loop then (WhileStmt expr body:stmts, env, val)
                                else (stmts, env, val)
                  success = succesful env
              in if success && isNothing val then interpret' ret else ret
          interpret' (FunctionStmt name args content:stmts, e, _) =
              let env = interpretFuncDef name args content e
                  ret = (stmts, env, Nothing)
                  success = succesful env
              in if success then interpret' ret else ret
          interpret' (ReturnStmt expr:stmts, e, _) =
              let (val, env) = maybe (NoValue, e) (flip interpretExpr e) expr
                  ret = (stmts, env, Just val)
              in ret
          interpret' (UnknownStmt:stmts,       e, _)  = interpret' (stmts, e, Nothing)

interpretPrintStmt :: Expr -> Environment -> Environment
interpretPrintStmt expr env =
    case interpretExpr expr env of
        (StringValue   s, envi)      -> envAddIO (putStrLn s) envi
        (IntegerValue  i, envi)      -> envAddIO (print i) envi
        (BooleanValue  b, envi)      -> envAddIO (print b) envi
        (FunctionValue args _, envi) -> envAddIO (print (FunctionValue args [])) envi
        (ErrorValue    e, envi)      -> envAddIO (putStrLn e) $ envUnsuccesful envi
        (NoValue        , envi)      -> envAddIO (putStrLn "NoValue cannot be evaluated.")
                                        $ envUnsuccesful envi
interpretExprStmt :: Expr -> Environment -> Environment
interpretExprStmt expr env =
    case interpretExpr expr env of
        (ErrorValue e, envi) -> envAddIO (putStrLn e) $ envUnsuccesful envi
        (_,            envi) -> envi

interpretVarDef :: String -> Expr -> Environment -> Environment
interpretVarDef name expr env =
    case envVarExists name env of
        (False) ->
            case interpretExpr expr env of
                (ErrorValue e, envi) -> envAddIO (putStrLn e) $ envUnsuccesful envi
                (NoValue     , envi) -> envAddIO (putStrLn "NoValue cannot be evaluated")
                                        $ envUnsuccesful envi
                (val         , envi) -> envSetVar name val envi
        (True)  -> envAddIO (putStrLn $ unwords ["Variable", "'" ++ name ++ "'", "is already defined"])
                    $ envUnsuccesful env

interpretIfStmt :: Expr -> Stmt -> Maybe Stmt -> Environment -> (Environment, Maybe Value)
interpretIfStmt expr ifBranch elseBranch env =
    case interpretExpr expr env of
        (ErrorValue      e,  envi) -> (envAddIO (putStrLn e) envi, Nothing)
        (NoValue          ,  envi) -> (envAddIO (putStrLn "NoValue cannot be evaluated")
                                      $ envUnsuccesful envi, Nothing)
        (BooleanValue True,  envi) ->
            case interpret [ifBranch] envi of
                (en, _, val) -> (en, val) 
        (BooleanValue False, envi) ->
            case elseBranch of
                (Just branch) ->
                    case interpret [branch] envi of
                        (en, _, val) -> (en, val)
                (Nothing    ) -> (envi, Nothing)
        (_,                  envi) ->
            (envAddIO (putStrLn "An if statement require a boolean result to its expression")
            $ envUnsuccesful envi, Nothing)

interpretWhileStmt :: Expr -> Stmt -> Environment -> (Environment, Maybe Value, Bool)
interpretWhileStmt expr body env =
    case interpretExpr expr env of
        (ErrorValue      e, envi) -> (envAddIO (putStrLn e) $ envUnsuccesful envi, Nothing, False)
        (NoValue          , envi) -> (envAddIO (putStrLn "NoValue cannot be evaluated.")
                                      $ envUnsuccesful envi, Nothing, False)
        (BooleanValue True, envi) ->
            let (e, _, val) = interpret [body] envi
            in (e, val, True)
        (BooleanValue False, envi) -> (envi, Nothing, False)
        (_, e) -> (envAddIO (putStr "A while statement require a boolean result as its expression")
                   $ envUnsuccesful e, Nothing, False)

interpretFuncDef :: IdentifierName -> [IdentifierName] -> [Stmt] -> Environment -> Environment
interpretFuncDef name args content env
    | envVarExists name env = envAddIO (putStrLn $ unwords ["Variable of name", name, "already exists"])
                                         $ envUnsuccesful env
    | otherwise = envSetVar name (FunctionValue args content) env

envSetVar :: String -> Value -> Environment -> Environment
envSetVar name val env = env { variables = Map.insert name val (variables env) }

envSetExistingVar :: String -> Value -> Environment -> (Value, Environment)
envSetExistingVar name val env =
    case Map.lookup name $ variables env of
        (Just  prev) -> if typeOfValue prev == typeOfValue val
                        then (val, env { variables = Map.insert name val (variables env) })
                        else (ErrorValue "Changing type of a variable isn't allowed, you need to explicitly cast it", env)
        (Nothing) ->
            case parentEnv env of
                (Just  e) ->
                    let (v, en) = envSetExistingVar name val e
                    in (v, env { parentEnv = Just en })
                (Nothing) ->
                    (ErrorValue $ unwords ["Variable", "'" ++ name ++ "'", "isn't defined"],
                    env)


envVarExists :: String -> Environment -> Bool
envVarExists name (Environment { variables = vars, parentEnv = env }) =
    case Map.lookup name vars of
        (Just  _) -> True
        (Nothing) -> maybe False (envVarExists name) env

envGetVar :: String -> Environment -> Value
envGetVar name (Environment { variables = vars, parentEnv = env}) =
    case Map.lookup name vars of
        (Just val) -> val
        (Nothing ) -> maybe (ErrorValue $ unwords ["Variable", "'" ++ name ++ "'", "isn't defined"])
                      (envGetVar name) env

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
interpretExpr (CallExpr callee args)      env =
    let (func, envi)  = interpretExpr callee env
        (argsVals, en) = interpretArgs args envi
    in interpretFunction func argsVals en

interpretArgs :: [Expr] -> Environment -> ([Value], Environment)
interpretArgs exprs env =
    let (_, envi, vals) = interpretArgs' (exprs, env, [])
    in (reverse vals, envi)
    where interpretArgs' :: ([Expr], Environment, [Value]) -> ([Expr], Environment, [Value])
          interpretArgs' ([], en, vals) = ([], en, vals)
          interpretArgs' (expr:exprs', en, vals) =
              let (val, envi) = interpretExpr expr en
              in interpretArgs' (exprs', envi, val:vals)

interpretFunction :: Value -> [Value] -> Environment -> (Value, Environment)
interpretFunction (ErrorValue e) _ en = (ErrorValue e, en)
interpretFunction (NoValue) _ en = (ErrorValue "NoValue cannot be evaluated", en)
interpretFunction f args env =
    case f of
        (FunctionValue params content)
            | functionArity f /= length args -> (ErrorValue "Function arity mismatch", env)
            | otherwise ->
                let envi = setArgs params args $ newEnvWithParent $ envRoot env
                    (en, _, val) = interpret content envi
                    e = mergeEnvRoots env $ fromMaybe (error "Parent env not found") (parentEnv en)
                in (fromMaybe NoValue val, e)
        (_) -> (ErrorValue "Only functions can be called", env)

mergeEnvRoots :: Environment -> Environment -> Environment
mergeEnvRoots local rootEnv =
    case parentEnv local of
        (Nothing) -> local { variables = Map.intersection (variables local) (variables rootEnv),
                             succesful = succesful rootEnv, ioOps = ioOps rootEnv}
        (Just en) -> local { parentEnv = Just $ mergeEnvRoots en rootEnv }

setArgs :: [IdentifierName] -> [Value] -> Environment -> Environment
setArgs [] (_:_) _ = error "Mismatch in count of arguments and values"
setArgs (_:_) [] _ = error "Mismatch in count of arguments and values"
setArgs [] [] env  = env
setArgs (a:as) (v:vs) env = setArgs as vs $ envSetVar a v env

interpretExprLiteral :: Literal -> Value
interpretExprLiteral (StringLiteral  s) = StringValue  s
interpretExprLiteral (IntegerLiteral i) = IntegerValue i
interpretExprLiteral (BooleanLiteral b) = BooleanValue b

interpretExprUnary :: Symbol -> Value -> Value
interpretExprUnary _ (ErrorValue s)     = ErrorValue s
interpretExprUnary _ (NoValue)          = ErrorValue "NoValue cannot be evaluated."
interpretExprUnary SUB (IntegerValue i) = IntegerValue (negate i)
interpretExprUnary NOT (BooleanValue b) = BooleanValue (not b)
interpretExprUnary op val               = ErrorValue (unwords
                                                        ["Unknown unary expression",
                                                        show op, "with type",
                                                        "'" ++ show (typeOfValue val) ++ "'"])

interpretExprBinary :: Symbol -> Value -> Value -> Value
interpretExprBinary _ (NoValue) _                           = ErrorValue "NoValue cannot be evaluated."
interpretExprBinary _ _ (NoValue)                           = ErrorValue "NoValue cannot be evaluated."
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
        (NoValue           , en) -> (ErrorValue "NoValue cannot be evaluated.", en)
        (BooleanValue True,  en) -> (BooleanValue True, en)
        (BooleanValue False, en) ->
            case interpretExpr right en of
                (BooleanValue b, envi) -> (BooleanValue b, envi)
                (val,            envi) -> (ErrorValue $ unwords ["Right side of expression expected to be a boolean, got:", show $ typeOfValue val], envi)
        (val               , en) -> (ErrorValue $ unwords ["Left side of a logical expression have to be a boolean, got:", show val], en)
interpretExprLogical left LAND right env =
    case interpretExpr left env of
        (NoValue           , en) -> (ErrorValue "NoValue cannot be evaluated.", en)
        (BooleanValue True,  en) ->
            case interpretExpr right en of
                (BooleanValue b, envi) -> (BooleanValue b, envi)
                (val,            envi) -> (ErrorValue $ unwords ["Right side of expression expected to be a boolean, got:", show $ typeOfValue val], envi)
        (BooleanValue False, en) -> (BooleanValue False, en)
        (val               , en) -> (ErrorValue $ unwords ["Left side of a logical expression have to be a boolean, got:", show val], en)
interpretExprLogical _ sym _ _ = error $ "Unexpected logical operation: " ++ show sym

functionArity :: Value -> Int
functionArity (FunctionValue args _) = length args
functionArity _                      = -1

isEqual :: Value -> Value -> Value
isEqual val1 val2
    | typeOfValue val1 /= typeOfValue val2 = BooleanValue False
    | otherwise =
        case (val1, val2) of
            (StringValue  a, StringValue  b) -> BooleanValue (a == b)
            (IntegerValue a, IntegerValue b) -> BooleanValue (a == b)
            (BooleanValue a, BooleanValue b) -> BooleanValue (a == b)
            (FunctionValue args1 stmts1, FunctionValue args2 stmts2) ->
                BooleanValue $ (args1 == args2) && (stmts1 == stmts2)
            (NoValue,                     _) -> ErrorValue "NoValue cannot be evaluated."
            (_,                     NoValue) -> ErrorValue "NoValue cannot be evaluated."
            (_             ,              _) -> ErrorValue "Unhandled equality operation"

cast :: Value -> ValueType -> Value
cast (IntegerValue i) StringType  = StringValue (show i)
cast (BooleanValue b) StringType  = StringValue (show b)
cast val typ = ErrorValue $ unwords ["Casting value:", show val, "to", show typ, "is not currently supported"]

typeOfValue :: Value -> ValueType
typeOfValue (StringValue  _) = StringType
typeOfValue (IntegerValue _) = IntegerType
typeOfValue (BooleanValue _) = BooleanType
typeOfValue (FunctionValue _ _) = FunctionType
typeOfValue (_)              = error "Unknown type of value"
