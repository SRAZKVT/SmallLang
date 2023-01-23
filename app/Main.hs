module Main where

import Prelude hiding (lex)
import System.Environment
import System.Exit
import System.IO

import Lexer
import Parser
import Interpreter

version :: String
version = "SmallLang 0.1.0.0"

main :: IO ()
main = do
    args <- getArgs
    case args of
        []      -> do
              putStrLn "Please input a command"
              commandHelp
              exitFailure
        ["ver"] -> putStrLn version
        ["help"] -> commandHelp
        ["repl"] -> commandRepl newEnv
        [_]     -> do
              putStrLn "Please input a file on top of a command"
              commandHelp
              exitFailure
        _       -> do
            f <- readFile (args !! 1)
            case head args of
                "com"     -> commandCom f
                "run"     -> commandRun f
                "dev"     -> commandDev f
                "ast"     -> commandAst f
                _         -> commandUnknown args

commandHelp :: IO ()
commandHelp = do
    argv0 <- getProgName
    putStrLn $ unwords ["Usage ::", argv0, "[COMMAND] file"]
    putStrLn "Commands:"
    putStr   $ unlines $ ("    " ++) <$> ["com  :: Compiles the file",
                                          "run  :: Runs the file",
                                          "repl :: Runs a REPL",
                                          "ver  :: Prints the version of the compiler",
                                          "ast  :: Prints out the AST of the program",
                                          "help :: Prints this message"]

commandCom :: String -> IO ()
commandCom _ = do
    putStrLn "Compiling not implemented yet"

commandRun :: String -> IO ()
commandRun f = do
    case lex f of
        (tks, []) ->
            case parse tks of
                (stmts, []) ->
                    let (Environment {ioOps = io}, success, _) = interpret stmts newEnv
                    in io >> if success then exitSuccess else exitFailure
                (_   , err) -> putStrLn $ unlines err
        (_, err) -> putStrLn $ unlines err

commandRepl :: Environment -> IO ()
commandRepl env = do
    putStr "> "
    hFlush stdout
    s <- getLine
    case s of
        ":q" -> putStrLn "Exiting!" >> exitSuccess
        ""   -> putStr ""
        content    ->
            case lex content of
                (tks, []) ->
                    case parse tks of
                        (stmts, []) ->
                            let (envi, _, _) = interpret stmts env
                            in ioOps env >> commandRepl envi
                        (_   , err) ->
                            case expression tks  of
                                (expr, _:[], []) ->
                                    let (val, envi) = interpretExpr expr env
                                    in ioOps envi >> print val >> commandRepl envi { ioOps = putStr "" }
                                (_, _:[], err') -> putStrLn $ unlines err'
                                (_,    _,  _) -> putStrLn $ unlines err
                (_, err) -> putStrLn $ unlines err
    commandRepl env

commandAst :: String -> IO ()
commandAst f = do
    let (tks, err) = lex f
    if not $ null err
        then putStrLn $ unlines err
        else do
            let (stmts, err') = parse tks
            if not $ null err'
                then putStrLn $ unlines err
                else putStrLn $ unlines $ map show stmts

commandDev :: String -> IO ()
commandDev f = do
    putStrLn "Content:"
    putStrLn f
    let (tks, err) = lex f
    putStrLn "Tokens:"
    print $ map tokenType tks
    print err
    let (stmts, err') = parse tks
    putStrLn "AST:"
    putStrLn $ unlines $ map show stmts
    putStrLn $ unlines err'
    let (envi, _, _) = interpret stmts newEnv
    ioOps envi
    print envi


commandUnknown :: [String] -> IO ()
commandUnknown args = do
    putStr "Unknown command: "
    putStrLn $ head args
    commandHelp
    exitFailure
