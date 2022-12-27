module Main where

import Prelude hiding (lex)
import System.Environment
import System.Exit
import System.IO

import Lexer
import Parser
import Interpreter

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
        ["repl"] -> commandRepl
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
                                          "help :: Prints this message"]

commandCom :: String -> IO ()
commandCom f = do
    putStrLn "Compiling not implemented yet"

commandRun :: String -> IO ()
commandRun f = do
    case lex f of
        (tks, []) ->
            case parse tks of
                (stmts, []) -> interpret stmts
                (_   , err) -> putStrLn $ unlines err
        (_, err) -> putStrLn $ unlines err

commandRepl :: IO ()
commandRepl = do
    putStr "> "
    hFlush stdout
    s <- getLine
    case s of
        ":q" -> putStrLn "Exiting!" >> exitSuccess
        ""   -> putStr ""
        s    ->
            case lex s of
                (tks, []) ->
                    case parse tks of
                        (stmts, []) -> interpret stmts
                        (_   , err) ->
                            case expression tks of
                                (expr, _:[], []) -> print $ interpretExpr expr
                                (_, _:[], err) -> putStrLn $ unlines err
                                (_,    _,  _) -> putStrLn $ unlines err
                (_, err) -> putStrLn $ unlines err
    commandRepl

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
    print stmts
    putStrLn $ unlines err'
    interpret stmts

commandUnknown :: [String] -> IO ()
commandUnknown args = do
    putStr "Unknown command: "
    putStrLn $ head args
    commandHelp
    exitFailure
