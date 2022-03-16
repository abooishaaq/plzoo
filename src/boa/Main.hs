module Main where

import Control.Monad.Except (runExceptT)
import Control.Monad.Identity (Identity)
import Control.Monad.State
import Control.Monad.Trans (MonadIO (liftIO))
import Parser (parseToplevel)
import Syntax
import System.Console.Haskeline (
    defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
 )

import Control.Applicative (Alternative (empty))
import Eval (emptyEnv, evalTop)
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)

runFile :: FilePath -> IO ()
runFile name = do
    file <- readFile name
    let res = parseToplevel file
    case res of
        Left err -> print err
        Right top -> do
            -- mapM_ pPrint top
            evalStateT (evalTop top) emptyEnv

process :: String -> IO ()
process line = do
    let res = parseToplevel line
    case res of
        Left err -> print err
        Right top -> do
            -- mapM_ pPrint top
            evalStateT (evalTop top) emptyEnv

main :: IO ()
main = do
    args <- getArgs
    case args of
        [name] -> runFile name
        _ -> runInputT defaultSettings loop
  where
    loop = do
        minput <- getInputLine "λ> "
        case minput of
            Nothing -> outputStrLn "Goodbye."
            Just input -> liftIO (process input) >> loop
