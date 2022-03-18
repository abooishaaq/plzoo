module Main where

import Control.Monad.Except (runExceptT, runExcept)
import Control.Monad.Identity (Identity)
import Control.Monad.State
import Control.Monad.Trans (MonadIO (liftIO))
import Parser (parseToplevel)
import System.Console.Haskeline (
    defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
 )

import Control.Applicative (Alternative (empty))
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)
import qualified Data.Map as Map
import Check (typeOf, TypeEnv)
import Eval (eval, Env, Thunk, printValue)
import Data.IORef (newIORef)
import Syntax


runTop :: [TopLevel] -> StateT (Env, TypeEnv) IO ()
runTop [] = return ()
runTop (t:ts) = do
    case t of
        Expr e -> do
            (env, tenv) <- get
            let res = runExcept $ typeOf tenv e
            case res of
                Left e -> liftIO $ print e
                Right t -> do
                    liftIO $ putStr ("- : " ++ show t ++ " = ")
                    lift $ eval env e >>= \x -> printValue x
        Def name e -> do
            (env, tenv) <- get
            let res = runExcept $ typeOf tenv e
            case res of
                Left e -> liftIO $ print e
                Right t -> do
                    v <- lift $ eval env e
                    th <- lift $ newIORef (\() -> return v)
                    put (Map.insert name th env, Map.insert name t tenv)
                    liftIO $ putStr $ name ++ " : " ++ show t ++ " = "
                    liftIO $ print v 
    runTop ts

runFile :: FilePath -> IO ()
runFile name = do
    file <- readFile name
    let res = parseToplevel file
    case res of
        Left err -> print err
        Right top -> do
            -- mapM_ pPrint top
            evalStateT (runTop top) (Map.empty, Map.empty)

process :: String -> IO ()
process line = do
    let res = parseToplevel line
    case res of
        Left err -> print err
        Right top -> do
            -- mapM_ pPrint top
            evalStateT (runTop top) (Map.empty, Map.empty)

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
