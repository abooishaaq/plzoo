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
import Text.Pretty.Simple ( pPrint )
import System.Environment ( getArgs )
import Infer (inferTop, emptyEnv)

runFile :: FilePath -> IO ()
runFile name = do
  file <- readFile name
  let res = parseToplevel file
  case res of
    Left err -> print err
    -- Right decls -> evalStateT (runDecls decls) global
    Right top -> do
      mapM_ pPrint top
      pPrint $ inferTop emptyEnv top

process :: String -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    -- Right decls -> evalStateT (runDecls decls) global 
    Right decls -> do
      mapM_ print decls
    
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
