module Main where

import System.IO
import System.Environment
import Control.Monad.State
import Control.Monad.Error

import Expr
import Parser

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
  result <- prompt
  if pred result 
     then return ()
     else action result >> until_ pred prompt action

evalString x = putStrLn $ parseToString x

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "lisp> ") evalString

main = do args <- getArgs
          case length args of
            0 -> runRepl
            otherwise -> putStrLn "Sorry litteLisp is repl only for now."