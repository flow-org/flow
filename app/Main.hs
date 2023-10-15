module Main where

import Parser
import Types
import Intermediate
import Interpreter (runEnv, toGraph)
import SyntaxSugar (handleSugarRoot)

import Debug.Trace

import System.IO
import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import Data.List (intercalate)
import GHC.GHCi.Helpers (flushAll)

main :: IO ()
main = putStrLn "flow ver 0.1.0" >> loop Map.empty

appendRoot :: Expses -> Env -> Env
appendRoot root = Map.insert "root" ([], [], root)

readLine = do
  -- hSetBuffering stdin LineBuffering
  putStr "> "
  flushAll
  x <- getLine
  -- hSetBuffering stdin NoBuffering
  return x

readLines :: IO String
readLines = intercalate "\n" . reverse <$> inner where
  inner :: IO [String]
  inner = do
    l <- readLine
    if l == ":}" then return []
    else do
      rest <- inner
      return (l : rest)

loop :: Env -> IO ()
loop env = do
  x <- readLine
  case x of
    "" -> loop env
    ":{" -> do
      text <- readLines
      runParse env text
    _ -> runParse env x

runParse env text = case parseFile text of
  Left parseError -> putStrLn parseError
  Right a -> runParseInner env [] text a

runParseInner :: Env -> Expses -> String -> [Command] -> IO ()
runParseInner env [] text [] = loop env
runParseInner env root text [] = run (appendRoot root env) >> loop env
runParseInner env root text (x:xs) = case x of
  CDecl exps -> case handleSugarRoot exps of
    Right exps -> runParseInner env ((text, exps) : root) text xs
    Left err -> putStrLn err
  CName fnName ins outs content -> case mapM handleSugarRoot content of
    Right expses -> runParseInner (Map.insert fnName (ins, outs, zip (repeat text) expses) env) root text xs
    Left err -> putStrLn err
  CLoad fileName -> do
    content <- readFile fileName
    runParse Map.empty content
  CExit -> return ()

run :: Env -> IO ()
run env = case convertEnv env of
  Left a -> putStrLn a
  Right ienv -> do
    result <- runEnv ienv
    case result of
      Just _ -> return ()
      Nothing -> putStrLn "terminated"
