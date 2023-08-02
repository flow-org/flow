module Main where

import qualified MyLib (someFunc)
-- import Runner (run)
import Parser
import Intermediate
import Interpreter (runGraph, toGraph, EvContext (EvContext))

main :: IO ()
main = do
  putStr "> "
  x <- getLine
  case parse x of
    Left a -> print "parse fail"
    Right a -> (case convert a of
      Left a -> print a
      Right (m, ic) -> do
        let graph = toGraph m
        result <- runGraph graph ic (EvContext [] 0)
        case result of
          Just _ -> return ()
          Nothing -> putStrLn "terminated")

test = do
  case parse "@a -> (1! -> merge) -> (1 -> +) -> trace -> @a" of
    Left a -> print "parse fail"
    Right a -> print a >> (case convert a of
      Left a -> print a
      Right (m, ic) -> do
        print (m, ic)
        let graph = toGraph m
        result <- runGraph graph ic (EvContext [] 0)
        case result of
          Just _ -> return ()
          Nothing -> putStrLn "terminated")

-- test file = do
--   result <- run file
--   case result of
--     Left error -> print error
--     Right result -> putStrLn result
