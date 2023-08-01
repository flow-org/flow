module Main where

import qualified MyLib (someFunc)
-- import Runner (run)
import Parser
import Intermediate
import Interpreter (runGraph, toGraph)

main :: IO ()
main = do
  case parse "1 -> + <- 2 -> output" of
    Left a -> print "parse fail"
    Right a -> print a >> (case convert a of
      Left a -> print a
      Right (m, ic) -> do
        print (m, ic)
        let graph = toGraph m
        runGraph graph ic [])

-- test file = do
--   result <- run file
--   case result of
--     Left error -> print error
--     Right result -> putStrLn result
