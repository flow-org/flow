module Main where

import qualified MyLib (someFunc)
-- import Runner (run)
import Parser
import Intermediate

main :: IO ()
main = do
  case parse "1 -> + <- 2 -> output" of
    Left a -> print a
    Right a -> (case convert a of
      Left a -> print a
      Right a -> print a)

-- test file = do
--   result <- run file
--   case result of
--     Left error -> print error
--     Right result -> putStrLn result
