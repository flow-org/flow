module Main where

import qualified MyLib (someFunc)
import Runner (run)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc

test file = do
  result <- run file
  case result of
    Left error -> print error
    Right result -> putStrLn result
