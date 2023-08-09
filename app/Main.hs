module Main where

import Parser
import Syntax
import Intermediate
import Interpreter (runGraph, toGraph, EvContext (EvContext))

import Debug.Trace

import System.IO
import Control.Monad (forM)

main :: IO ()
main = loop []

loop expses = do
  hSetBuffering stdin LineBuffering
  putStr "> "
  x <- getLine
  hSetBuffering stdin NoBuffering
  if x == "" then loop expses
  else case parse x of
    Left a -> print ("parse fail: " ++ a)
    Right a -> (case a of
      CImRun exps -> run [exps] >> loop []
      CDecl newExps -> loop (newExps : expses)
      CRun -> run expses >> loop []
      CLoad fileName -> do
        content <- readFile fileName
        case forM (lines content) parseExp of
          Left a -> print ("parse fail: " ++ a)
          Right a -> run a >> loop []
      CExit -> return ())

run expses = case convertMultiline expses of
  Left a -> print a
  Right (m, ic) -> do
    let graph = toGraph m
    result <- runGraph graph ic
    case result of
      Just _ -> return ()
      Nothing -> putStrLn "terminated"

-- checks
-- let (Right x) = parse "1! -> if (then:-> 2 -> output) (else:-> 3 -> output)" in convert x
-- let (Right x) = parse "@a -> (1! ->) merge -> (1* ->) + -> trace -> @a" in convert x
-- let (Right x) = parse "@current -> (5! ->) == -> if (then:->:en (@result ->) control -> output) (else:-> copy (->:en (@result ->) control -> @next) (->:en (@current -> (1* ->) + ->) control -> @current))" in x
-- 1! -> copy (-> 2 ->) (-> 3 ->) + -> output
-- 1! -> copy (-> @a) ->:en (@a -> (@a ->) + ->) control -> output
-- 1! -> (@next ->) merge -> copy (-> @a) ->:en (@a -> (@a ->) + ->) control -> trace -> @next
-- check = 
--   let x =
--         (let (Right l1) = parse "@next -> (1! ->) merge -> (@current -> (1! ->) merge ->) * -> @result" in
--         let (Right l2) = parse "@current -> (5! ->) == -> if (then:->:en (@result ->) control -> output) (else:-> copy (->:en (@result ->) control -> @next) (->:en (@current -> (1* ->) + ->) control -> @current))" in
--           convertMultiline [l1, l2]) in x
