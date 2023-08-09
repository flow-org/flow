module Main where

import Parser
import Syntax
import Intermediate
import Interpreter (runGraph, toGraph, EvContext (EvContext))

import Debug.Trace

import System.IO

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
      CExit -> return ())

run expses = case convertMultiline expses of
  Left a -> print a
  Right (m, ic) -> do
    trace (show m) return ()
    let graph = toGraph m
    result <- runGraph graph ic (EvContext [] 0)
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

-- test = do
--   case parse "0! -> if then:-> 2 else:-> 3 -> merge -> output" of
--   -- case parse "@a -> (0! -> merge) -> (1* -> +) -> trace -> @a" of
--   -- case parse "1* -> 2* -> + -> output" of
--     Left a -> print "parse fail"
--     Right a -> print a >> (case convert a of
--       Left a -> print a
--       Right (m, ic) -> do
--         print (m, ic)
--         let graph = toGraph m
--         result <- runGraph graph ic (EvContext [] 0)
--         case result of
--           Just _ -> return ()
--           Nothing -> putStrLn "terminated")

-- test file = do
--   result <- run file
--   case result of
--     Left error -> print error
--     Right result -> putStrLn result
