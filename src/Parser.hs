{-# LANGUAGE RankNTypes #-}
module Parser where

import Types
import PEGParser
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.State (StateT, get)
import Debug.Trace

allowedLetter :: ParseFn Char u n [Char]
allowedLetter =
      letter
  <|> digit
  <|> (char '=' >> char '=' >> return "==")
  <|> char '+'
  <|> char '*'
  <|> char '/'
  <|> char '-'

var :: ParseFn Char u Exp Exp
var = useMemo "var" $ EVar <$> many1 allowedLetter

genMode :: ParseFn Char u n EGenMode
genMode = (char '*' >> return EGMAlways) <|> return EGMNormal

number :: ParseFn Char u Exp Exp
number = useMemo "number" $ do
  x <- many1 digit
  gm <- genMode
  return $ EImm (VNum (read x)) gm

str :: ParseFn Char u Exp Exp
str = useMemo "string" $ do
  char '"'
  x <- many ((do
    char '\\'
    c <- anyChar
    return ('\\' : c)) <|> charNot (char '\n' <|> char '\"'))
  char '"'
  gm <- genMode
  return $ EImm (VString (read ("\"" ++ x ++ "\""))) gm -- by using `read`, we can unescape the text

ref :: ParseFn Char u Exp Exp
ref = useMemo "ref" $ do
  char '@'
  x <- many1 (letter <|> digit)
  return $ ERef x

address :: ParseFn Char u Exp Exp
address = useMemo "address" $ do
  char '#'
  x <- many1 (letter <|> digit)
  return $ EAddress x

primitive :: ParseFn Char u Exp Exp
primitive = ref <|> address <|> number <|> str <|> var

middle :: ParseFn Char u Exp [Exp]
middle = useMemo "middle" (EMiddle <$> primitive) >>= (\head -> manySpaces >> (head :) <$> out)

arrow :: ParseFn Char u Exp ()
arrow = char '-' >> char '>' >> return ()
arrowLabelLeft = (do
  x <- many1 (letter <|> digit)
  char ':'
  return $ Just x) <|> return Nothing
arrowLabelRight = (do
  char ':'
  x <- many1 (letter <|> digit)
  return $ Just x) <|> return Nothing

inn :: ParseFn Char u Exp [Exp]
inn = (useMemo "inn" (do
  char '('
  manySpaces
  seq <- entry
  ps <- get
  -- many1Spaces
  label1 <- arrowLabelLeft
  arrow
  label2 <- arrowLabelRight
  manySpaces
  char ')'
  return $ EIn seq label1 label2) >>= (\head -> manySpaces >> (head :) <$> inn)) <|> middle

out :: ParseFn Char u Exp [Exp]
out = (useMemo "out" (inner <|> do
    char '('
    manySpaces
    x <- inner
    manySpaces
    char ')'
    return x) >>= (\head -> manySpaces >> (head :) <$> out)) <|> bi <|> return []
  where
    inner = do
      label1 <- arrowLabelLeft
      arrow
      label2 <- arrowLabelRight
      many1Spaces
      seq <- entry
      return $ EOut seq label1 label2

bi :: ParseFn Char u Exp [Exp]
bi = useMemo "bi" (do
  char '('
  manySpaces
  label1 <- arrowLabelLeft
  arrow
  label2 <- arrowLabelRight
  many1Spaces
  seq <- entry
  test <- psRest <$> get
  -- many1Spaces
  label3 <- arrowLabelLeft
  arrow
  label4 <- arrowLabelRight
  manySpaces
  char ')'
  return $ EBi seq label1 label2 label3 label4) >>= (\head -> manySpaces >> (head :) <$> (bi <|> inn))

entry :: ParseFn Char u Exp [Exp]
entry = inn

parseCommand = (do
  string "run"
  many1Spaces
  exps <- entry
  return $ CImRun exps)
  <|> (string "run" >> return CRun)
  <|> (string "exit" >> return CExit)
  <|> (do
    string "load"
    many1Spaces
    rest <- psRest <$> get
    return $ CLoad rest)
  <|> (CDecl <$> entry)

parseExp :: [Char] -> Either String [Exp]
parseExp text = case evalStateT (parseAll entry) (ParseState 0 initMemos text ()) of
  Left ps -> Left . parseError $ ps
  Right e -> Right e

parse :: [Char] -> Either String Command
parse text = case evalStateT (parseAll parseCommand) (ParseState 0 initMemos text ()) of
  Left ps -> Left . parseError $ ps
  Right e -> Right e

parseError :: ParseState Char () Exp -> String
parseError ps = "ParseError: failed to parse " ++ show (head $ psRest ps) ++ " at position " ++ show (psPos ps)
