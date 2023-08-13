{-# LANGUAGE RankNTypes #-}
module Parser where

import Types
import PEGParser
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.State (StateT, get)
import Debug.Trace
import Control.Applicative ((<|>))

allowedLetter :: ParseM Char u n [Char]
allowedLetter =
      letter
  <|> digit
  <|> (char '=' >> char '=' >> return "==")
  <|> char '+'
  <|> char '*'
  <|> char '/'
  <|> char '-'

withInfo :: ParseM Char u v Exp -> ParseM Char u v ExpWithInfo
withInfo p = do
  start <- pos
  e <- p
  end <- pos
  return (e, ExpInfo start end)

var :: ParseM Char u ExpWithInfo ExpWithInfo
var = useMemo "var" $ withInfo $ EVar <$> many1 allowedLetter

genMode :: ParseM Char u n EGenMode
genMode = (char '*' >> return EGMAlways) <|> return EGMNormal

number :: ParseM Char u ExpWithInfo ExpWithInfo
number = useMemo "number" $ withInfo $ do
  x <- many1 digit
  gm <- genMode
  return $ EImm (VNum (read x)) gm

str :: ParseM Char u ExpWithInfo ExpWithInfo
str = useMemo "string" $ withInfo $ do
  char '"'
  x <- many ((do
    char '\\'
    c <- anyChar
    return ('\\' : c)) <|> charNot (char '\n' <|> char '\"'))
  char '"'
  gm <- genMode
  return $ EImm (VString (read ("\"" ++ x ++ "\""))) gm -- by using `read`, we can unescape the text

ref :: ParseM Char u ExpWithInfo ExpWithInfo
ref = useMemo "ref" $ withInfo $ do
  char '@'
  x <- many1 (letter <|> digit)
  return $ ERef x

address :: ParseM Char u ExpWithInfo ExpWithInfo
address = useMemo "address" $ withInfo $ do
  char '#'
  x <- many1 (letter <|> digit)
  return $ EAddress x

primitive :: ParseM Char u ExpWithInfo ExpWithInfo
primitive = ref <|> address <|> number <|> str <|> var

middle :: ParseM Char u ExpWithInfo [ExpWithInfo]
middle = useMemo "middle" (withInfo $ EMiddle <$> primitive) >>= (\head -> manySpaces >> (head :) <$> out)

arrow :: ParseM Char u v ()
arrow = char '-' >> char '>' >> return ()
arrowLabelLeft = (do
  x <- many1 (letter <|> digit)
  char ':'
  return $ Just x) <|> return Nothing
arrowLabelRight = (do
  char ':'
  x <- many1 (letter <|> digit)
  return $ Just x) <|> return Nothing

inn :: ParseM Char u ExpWithInfo [ExpWithInfo]
inn = (useMemo "inn" (withInfo $ do
  char '('
  manySpaces
  seq <- entry
  ps <- pGet
  -- many1Spaces
  label1 <- arrowLabelLeft
  arrow
  label2 <- arrowLabelRight
  manySpaces
  char ')'
  return $ EIn seq label1 label2) >>= (\head -> manySpaces >> (head :) <$> inn)) <|> middle

out :: ParseM Char u ExpWithInfo [ExpWithInfo]
out = (useMemo "out" (withInfo (inner <|> do
    char '('
    manySpaces
    x <- inner
    manySpaces
    char ')'
    return x)) >>= (\head -> manySpaces >> (head :) <$> out)) <|> bi <|> return []
  where
    inner = do
      label1 <- arrowLabelLeft
      arrow
      label2 <- arrowLabelRight
      many1Spaces
      seq <- entry
      return $ EOut seq label1 label2

bi :: ParseM Char u ExpWithInfo [ExpWithInfo]
bi = useMemo "bi" (withInfo $ do
  char '('
  manySpaces
  label1 <- arrowLabelLeft
  arrow
  label2 <- arrowLabelRight
  many1Spaces
  seq <- entry
  test <- psRest <$> pGet
  -- many1Spaces
  label3 <- arrowLabelLeft
  arrow
  label4 <- arrowLabelRight
  manySpaces
  char ')'
  return $ EBi seq label1 label2 label3 label4) >>= (\head -> manySpaces >> (head :) <$> (bi <|> inn))

entry :: ParseM Char u ExpWithInfo [ExpWithInfo]
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
    rest <- psRest <$> pGet
    return $ CLoad rest)
  <|> (CDecl <$> entry)

parseExp :: [Char] -> Either String [ExpWithInfo]
parseExp text = case evalParseM (parseAll entry) (ParseState 0 initMemos text ()) of
  Left ps -> Left . parseError $ ps
  Right e -> Right e

parse :: [Char] -> Either String Command
parse text = case evalParseM (parseAll parseCommand) (ParseState 0 initMemos text ()) of
  Left ps -> Left . parseError $ ps
  Right e -> Right e

parseError :: ParseState Char () v -> String
parseError ps = "ParseError: failed to parse " ++ show (head $ psRest ps) ++ " at position " ++ show (psPos ps)
