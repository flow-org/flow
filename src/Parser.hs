{-# LANGUAGE RankNTypes #-}
module Parser where

import Syntax
import PEGParser
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.State (StateT, get)
import Debug.Trace

-- parens = do
--   char '('
--   exp <- eexp
--   char ')'
--   return exp

allowedLetter :: StateT (ParseState Char u n) (Either (Memos Char n)) [Char]
allowedLetter = letter <|> digit <|> char '+' <|> char '*' <|> char '/' <|> char '-'

-- var = EVar <$> ((++) <$> (return <$> letter) <*> many (letter <|> digit))
var :: StateT (ParseState Char u Exp) (Either (Memos Char Exp)) Exp
var = useMemo "var" $ EVar <$> many1 allowedLetter

genMode :: StateT (ParseState Char u n) (Either (Memos Char n)) GenMode
genMode = (char '!' >> return GMOnce) <|> (char '*' >> return GMAlways) <|> return GMPassive

number :: StateT (ParseState Char u Exp) (Either (Memos Char Exp)) Exp
number = useMemo "number" $ do
  x <- many1 digit
  gm <- genMode
  return $ ENum (read x) gm

ref :: StateT (ParseState Char u Exp) (Either (Memos Char Exp)) Exp
ref = useMemo "ref" $ do
  char '@'
  x <- many1 (letter <|> digit)
  return $ ERef x
address :: StateT (ParseState Char u Exp) (Either (Memos Char Exp)) Exp

address = useMemo "address" $ do
  char '#'
  x <- many1 (letter <|> digit)
  return $ EAddress x

primitive :: StateT (ParseState Char u Exp) (Either (Memos Char Exp)) Exp
primitive = ref <|> address <|> number <|> var

middle :: StateT (ParseState Char u Exp) (Either (Memos Char Exp)) [Exp]
middle = useMemo "middle" (EMiddle <$> primitive) >>= (\head -> manySpaces >> (head :) <$> out)

arrow :: StateT (ParseState Char u n) (Either (Memos Char n)) [Char]
arrow = char '-' >> char '>'
arrowLabelLeft = (do
  x <- many1 (letter <|> digit)
  char ':'
  return $ Just x) <|> return Nothing
arrowLabelRight = (do
  char ':'
  x <- many1 (letter <|> digit)
  return $ Just x) <|> return Nothing

inn :: StateT (ParseState Char d Exp) (Either (Memos Char Exp)) [Exp]
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

out :: StateT (ParseState Char u Exp) (Either (Memos Char Exp)) [Exp]
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

bi :: StateT (ParseState Char u Exp) (Either (Memos Char Exp)) [Exp]
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

entry :: StateT (ParseState Char d Exp) (Either (Memos Char Exp)) [Exp]
entry = inn

-- arrow = do
--   label1 <- arrowLabelLeft
--   (do
--     char '<'
--     char '-'
--     label2 <- arrowLabelRight
--     return $ AToLeft Normal label2 label1) <|> (do
--     char '-'
--     char '>'
--     label2 <- arrowLabelRight
--     return $ AToRight Normal label1 label2)

-- eexp = do
--   head <- primitive
--   tail <- many (do
--     spaces
--     a <- arrow
--     spaces
--     item <- primitive
--     return (a, item)) <|> return []
--   case tail of
--     [] -> return head
--     _ -> return $ foldl (\left (arrow, right) -> EConnect arrow left right) head tail

-- main = do
--   parseTest eexp "a -> b <- c <- d"
--   parseTest eexp "input -> (1 -> +) -> output"
--   parseTest eexp "1 -> + <- 2 -> output"
--   parseTest eexp "@a <- trace <- + <- 1 <- @a"

parse text = case evalStateT entry (ParseState 0 initMemos text ()) of
  Left _ -> Left "parse error"
  Right e -> Right e
