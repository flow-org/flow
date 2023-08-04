{-# LANGUAGE RankNTypes #-}
module Parser where

import Text.Parsec
import Text.Parsec.Char
import Syntax

parens :: Parsec String u Exp
parens = do
  char '('
  exp <- eexp
  char ')'
  return exp

allowedLetter :: Parsec String u Char
allowedLetter = letter <|> digit <|> char '+' <|> char '*' <|> char '/' <|> char '-'

var :: Parsec String u Exp
-- var = EVar <$> ((++) <$> (return <$> letter) <*> many (letter <|> digit))
var = EVar <$> many1 allowedLetter

genMode :: Parsec String u GenMode
genMode = (char '!' >> return GMOnce) <|> (char '*' >> return GMAlways) <|> return GMPassive

number :: Parsec String u Exp
number = do
  x <- many1 digit
  gm <- genMode
  return $ ENum (read x) gm

ref :: Parsec String u Exp
ref = do
  char '@'
  x <- many1 (letter <|> digit)
  return $ ERef x

address :: Parsec String u Exp
address = do
  char '#'
  x <- many1 (letter <|> digit)
  return $ EAddress x

primitive = parens <|> ref <|> address <|> number <|> var

arrowLabelLeft :: Parsec String u (Maybe String)
arrowLabelLeft = (do
  x <- many1 (letter <|> digit)
  char ':'
  return $ Just x) <|> return Nothing

arrowLabelRight :: Parsec String u (Maybe String)
arrowLabelRight = (do
  char ':'
  x <- many1 (letter <|> digit)
  return $ Just x) <|> return Nothing

arrow :: Parsec String u Arrow
arrow = do
  label1 <- arrowLabelLeft
  (do
    char '<'
    char '-'
    label2 <- arrowLabelRight
    return $ AToLeft Normal label2 label1) <|> (do
    char '-'
    char '>'
    label2 <- arrowLabelRight
    return $ AToRight Normal label1 label2)

eexp = do
  head <- primitive
  tail <- many (do
    spaces
    a <- arrow
    spaces
    item <- primitive
    return (a, item)) <|> return []
  case tail of
    [] -> return head
    _ -> return $ foldl (\left (arrow, right) -> EConnect arrow left right) head tail

-- main = do
--   parseTest eexp "a -> b <- c <- d"
--   parseTest eexp "input -> (1 -> +) -> output"
--   parseTest eexp "1 -> + <- 2 -> output"
--   parseTest eexp "@a <- trace <- + <- 1 <- @a"

parse = runParser eexp () "test"
