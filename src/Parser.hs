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

number :: Parsec String u Exp
number = do
  x <- many1 digit
  return $ ENum $ read x

ref :: Parsec String u Exp
ref = do
  char '@'
  x <- many1 (letter <|> digit)
  return $ ERef x

primitive = parens <|> ref <|> number <|> var

arrow :: Parsec String u Arrow
arrow = (do
    char '<'
    char '-'
    return $ AToLeft Normal) <|> (do
    char '-'
    char '>'
    return $ AToRight Normal)

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
