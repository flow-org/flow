{-# LANGUAGE RankNTypes #-}
module Parser where

import Syntax
import PEGParser
import Control.Monad.State (evalStateT)

-- parens = do
--   char '('
--   exp <- eexp
--   char ')'
--   return exp

allowedLetter = letter <|> digit <|> char '+' <|> char '*' <|> char '/' <|> char '-'

-- var = EVar <$> ((++) <$> (return <$> letter) <*> many (letter <|> digit))
var = useMemo "var" $ EVar <$> many1 allowedLetter

genMode = (char '!' >> return GMOnce) <|> (char '*' >> return GMAlways) <|> return GMPassive

number = useMemo "number" $ do
  x <- many1 digit
  gm <- genMode
  return $ ENum (read x) gm

ref = useMemo "ref" $ do
  char '@'
  x <- many1 (letter <|> digit)
  return $ ERef x

address = useMemo "address" $ do
  char '#'
  x <- many1 (letter <|> digit)
  return $ EAddress x

parens = useMemo "parens" $ do
  char '('
  x <- middle
  char ')'
  return x

primitive = parens <|> ref <|> address <|> number <|> var

middle = useMemo "middle" $ (do
  x <- primitive
  outs <- manyOnes (do
    many1Spaces
    out)
  return $ EMiddle [] x outs) <|> do
  ins <- manyOnes (do
    x <- inn
    many1Spaces
    return x)
  mid <- middle
  outs <- manyOnes (do
    many1Spaces
    out)
  return $ EMiddle ins mid outs

arrow = char '-' >> char '>'
arrowLabelLeft = (do
  x <- many1 (letter <|> digit)
  char ':'
  return $ Just x) <|> return Nothing
arrowLabelRight = (do
  char ':'
  x <- many1 (letter <|> digit)
  return $ Just x) <|> return Nothing

inn = useMemo "inn" $ do
  char '('
  manySpaces
  mid <- middle
  many1Spaces
  label1 <- arrowLabelLeft
  arrow
  label2 <- arrowLabelRight
  manySpaces
  char ')'
  return $ EIn mid label1 label2

out = useMemo "out" $ (do
  label1 <- arrowLabelLeft
  arrow
  label2 <- arrowLabelRight
  many1Spaces
  mid <- middle
  return $ EOut mid label1 label2) <|> do
    char '('
    manySpaces
    x <- out
    manySpaces
    char ')'
    return x

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

mainParse = do
  x <- middle
  char '$'
  return x

parse text = case evalStateT middle (0, initMemos, text, ()) of
  Left _ -> Left "parse error"
  Right e -> Right e
