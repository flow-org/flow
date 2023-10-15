{-# LANGUAGE RankNTypes #-}
module Parser where

import Types
import PEGParser
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.State (StateT, get)
import Debug.Trace
import Control.Applicative ((<|>))

withInfo :: ParseM Char u v Exp -> ParseM Char u v ExpWithInfo
withInfo p = do
  start <- pos
  e <- p
  end <- pos
  return (e, ExpInfo start end)

varName = do
  head <- lowerLetter
  tail <- many (letter <|> digit)
  last <- many (char '!')
  return (head ++ tail ++ last)

var :: ParseM Char u ExpWithInfo ExpWithInfo
var = useMemo "var" $ withInfo $ EVar <$> varName

ope :: ParseM Char u ExpWithInfo ExpWithInfo
ope = useMemo "ope" $ withInfo $ EVar <$> (
  (char '=' >> char '=' >> return "==")
  <|> char '+'
  <|> char '*'
  <|> char '/'
  <|> char '-')

number :: ParseM Char u ExpWithInfo ExpWithInfo
number = useMemo "number" $ withInfo $ do
  x <- many1 digit
  return $ EImm (VNum (read x))

str :: ParseM Char u ExpWithInfo ExpWithInfo
str = useMemo "string" $ withInfo $ do
  char '"'
  x <- many ((do
    char '\\'
    c <- anyChar
    return ('\\' : c)) <|> charNot (char '\n' <|> char '\"'))
  char '"'
  return $ EImm (VString (read ("\"" ++ x ++ "\""))) -- by using `read`, we can unescape the text

bool = useMemo "bool" $ withInfo $ (do
  string "True"
  return $ EImm (VBool True)) <|> (do
    string "False"
    return $ EImm (VBool False))

ref :: ParseM Char u ExpWithInfo ExpWithInfo
ref = useMemo "ref" $ withInfo (do
  char '@'
  name <- varName
  (do
    char '\''
    exp <- primitiveAtom
    return $ ERef name (Just exp)) <|> return (ERef name Nothing))

list :: ParseM Char u ExpWithInfo ExpWithInfo
list = useMemo "list" $ withInfo $ do
  char '['
  manySpaces
  head <- primitiveAtom
  let (EImm headV, _) = head
  manySpaces
  tail <- many (do
    char ','
    manySpaces
    x <- primitiveAtom
    let (EImm v, _) = x
    manySpaces
    return [v])
  char ']'
  return $ EImm (VList (headV : tail))

pair = useMemo "pair" $ withInfo $ do
  char '('
  manySpaces
  a <- primitiveAtom
  let (EImm va, _) = a
  manySpaces
  char ','
  manySpaces
  b <- primitiveAtom
  let (EImm vb, _) = b
  manySpaces
  char ')'
  return $ EImm (VPair va vb)

parens = do
  char '('
  p <- composed
  char ')'
  return p

primitiveAtom :: ParseM Char u ExpWithInfo ExpWithInfo
primitiveAtom = ref <|> number <|> str <|> bool <|> var <|> pair <|> parens <|> list

functionApply :: ParseM Char u ExpWithInfo ExpWithInfo
functionApply = withInfo $ do
  head <- var
  let (EVar name, _) = head
  tail <- many (do
    many1Spaces
    term <- primitiveAtom
    return [term])
  return $ EComposed name tail

infixlFactory ope next = do
  start <- pos
  head <- next
  tail <- many (do
    manySpaces
    ope <- ope
    manySpaces
    term <- next
    pos <- pos
    return [(ope, term, pos)])
  return $ foldl (\a (b, c, pos) -> (EComposed b [a, c], ExpInfo start pos)) head tail

infixInner = functionApply <|> primitiveAtom

infixl7Opes = char '*' <|> char '/' <|> char '%'
infixl7 :: ParseM Char u ExpWithInfo ExpWithInfo
infixl7 = useMemo "infixl7" $ infixlFactory infixl7Opes infixInner

infixl6Opes = char '+' <|> char '-'
infixl6 :: ParseM Char u ExpWithInfo ExpWithInfo
infixl6 = useMemo "infixl6" $ infixlFactory infixl6Opes infixl7

infixl5Opes = string "++"
infixl5 :: ParseM Char u ExpWithInfo ExpWithInfo
infixl5 = useMemo "infixl5" $ infixlFactory infixl5Opes infixl6

infixl4Opes = string "==" <|> string "!=" <|> string ">=" <|> string "<="
infixl4 :: ParseM Char u ExpWithInfo ExpWithInfo
infixl4 = useMemo "infixl4" $ infixlFactory infixl4Opes infixl5

infixl3Opes = string "&&"
infixl3 :: ParseM Char u ExpWithInfo ExpWithInfo
infixl3 = useMemo "infixl3" $ infixlFactory infixl3Opes infixl4

infixl2Opes = string "||"
infixl2 :: ParseM Char u ExpWithInfo ExpWithInfo
infixl2 = useMemo "infixl2" $ infixlFactory infixl2Opes infixl3

composed = infixl2

rightSecFactory ope next = withInfo (do
  ope <- ope
  manySpaces
  inner <- next
  return $ ESectionRightHand ope inner) <|> (do
    char '('
    manySpaces
    x <- rightSecFactory ope next
    manySpaces
    char ')'
    return x)
leftSecFactory ope next = withInfo $ do
  char '('
  inner <- next
  manySpaces
  ope <- ope
  char ')'
  return $ ESectionLeftHand ope inner

rightSec =
      rightSecFactory infixl7Opes infixInner
  <|> rightSecFactory infixl6Opes infixl7
  <|> rightSecFactory infixl5Opes infixl6
  <|> rightSecFactory infixl4Opes infixl5
leftSec =
      leftSecFactory infixl7Opes infixInner
  <|> leftSecFactory infixl6Opes infixl7
  <|> leftSecFactory infixl5Opes infixl6
  <|> leftSecFactory infixl4Opes infixl5

middle :: ParseM Char u ExpWithInfo [ExpWithInfo]
middle = withInfo (EMiddle <$> (composed <|> rightSec <|> leftSec <|> ope)) >>= (\head -> manySpaces >> (head :) <$> out)

arrowType :: ParseM Char u v ArrowType
arrowType =
      (string "->" >> return ANormal)
  <|> (string "~>" >> return AControl)
arrow :: ParseM Char u v Arrow
arrow = do
  label1 <- arrowLabelLeft
  arrowType <- arrowType
  label2 <- arrowLabelRight
  return $ Arrow arrowType label1 label2
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
  arrow <- arrow
  manySpaces
  char ')'
  return $ EIn seq arrow False) >>= (\head -> manySpaces >> (head :) <$> inn)) <|> middle

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
      arrow <- arrow
      many1Spaces
      seq <- entry
      return $ EOut seq arrow

bi :: ParseM Char u ExpWithInfo [ExpWithInfo]
bi = useMemo "bi" (withInfo $ do
  char '('
  manySpaces
  arrow1 <- arrow
  many1Spaces
  seq <- entry
  test <- psRest <$> pGet
  -- many1Spaces
  arrow2 <- arrow
  manySpaces
  char ')'
  return $ EBi seq arrow1 arrow2) >>= (\head -> manySpaces >> (head :) <$> (bi <|> inn))

entry :: ParseM Char u ExpWithInfo [ExpWithInfo]
entry = inn

cInArg = do
  char '('
  manySpaces
  char '@'
  name <- varName
  manySpaces
  string "->"
  manySpaces
  char ')'
  return name
cOutArg = do
  char '('
  manySpaces
  string "->"
  manySpaces
  char '@'
  name <- varName
  manySpaces
  char ')'
  return name

parseCommand =
      (string ":q" >> return CExit)
  <|> (do
    string ":l"
    many1Spaces
    rest <- many1 anyChar
    return $ CLoad rest)
  <|> (do
    name <- varName
    manySpaces
    ins <- many (do
      inn <- cInArg
      manySpaces
      return [inn])
    outs <- many (do
      out <- cOutArg
      manySpaces
      return [out])
    char '='
    manySpaces
    content <- entry
    return $ CName name ins outs [content])
  <|> (CDecl <$> entry)

pads = char '\n' <|> char '\r' <|> char ' ' <|> char '\t'
trimPads p = do
  many pads
  inner <- p
  many pads
  return inner

parseFile :: [Char] -> Either String [Command]
parseFile text = reverse <$> inner text where
  inner text = case runParseM (trimPads parseCommand) (ParseState 0 initMemos text () 0) of
    Left ps -> Left . parseError $ ps
    Right (c, ParseState { psRest = "" }) -> Right [c]
    Right (c, ParseState { psRest = rest }) -> inner rest >>= \cmds -> Right (c : cmds)

parseError :: ParseState Char () ExpWithInfo -> String
parseError ps =
  let maxPos = psMaxPos ps in
  replicate (maxPos + 2) ' ' ++ "^\n" ++
  "ParseError: failed to parse " ++ show  (psRest ps !! (maxPos - psPos ps)) ++ " at position " ++ show maxPos
