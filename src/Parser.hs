module Parser where

import Node
import qualified Data.Map.Strict as Map
import Control.Monad.State
import qualified Data.Text as Text
import Data.Char

-- texts which cannot be used for symbols
reserved = ["<<", "-<"]

type Memos = Map.Map (Int, String) (Maybe (Int, Node))
findMemo :: Memos -> Int -> String -> Maybe (Maybe (Int, Node))
findMemo memos i label = Map.lookup (i, label) memos
addMemo :: Memos -> Int -> String -> Maybe (Int, Node) -> Memos
addMemo memos i label result = Map.insert (i, label) result memos

initMemos :: Memos
initMemos = Map.empty

type ParseState = (Int, Memos, Text.Text)
useMemo :: String
  -> StateT ParseState (Either Memos) Node
  -> StateT ParseState (Either Memos) Node
useMemo label p = StateT $ \(i, memos, xs) -> case findMemo memos i label of
  Just (Just (i', node)) -> Right (node, (i', memos, Text.drop (i' - i) xs))
  Just Nothing -> Left memos
  Nothing -> case runStateT p (i, memos, xs) of
    Left memos -> Left (addMemo memos i label Nothing)
    Right (a, (i', memos, xs)) -> Right (a, (i', addMemo memos i label (Just (i', a)), xs))

(<|>) :: StateT ParseState (Either Memos) t -> StateT ParseState (Either Memos) t -> StateT ParseState (Either Memos) t
(StateT a) <|> (StateT b) = StateT $ \(i, memos, xs) -> case a (i, memos, xs) of
    Left memos -> b (i, memos, xs)
    y -> y

forward :: Int -> StateT ParseState (Either Memos) ()
forward i' = do
  (i, memos, text) <- get
  put (i + i', memos, Text.drop i' text)

string :: String -> StateT ParseState (Either Memos) String
string s = do
  (_, memos, xs) <- get
  unless (Text.pack s `Text.isPrefixOf` xs) $ lift $ Left memos
  forward $ length s
  return s

guardText :: StateT ParseState (Either Memos) (Char, Text.Text)
guardText = do
  (i, memos, text) <- get
  when (Text.length text == 0) $ lift $ Left memos
  return (Text.head text, Text.tail text)

returnFail :: StateT ParseState (Either Memos) t
returnFail = do
  (_, memos, _) <- get
  lift $ Left memos

digit :: StateT ParseState (Either Memos) String
digit = do
  (x, _) <- guardText
  unless (isDigit x) returnFail
  forward 1
  return [x]

many :: StateT ParseState (Either Memos) [t] -> StateT ParseState (Either Memos) [t]
many p = StateT inner where
  inner s = case runStateT p s of
    Right (x', s') -> case runStateT (many p) s' of
      Right (x'', s'') -> Right (x' ++ x'', s'')
      Left memos -> let (i', _, xs') = s' in Right (x', (i', memos, xs'))
    Left memos -> let (i, _, xs) = s in Right ([], (i, memos, xs))

manyOnes :: StateT ParseState (Either Memos) t -> StateT ParseState (Either Memos) [t]
manyOnes p = StateT inner where
  inner s = case runStateT p s of
    Right (x', s') -> case runStateT (manyOnes p) s' of
      Right (x'', s'') -> Right (x':x'', s'')
      Left memos -> let (i', _, xs') = s' in Right ([x'], (i', memos, xs'))
    Left memos -> let (i, _, xs) = s in Right ([], (i, memos, xs))

oneOrMore :: StateT ParseState (Either Memos) [t] -> StateT ParseState (Either Memos) [t]
oneOrMore p = do
  x <- p
  xs <- many p
  return $ x ++ xs

anyChar :: StateT ParseState (Either Memos) String
anyChar = do
  (x, _) <- guardText
  forward 1
  return [x]

char :: Char -> StateT ParseState (Either Memos) String
char c = do
  (x, _) <- guardText
  unless (x == c) returnFail
  forward 1
  return [x]

charNot :: StateT ParseState (Either Memos) String -> StateT ParseState (Either Memos) String
charNot p = do
  (x, _) <- guardText
  parseState <- get
  case runStateT p parseState of
    Left memos -> do
      forward 1
      return [x]
    Right (_, (_, memos, _)) -> returnFail

optional p = StateT $ \s -> case runStateT p s of
  Left memos -> let (i, _, text) = s in Right ([], (i, memos, text))
  Right x -> Right x

commentTest = do
  char '#'
  many $ charNot $ char '\n'
  return []
lineTest = commentTest <|> do
  exp <- statementTest
  many blankTest
  optional commentTest
  return [exp]
blankTest = char ' ' <|> char '\t'
statementTest = connectorDefTest <|> connectionTest
stringTest :: StateT ParseState (Either Memos) Node
stringTest = do
  char '"'
  text <- many $ (do
    char '\\'
    anyChar) <|> charNot (char '\\' <|> char '"' <|> char '\n')
  char '"'
  return $ String text
numberTest = do
  text <- many digit
  return $ Number (read text :: Int)
booleanTest = (do
  string "true"
  return $ Boolean True) <|> do
    string "false"
    return $ Boolean False
symbolTest = do
  text <- oneOrMore $ charNot $ char ' ' <|> char '\n'
  when (text `elem` reserved) returnFail
  return $ Symbol text
parensTest = do
  char '('
  many blankTest
  exp <- valueTest
  many blankTest
  char ')'
  return exp
valueTest = useMemo "value" $ stringTest <|> numberTest <|> booleanTest <|> symbolTest <|> parensTest
applyBaseTest = do
  head <- valueTest
  tail <- manyOnes $ do
    oneOrMore blankTest
    valueTest
  return $ ApplyBase $ head:tail
expTest = useMemo "exp" $ valueTest <|> applyBaseTest

connectionTest = useMemo "connection" $ do
  head <- expTest
  tail <- manyOnes $ do
    oneOrMore blankTest
    string "<<"
    oneOrMore blankTest
    expTest
  return $ Connection $ head:tail

connectorDefTest = useMemo "connectorDef" $ do
  connector <- symbolTest
  oneOrMore blankTest
  string "-<"
  oneOrMore blankTest
  ConnectorDef connector <$> connectionTest

-- connectorDefTest = do