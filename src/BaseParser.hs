module BaseParser where

import Node
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Data.Char
import Data.List


type Memos t = Map.Map (Int, [t]) (Maybe (Int, Node))
findMemo :: (Ord t) => Memos t -> Int -> [t] -> Maybe (Maybe (Int, Node))
findMemo memos i label = Map.lookup (i, label) memos
addMemo :: (Ord t) => Memos t -> Int -> [t] -> Maybe (Int, Node) -> Memos t
addMemo memos i label result = Map.insert (i, label) result memos

initMemos :: Memos t
initMemos = Map.empty

type ParseState t = (Int, Memos t, [t])
useMemo :: (Ord t) => [t]
  -> StateT (ParseState t) (Either (Memos t)) Node
  -> StateT (ParseState t) (Either (Memos t)) Node
useMemo label p = StateT $ \(i, memos, xs) -> case findMemo memos i label of
  Just (Just (i', node)) -> Right (node, (i', memos, drop (i' - i) xs))
  Just Nothing -> Left memos
  Nothing -> case runStateT p (i, memos, xs) of
    Left memos -> Left (addMemo memos i label Nothing)
    Right (a, (i', memos, xs)) -> Right (a, (i', addMemo memos i label (Just (i', a)), xs))

(StateT a) <|> (StateT b) = StateT $ \(i, memos, xs) -> case a (i, memos, xs) of
    Left memos -> b (i, memos, xs)
    y -> y

forward :: Int -> StateT (ParseState t) (Either (Memos t)) ()
forward i' = do
  (i, memos, text) <- get
  put (i + i', memos, drop i' text)

string s = do
  (_, memos, xs) <- get
  unless (s `isPrefixOf` xs) $ lift $ Left memos
  forward $ length s
  return s

guardText :: StateT (ParseState t) (Either (Memos t)) (t, [t])
guardText = do
  (i, memos, text) <- get
  when (null text) $ lift $ Left memos
  return (head text, tail text)

returnFail :: StateT (ParseState t) (Either (Memos t)) u
returnFail = do
  (_, memos, _) <- get
  lift $ Left memos

digit = do
  (x, _) <- guardText
  unless (isDigit x) returnFail
  forward 1
  return [x]

many p = StateT inner where
  inner s = case runStateT p s of
    Right (x', s') -> case runStateT (many p) s' of
      Right (x'', s'') -> Right (x' ++ x'', s'')
      Left memos -> let (i', _, xs') = s' in Right (x', (i', memos, xs'))
    Left memos -> let (i, _, xs) = s in Right ([], (i, memos, xs))

manyOnes p = StateT inner where
  inner s = case runStateT p s of
    Right (x', s') -> case runStateT (manyOnes p) s' of
      Right (x'', s'') -> Right (x':x'', s'')
      Left memos -> let (i', _, xs') = s' in Right ([x'], (i', memos, xs'))
    Left memos -> let (i, _, xs) = s in Right ([], (i, memos, xs))

oneOrMore p = do
  x <- p
  xs <- many p
  return $ x ++ xs

anyChar = do
  (x, _) <- guardText
  forward 1
  return [x]

char c = do
  (x, _) <- guardText
  unless (x == c) returnFail
  forward 1
  return [x]

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