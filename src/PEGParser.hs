module PEGParser where

import qualified Data.Map.Strict as Map
import Control.Monad.State
import Data.Char
import Data.List


type Memos t n = Map.Map (Int, [t]) (Maybe (Int, n))
findMemo :: (Ord t) => Memos t n -> Int -> [t] -> Maybe (Maybe (Int, n))
findMemo memos i label = Map.lookup (i, label) memos
addMemo :: (Ord t) => Memos t n -> Int -> [t] -> Maybe (Int, n) -> Memos t n
addMemo memos i label result = Map.insert (i, label) result memos

initMemos :: Memos t n
initMemos = Map.empty

type ParseState t u n = (Int, Memos t n, [t], u)
useMemo :: (Ord t) => [t]
  -> StateT (ParseState t u n) (Either (Memos t n)) n
  -> StateT (ParseState t u n) (Either (Memos t n)) n
useMemo label p = StateT $ \(i, memos, xs, extra) -> case findMemo memos i label of
  Just (Just (i', node)) -> Right (node, (i', memos, drop (i' - i) xs, extra))
  Just Nothing -> Left memos
  Nothing -> case runStateT p (i, memos, xs, extra) of
    Left memos -> Left (addMemo memos i label Nothing)
    Right (a, (i', memos, xs, extra)) -> Right (a, (i', addMemo memos i label (Just (i', a)), xs, extra))

  -- todo: make this an instance of Alternative
(StateT a) <|> (StateT b) = StateT $ \(i, memos, xs, extra) -> case a (i, memos, xs, extra) of
    Left memos -> b (i, memos, xs, extra)
    y -> y

forward :: Int -> StateT (ParseState t u n) (Either (Memos t n)) ()
forward i' = do
  (i, memos, text, extra) <- get
  put (i + i', memos, drop i' text, extra)

string s = do
  (_, memos, xs, _) <- get
  unless (s `isPrefixOf` xs) $ lift $ Left memos
  forward $ length s
  return s

guardText :: StateT (ParseState t u n) (Either (Memos t n)) (t, [t])
guardText = do
  (i, memos, text, _) <- get
  when (null text) $ lift $ Left memos
  return (head text, tail text)

returnFail :: StateT (ParseState t u n) (Either (Memos t n)) v
returnFail = do
  (_, memos, _, _) <- get
  lift $ Left memos

many :: StateT (ParseState t u n) (Either (Memos t n)) [a] -> StateT (ParseState t u n) (Either (Memos t n)) [a]
many p = StateT inner where
  inner s = case runStateT p s of
    Right (x', s') -> case runStateT (many p) s' of
      Right (x'', s'') -> Right (x' ++ x'', s'')
      Left memos -> let (i', _, xs', extra) = s' in Right (x', (i', memos, xs', extra))
    Left memos -> let (i, _, xs, extra) = s in Right ([], (i, memos, xs, extra))

manyOnes :: StateT (ParseState t u n) (Either (Memos t n)) a -> StateT (ParseState t u n) (Either (Memos t n)) [a]
manyOnes p = StateT inner where
  inner s = case runStateT p s of
    Right (x', s') -> case runStateT (manyOnes p) s' of
      Right (x'', s'') -> Right (x':x'', s'')
      Left memos -> let (i', _, xs', extra) = s' in Right ([x'], (i', memos, xs', extra))
    Left memos -> let (i, _, xs, extra) = s in Right ([], (i, memos, xs, extra))

many1 p = do
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
    Right (_, (_, memos, _, _)) -> returnFail

letter = do
  (x, _) <- guardText
  unless (isLetter x) returnFail
  forward 1
  return [x]
digit = do
  (x, _) <- guardText
  unless (isDigit x) returnFail
  forward 1
  return [x]

many1Spaces = many1 (char ' ')
manySpaces = many (char ' ')

optional p = StateT $ \s -> case runStateT p s of
  Left memos -> let (i, _, text, extra) = s in Right ([], (i, memos, text, extra))
  Right x -> Right x
