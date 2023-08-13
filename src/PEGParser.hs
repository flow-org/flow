module PEGParser where

import qualified Data.Map.Strict as Map
import Control.Monad.State
import Data.Char
import Data.List


type MemoItem t u n = Either (ParseState t u n) (Int, n)
type Memos t u n = Map.Map (Int, [t]) (MemoItem t u n)
findMemo :: (Ord t) => Memos t u n -> Int -> [t] -> Maybe (MemoItem t u n)
findMemo memos i label = Map.lookup (i, label) memos
addMemo :: (Ord t) => Memos t u n -> Int -> [t] -> MemoItem t u n -> Memos t u n
addMemo memos i label result = Map.insert (i, label) result memos

initMemos :: Memos t u n
initMemos = Map.empty

data ParseState t u n = ParseState { psPos :: Int, psMemos :: Memos t u n, psRest :: [t], psExtra :: u }
useMemo :: (Ord t) => [t] -> ParseFn t u n n -> ParseFn t u n n
useMemo label p = StateT $ \(ParseState i memos xs extra) -> case findMemo memos i label of
  Just (Right (i', node)) -> Right (node, ParseState i' memos (drop (i' - i) xs) extra)
  Just (Left ps) -> Left ps
  Nothing -> case runStateT p (ParseState i memos xs extra) of
    Left ps -> Left $ ps { psMemos = addMemo (psMemos ps) i label (Left ps) }
    Right (a, ps) -> Right (a, ps { psMemos = addMemo (psMemos ps) i label (Right (psPos ps, a)) })

  -- todo: make this an instance of Alternative
(<|>) :: ParseFn t u n a -> ParseFn t u n a -> ParseFn t u n a
(StateT a) <|> (StateT b) = StateT $ \ps -> case a ps of
    Left (ParseState { psMemos = memos }) -> b (ps { psMemos = memos })
    y -> y

type ParseFn t u n r = StateT (ParseState t u n) (Either (ParseState t u n)) r
forward :: Int -> ParseFn t u n ()
forward i' = do
  ps <- get
  put $ ps { psPos = psPos ps + i', psRest = drop i' (psRest ps) }

string :: Eq a => [a] -> ParseFn a u n [a]
string s = do
  ps <- get
  unless (s `isPrefixOf` psRest ps) returnFail
  forward $ length s
  return s

guardText :: ParseFn t u n (t, [t])
guardText = do
  ps <- get
  when (null $ psRest ps) returnFail
  let rest = psRest ps
  return (head rest, tail rest)

returnFail :: ParseFn t u n v
returnFail = do
  ps <- get
  lift $ Left ps

many :: ParseFn t u n [a] -> ParseFn t u n [a]
many p = StateT inner where
  inner s = case runStateT p s of
    Right (x', s') -> case runStateT (many p) s' of
      Right (x'', s'') -> Right (x' ++ x'', s'')
      Left ps -> Right (x', s' { psMemos = psMemos ps })
    Left ps -> Right ([], s { psMemos = psMemos ps })

many1 :: ParseFn t u n [a] -> ParseFn t u n [a]
many1 p = do
  x <- p
  xs <- many p
  return $ x ++ xs

anyChar :: ParseFn a u n [a]
anyChar = do
  (x, _) <- guardText
  forward 1
  return [x]

char :: Eq a => a -> ParseFn a u n [a]
char c = do
  (x, _) <- guardText
  unless (x == c) returnFail
  forward 1
  return [x]

charNot :: ParseFn a u n [a] -> ParseFn a u n [a]
charNot p = do
  (x, _) <- guardText
  parseState <- get
  case runStateT p parseState of
    Left memos -> do
      forward 1
      return [x]
    Right _ -> returnFail

letter :: ParseFn Char u n [Char]
letter = do
  (x, _) <- guardText
  unless (isLetter x) returnFail
  forward 1
  return [x]
digit :: ParseFn Char u n [Char]
digit = do
  (x, _) <- guardText
  unless (isDigit x) returnFail
  forward 1
  return [x]

many1Spaces :: ParseFn Char u n [Char]
many1Spaces = many1 (char ' ')
manySpaces :: ParseFn Char u n [Char]
manySpaces = many (char ' ')

parseAll :: ParseFn t u n a -> ParseFn t u n a
parseAll p = do
  e <- p
  ps <- get
  if null $ psRest ps then return e else returnFail
