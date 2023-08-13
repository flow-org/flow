{-# LANGUAGE DeriveFunctor #-}
module PEGParser where

import qualified Data.Map.Strict as Map
import Control.Monad.State
import Data.Char
import Data.List
import Control.Applicative (Alternative)
import GHC.Base (empty, (<|>))

newtype ParseM t u n r = ParseM (StateT (ParseState t u n) (Either (ParseState t u n)) r) deriving Functor
runParseM :: ParseM t u n a -> ParseState t u n -> Either (ParseState t u n) (a, ParseState t u n)
runParseM (ParseM s) = runStateT s
evalParseM :: ParseM t u n a -> ParseState t u n -> Either (ParseState t u n) a
evalParseM (ParseM s) = evalStateT s
pGet :: ParseM t u n (ParseState t u n)
pGet = ParseM get
pPut :: ParseState t u n -> ParseM t u n ()
pPut a = ParseM $ put a

instance Applicative (ParseM t u n) where
  pure = ParseM . pure
  (ParseM f) <*> (ParseM a) = ParseM $ f <*> a

instance Monad (ParseM t u n) where
  (ParseM a) >>= f = ParseM $ a >>= (\inner -> let (ParseM r) = f inner in r)

instance Alternative (ParseM t u n) where
  -- todo: actually, this is not a unit.
  empty = ParseM (get >>= lift . Left)
  (ParseM (StateT a)) <|> (ParseM (StateT b)) = ParseM $ StateT $ \ps -> case a ps of
      Left (ParseState { psMemos = memos }) -> b (ps { psMemos = memos })
      y -> y

type MemoItem t u n = Either (ParseState t u n) (Int, n)
type Memos t u n = Map.Map (Int, [t]) (MemoItem t u n)
findMemo :: (Ord t) => Memos t u n -> Int -> [t] -> Maybe (MemoItem t u n)
findMemo memos i label = Map.lookup (i, label) memos
addMemo :: (Ord t) => Memos t u n -> Int -> [t] -> MemoItem t u n -> Memos t u n
addMemo memos i label result = Map.insert (i, label) result memos

initMemos :: Memos t u n
initMemos = Map.empty

data ParseState t u n = ParseState { psPos :: Int, psMemos :: Memos t u n, psRest :: [t], psExtra :: u }
useMemo :: (Ord t) => [t] -> ParseM t u n n -> ParseM t u n n
useMemo label p = ParseM $ StateT $ \(ParseState i memos xs extra) -> case findMemo memos i label of
  Just (Right (i', node)) -> Right (node, ParseState i' memos (drop (i' - i) xs) extra)
  Just (Left ps) -> Left ps
  Nothing -> case runParseM p (ParseState i memos xs extra) of
    Left ps -> Left $ ps { psMemos = addMemo (psMemos ps) i label (Left ps) }
    Right (a, ps) -> Right (a, ps { psMemos = addMemo (psMemos ps) i label (Right (psPos ps, a)) })

forward :: Int -> ParseM t u n ()
forward i' = do
  ps <- pGet
  pPut $ ps { psPos = psPos ps + i', psRest = drop i' (psRest ps) }

string :: Eq a => [a] -> ParseM a u n [a]
string s = do
  ps <- pGet
  guard (s `isPrefixOf` psRest ps)
  forward $ length s
  return s

guardText :: ParseM t u n (t, [t])
guardText = do
  ps <- pGet
  guard (not $ null $ psRest ps)
  let rest = psRest ps
  return (head rest, tail rest)

many :: ParseM t u n [a] -> ParseM t u n [a]
many p = ParseM $ StateT inner where
  inner s = case runParseM p s of
    Right (x', s') -> case runParseM (many p) s' of
      Right (x'', s'') -> Right (x' ++ x'', s'')
      Left ps -> Right (x', s' { psMemos = psMemos ps })
    Left ps -> Right ([], s { psMemos = psMemos ps })

many1 :: ParseM t u n [a] -> ParseM t u n [a]
many1 p = do
  x <- p
  xs <- many p
  return $ x ++ xs

anyChar :: ParseM a u n [a]
anyChar = do
  (x, _) <- guardText
  forward 1
  return [x]

char :: Eq a => a -> ParseM a u n [a]
char c = do
  (x, _) <- guardText
  guard (x == c)
  forward 1
  return [x]

charNot :: ParseM a u n [a] -> ParseM a u n [a]
charNot p = do
  (x, _) <- guardText
  parseState <- pGet
  case runParseM p parseState of
    Left memos -> do
      forward 1
      return [x]
    Right _ -> empty

letter :: ParseM Char u n [Char]
letter = do
  (x, _) <- guardText
  guard (isLetter x)
  forward 1
  return [x]
digit :: ParseM Char u n [Char]
digit = do
  (x, _) <- guardText
  guard (isDigit x)
  forward 1
  return [x]

many1Spaces :: ParseM Char u n [Char]
many1Spaces = many1 (char ' ')
manySpaces :: ParseM Char u n [Char]
manySpaces = many (char ' ')

parseAll :: ParseM t u n a -> ParseM t u n a
parseAll p = do
  e <- p
  ps <- pGet
  guard $ null $ psRest ps
  return e
