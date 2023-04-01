module ApplyBaseParser where
import BaseParser
import Node
import Control.Monad
import Control.Monad.Cont (lift)
import Control.Monad.State

node :: Node -> StateT (ParseState Node) (Either (Memos Node)) Node
node n = do
  (head, _) <- guardText -- not text here
  unless (head == n) returnFail
  forward 1
  return head

baseTest = (do
  n <- o6Test
  node EOF
  return n) <|> do
    n <- secTest
    node EOF
    return n

o6Test = do
  head <- o7Test
  tail <- manyOnes $ do
    node $ Symbol "+"
    o7Test
  return $ foldl (Application . Application (Symbol "+")) head tail

o7Test = do
  head <- o8Test
  tail <- manyOnes $ do
    node $ Symbol "*"
    o8Test
  return $ foldl (Application . Application (Symbol "*")) head tail

modifyApplyBase :: Node -> Either (Memos Node) Node
modifyApplyBase node = do
  case node of
    ApplyBase nodes -> parseApplyBase nodes
    _ -> return node

o8Test :: StateT (ParseState Node) (Either (Memos Node)) Node
o8Test = do
  headBase <- head <$> anyChar
  head <- lift $ modifyApplyBase headBase
  case head of
    String _ -> return head
    Number _ -> return head
    Boolean _ -> return head
    Symbol _ -> foldApplication head
    Application _ _ -> foldApplication head
    SectionApplyFromRight _ _ -> foldApplication head
    _ -> returnFail
  where
    foldApplication head = do
      tail <- many $ charNot $ node (Symbol "+") <|> node (Symbol "*") <|> node EOF
      tailModified <- forM tail (lift . modifyApplyBase)
      return $ foldl Application head tailModified


secTest = (do
  ope <- opeTest
  Application ope <$> baseTest) <|> (do
    base <- o6Test
    ope <- opeTest
    return $ SectionApplyFromRight ope base
  )

opeTest = node (Symbol "+") <|> node (Symbol "*")

parseApplyBase :: [Node] -> Either (Memos Node) Node
parseApplyBase nodes = evalStateT baseTest (0, initMemos, nodes ++ [EOF])
