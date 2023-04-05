{-# LANGUAGE LambdaCase #-}
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

o6Operators = node (Symbol "+") <|> node (Symbol "-")
o6Test = do
  head <- o7Test
  tail <- many $ do
    symbol <- o6Operators
    exp <- o7Test
    return [symbol, exp]
  return $ fst $ foldl (\(a, state) b -> case state of
    0 -> (Application b a, 1)
    1 -> (Application a b, 0)) (head, 0) tail

o7Operators = node (Symbol "*") <|> node (Symbol "/")
o7Test = do
  head <- o8Test
  tail <- many $ do
    symbol <- o7Operators
    exp <- o8Test
    return [symbol, exp]
  return $ fst $ foldl (\(a, state) b -> case state of
    0 -> (Application b a, 1)
    1 -> (Application a b, 0)) (head, 0) tail

modifyApplyBaseForStatements nodes = forM nodes $ \case
  ConnectorDef connector (Connection nodes) -> do
    modifiedNodes <- forM nodes modifyApplyBase
    return $ ConnectorDef connector (Connection modifiedNodes)
  Connection nodes -> do
    modifiedNodes <- forM nodes modifyApplyBase
    return $ Connection modifiedNodes
  _ -> Left initMemos 

modifyApplyBase :: Node -> Either (Memos Node) Node
modifyApplyBase node = do
  case node of
    ApplyBase nodes -> parseApplyBase nodes
    Bypass c1 c2 -> do
      a <- modifyApplyBaseForStatements [c1]
      b <- modifyApplyBaseForStatements [c2]
      return $ Bypass (head a) (head b)
    _ -> return node

o8Test :: StateT (ParseState Node) (Either (Memos Node)) Node
o8Test = do
  headBase <- head <$> anyChar
  headNode <- lift $ modifyApplyBase headBase
  case headNode of
    String _ -> return headNode
    Number _ -> return headNode
    Boolean _ -> return headNode
    Symbol value -> if value `elem` ["+", "-", "*", "/"]
      then do
        tail <- many $ charNot $ opeTest <|> node EOF
        tailModified <- forM tail (lift . modifyApplyBase)
        if length tailModified == 1
          then return $ SectionApplyFromRight headNode (head tailModified)
          else return $ foldl Application headNode tailModified
      else foldApplication headNode
    Application _ _ -> foldApplication headNode
    SectionApplyFromRight _ _ -> foldApplication headNode
    _ -> returnFail
  where
    foldApplication head = do
      tail <- many $ charNot $ opeTest <|> node EOF
      tailModified <- forM tail (lift . modifyApplyBase)
      return $ foldl Application head tailModified


secTest = do
  base <- o6Test
  ope <- opeTest
  return $ Application ope base

opeTest = o6Operators <|> o7Operators

parseApplyBase :: [Node] -> Either (Memos Node) Node
parseApplyBase nodes = evalStateT baseTest (0, initMemos, nodes ++ [EOF])
