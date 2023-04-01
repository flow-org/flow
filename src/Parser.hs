{-# LANGUAGE LambdaCase #-}
module Parser where

import Node
import BaseParser
import Control.Monad.State
import ApplyBaseParser (modifyApplyBase)

-- texts which cannot be used for symbols
reserved = ["<<", "-<"]

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
breakLineTest = char '\n'
fileTest = many $ do
  many breakLineTest
  optional lineTest
  many blankTest
statementTest = useMemo "statement" $ connectorDefTest <|> connectionTest
stringTest = useMemo "string" $ do
  char '"'
  text <- many $ (do
    char '\\'
    anyChar) <|> charNot (char '\\' <|> char '"' <|> char '\n')
  char '"'
  return $ String text
numberTest = useMemo "number" $ do
  text <- oneOrMore digit
  return $ Number (read text :: Int)
booleanTest = useMemo "boolean" $ (do
  string "true"
  return $ Boolean True) <|> do
    string "false"
    return $ Boolean False
symbolTest = useMemo "symbol" $ do
  text <- oneOrMore $ charNot $ char ' ' <|> char '\n' <|> char '(' <|> char ')'
  when (text `elem` reserved) returnFail
  return $ Symbol text
parensTest = useMemo "parens" $ do
  char '('
  many blankTest
  exp <- expTest
  many blankTest
  char ')'
  return exp
valueTest = useMemo "value" $ stringTest <|> numberTest <|> booleanTest <|> symbolTest <|> parensTest
applyBaseTest = useMemo "applyBase" $ do
  head <- valueTest
  tail <- manyOnes $ do
    oneOrMore blankTest
    valueTest
  return $ ApplyBase $ head:tail
expTest = useMemo "exp" $ applyBaseTest <|> valueTest

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

parseFile file = evalStateT lineTest (0, initMemos, file)
parse file = case parseFile file of
  Right nodes -> forM nodes $ \case
    ConnectorDef connector (Connection nodes) -> do
      modifiedNodes <- forM nodes modifyApplyBase
      return $ ConnectorDef connector (Connection modifiedNodes)
    Connection nodes -> do
      modifiedNodes <- forM nodes modifyApplyBase
      return $ Connection modifiedNodes
    _ -> Left initMemos 
  _ -> Left initMemos
