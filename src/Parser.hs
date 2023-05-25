{-# LANGUAGE RankNTypes #-}
module Parser where

import Node
import BaseParser
import Control.Monad.State
import PipelineProcessor (processPipeline)
import Debug.Trace (trace)
-- import ApplyBaseParser (modifyApplyBase, modifyApplyBaseForStatements)

-- texts which cannot be used for symbols
reserved = ["<(", ")>", "=", "<->", "<-", "|", "->", "$", "{", "}"]

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
fileTest = do
  many $ breakLineTest <|> blankTest
  head <- lineTest
  many blankTest
  tail <- many $ do
    oneOrMore breakLineTest
    (do
      exp <- lineTest
      many blankTest
      return exp) <|> (do
        oneOrMore blankTest
        return [])
  breakLineTest
  char '$'
  return $ head ++ tail
statementTest = useMemo "statement" $ defTest <|> pipelineTest
stringTest = useMemo "string" $ do
  char '"'
  text <- many $ (do
    char '\\'
    anyChar) <|> charNot (char '\\' <|> char '"' <|> char '\n')
  char '"'
  return $ Value $ String text
numberTest = useMemo "number" $ do
  text <- oneOrMore digit
  return $ Value $ Number (read text :: Int)
booleanTest = useMemo "boolean" $ (do
  string "true"
  return $ Value $ Boolean True) <|> do
    string "false"
    return $ Value $ Boolean False
symbolTest = useMemo "symbol" $ do
  text <- oneOrMore $ charNot $ char ' ' <|> char '\n' <|> char '(' <|> char ')'
  when (text `elem` reserved) returnFail
  return $ Value $ Symbol text
refTest = useMemo "ref" $ do
  char '@'
  symbol <- symbolTest
  case symbol of
    Value (Symbol name) -> return $ Value $ Ref $ GeneralRef name
additionalParamTest :: StateT (ParseState Char t) (Either (Memos Char)) Node
additionalParamTest = useMemo "additionalParam" $ (do
  char '('
  many blankTest
  paramType <- (do string "<-"; return In) <|> (do string "->"; return Out)
  oneOrMore blankTest
  pipeline <- pipelineTest
  let (Pipeline nodes) = pipeline
  many blankTest
  char ')'
  return $ AdditionalParam nodes paramType R False) <|> (do
    char '('
    many blankTest
    pipeline <- pipelineTest
    let (Pipeline nodes) = pipeline
    oneOrMore blankTest
    -- <- and -> are not exp.
    paramType <- (do string "<-"; return In) <|> (do string "->"; return Out)
    many blankTest
    char ')'
    return $ AdditionalParam nodes paramType L False)
parensTest = useMemo "parens" $ do
  char '('
  many blankTest
  exp <- expTest
  many blankTest
  char ')'
  return exp
valueTest :: StateT (ParseState Char t) (Either (Memos Char)) Node
valueTest = useMemo "value" $
  stringTest <|>
  numberTest <|>
  booleanTest <|>
  refTest <|>
  symbolTest <|>
  parensTest
applyBaseTest :: StateT (ParseState Char t) (Either (Memos Char)) Node
applyBaseTest = useMemo "applyBase" $ do
  head <- valueTest
  tail <- manyOnes $ do
    oneOrMore blankTest
    valueTest <|> additionalParamTest
  return $ ApplyBase $ head:tail
machineTest = useMemo "machine" $ do
  string "machine"
  params <- manyOnes $ do
    oneOrMore blankTest
    machineParamTest
  (do many blankTest; char '\n') <|> oneOrMore blankTest
  many blankTest
  char '{'
  many blankTest
  head <- pipelineTest
  tail <- manyOnes $ do
    many blankTest
    char '\n'
    many blankTest
    pipelineTest
  many blankTest
  char '}'
  return $ Machine params (head:tail)
  -- todo
machineParamTest :: StateT (ParseState Char u) (Either (Memos Char)) Node
machineParamTest = useMemo "machineParam" $ do
  char '('
  paramType <- (do string "<-"; return In) <|> (do string "->"; return Out)
  params <- manyOnes $ do
    oneOrMore blankTest
    refTest <|> symbolTest
  char ')'
  return $ MachineParam paramType params
expTest = useMemo "exp" $ machineTest <|> applyBaseTest <|> valueTest

pipelineTest = useMemo "pipeline" $ do
  head <- expTest
  tail <- many $ do
    oneOrMore blankTest
    connection <- (Bind <$> (do
      string "<-"
      return $ Direction R2L) <|> (do
        string "<->"
        return Bi) <|> (do
          string "->"
          return $ Direction L2R)) <|> bypassTest
    oneOrMore blankTest
    exp <- expTest
    return [connection, exp]
  return $ Pipeline $ head:tail

bypassTest :: StateT (ParseState Char t) (Either (Memos Char)) Node
bypassTest = useMemo "bypass" $ do
  string "<("
  oneOrMore blankTest
  r2l <- pipelineTest
  oneOrMore blankTest
  string "|"
  oneOrMore blankTest
  l2r <- pipelineTest
  oneOrMore blankTest
  string ")>"
  return $ Bypass r2l l2r

defTest = useMemo "def" $ do
  connector <- symbolTest
  oneOrMore blankTest
  string "="
  oneOrMore blankTest
  Def connector <$> pipelineTest

parseFile file = evalStateT fileTest (0, initMemos, file, ())
parse_ file = case parseFile (file ++ "\n$") of
  Right nodes -> Right nodes
  _ -> Left initMemos
parse file = case parseFile (file ++ "\n$") of
  Right [Pipeline nodes] -> Right $ runStateT (processPipeline nodes) 0
  _ -> Left initMemos

-- type Equal a b = forall p. (p a -> p b, p b -> p a)
-- refl :: Equal a a
-- refl = (id, id)
-- symm :: Equal a b -> Equal b a
-- symm (x, y) = (y, x)
-- newtype Pack a = Pack { unpack :: a }
-- apply :: Equal a b -> a -> b
-- apply (s, t) = unpack . s . Pack
-- newtype Lift f a b = Lift { unlift :: Equal (f a) (f b) }
-- lift :: Equal a b -> Equal (f a) (f b)
-- lift (s, t) = (unlift (s (Lift refl)), unlift (t, (Lift refl)))
