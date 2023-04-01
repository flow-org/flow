module Runner where

import Parser
import Node
import qualified Data.Map.Strict as Map
import Debug.Trace
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text

run file = case parse file of
  Left memos -> return $ Left $ "Failed to parse. " ++ show memos
  Right nodes -> do 
    prelude <- preludeJS
    return $ Right $ prelude ++ "\n" ++ intercalate "\n" (map toJS nodes)

preludeJS = do
  content <- TextIO.readFile "static/runtime.js"
  return $ Text.unpack content

toJS :: Node -> [Char]
toJS node = case node of
  ConnectorDef (Symbol connectorName) connection ->
    "const " ++ connectorName ++ " = " ++ toJS connection
  Connection connections ->
    foldr1 (\a b -> a ++ ".connect(" ++ b ++ ")") $ map toJS connections
  Application _ _ -> "new FromFunc(" ++ toJSExp node ++ ")"
  SectionApplyFromRight _ _ -> "new FromFunc(" ++ toJSExp node ++ ")"
  Symbol value -> value

reservedSymbols = Map.fromList [("+", "((a) => (b) => a + b)")]

toJSExp :: Node -> [Char]
toJSExp node = case node of
  String value -> show value
  Number value -> show value
  Boolean value -> if value then "true" else "false"
  Symbol value -> fromMaybe value $ Map.lookup value reservedSymbols
  Application fn arg -> toJSExp fn ++ "(" ++ toJSExp arg ++ ")"
  SectionApplyFromRight fn arg -> "((a) => " ++ toJSExp fn ++ "(a)(" ++ toJSExp arg ++ "))"
