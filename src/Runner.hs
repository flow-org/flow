module Runner where

import Parser
import Node
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text

run file = case parse file of
  Left memos -> return $ Left $ "Failed to parse. " ++ show memos
  Right nodes -> do 
    prelude <- preludeJS
    return $ Right $ prelude ++ "\n" ++ intercalate "\n" (map toJS $ concatMap deconstruct nodes)

preludeJS = do
  content <- TextIO.readFile "static/runtime.js"
  return $ Text.unpack content

primitives = ["input", "output"]
reservedSymbols = Map.fromList [
    ("+", "((a) => (b) => a + b)"),
    ("-", "((a) => (b) => a - b)"),
    ("*", "((a) => (b) => a * b)"),
    ("/", "((a) => (b) => a / b)")
  ]

deconstruct :: Node -> [Node]
deconstruct node = case node of
  -- ConnectorDef def c ->
  --   let h:t = deconstruct c in
  --     ConnectorDef def h : t
  -- Connection (head:(Bind bindType:(head3:tail))) ->
  --   let (Connection h):t = deconstruct (Connection (head3:tail)) in
  --     Connection (head:(Bind bindType:h)) : t
  -- Connection (head:(Bypass (Connection r2l) (Connection l2r):(head3:tail))) ->
  --   let (Connection h):t = deconstruct (Connection (head3:tail))
  --       (Connection r2lh):r2lt = deconstruct (Connection r2l)
  --       (Connection l2rh):l2rt = deconstruct (Connection l2r) in
  --     [
  --       Connection ([head, Bind R2L] ++ r2lh ++ [Bind R2L] ++ h),
  --       Connection ([head, Bind L2R] ++ l2rh ++ [Bind L2R] ++ [head3])
  --     ] ++ r2lt ++ l2rt
  -- Connection [head] -> [Connection [head]]

toJS :: Node -> [Char]
toJS node = case node of
  -- ConnectorDef (Symbol connectorName) connection ->
  --   "const " ++ connectorName ++ " = () => " ++ toJS connection
  -- Connection connections ->
  --   -- foldr1 (\a b -> a ++ ".connect(" ++ b ++ ")") $ map toJS connections
  --   let toJSed = map toJS connections
  --       (result, lastState) = foldr (\a (b, parseState) -> case parseState of
  --         0 -> ("." ++ a ++ "(" ++ b ++ ")", 1)
  --         1 -> (a ++ b, 0)) (last toJSed, 0) (init toJSed) in
  --       result -- todo: should check lastState
  -- Application (Symbol fnName) _ | fnName `elem` primitives
  --   -> toJSExp node
  -- Application _ _ -> "new FromFunc(" ++ toJSExp node ++ ")"
  -- SectionApplyFromRight _ _ -> "new FromFunc(" ++ toJSExp node ++ ")"
  -- Symbol value -> value ++ "()"
  -- Bind R2L -> "connect"
  -- Bind L2R -> "connected"
  -- Bind Bi -> "biconnect"

toJSExp :: Node -> [Char]
toJSExp node = case node of
  String value -> show value
  Number value -> show value
  Boolean value -> if value then "true" else "false"
  Symbol value -> fromMaybe (value ++ "()") $ Map.lookup value reservedSymbols
  Application fn arg -> (case fn of
    Symbol value -> fromMaybe value $ Map.lookup value reservedSymbols -- no ()
    _ -> toJSExp fn) ++ "(" ++ toJSExp arg ++ ")"
  SectionApplyFromRight fn arg -> "((a) => " ++ toJSExp fn ++ "(a)(" ++ toJSExp arg ++ "))"
