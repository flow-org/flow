module ApplyBaseParser where
import BaseParser
import Node
import Control.Monad
import Control.Monad.Cont (lift)
import Control.Monad.State
import Data.Maybe (isJust)
import Debug.Trace (trace)

parseApplyBase :: [Node] -> Int -> Either (Memos Node) (Int, [SemanticStructure])
parseApplyBase nodes refIndex = case runStateT baseTest (0, initMemos, nodes ++ [EOF], (refIndex, [])) of
  Left memos -> Left memos
  Right (ss, (_, _, _, (newRefIndex, byproducts))) -> Right (newRefIndex, ss : byproducts)

pushStructures :: [SemanticStructure] -> StateT (ParseState Node (Int, [SemanticStructure])) (Either (Memos Node)) ()
pushStructures sts = do
  modify $ \(i, memos, ns, (refIndex, _sts)) -> (i, memos, ns, (refIndex, sts ++ _sts))

node :: Node -> StateT (ParseState Node t) (Either (Memos Node)) Node
node n = do
  (head, _) <- guardText -- not text here
  unless (head == n) returnFail
  forward 1
  return head

baseTest :: StateT (ParseState Node (Int, [SemanticStructure])) (Either (Memos Node)) SemanticStructure
baseTest = (do
  n <- o6Test
  node EOF
  return n)
  -- <|> do
  --   n <- secTest
  --   node EOF
  --   return n

nodesToSemanticStructureM head tail = do
  folded <- foldM (\a b -> case a of
    (Nothing, nodes, 0) -> return (Nothing, b : nodes, 1) -- b: operator
    (Nothing, nodes, 1) -> case resolveMachineRun (nodes ++ [b]) of
      Left error -> returnFail -- todo
      Right (headStructure:tailStructure) -> do
        pushStructures tailStructure
        case headStructure of
          SSValue value -> return (Just headStructure, [Value value], 0)
          _ -> return (Just headStructure, [], 0) -- error unless this is last element
    ) (Nothing, [head], 0) tail
  case folded of
    (Just structure, _, _) -> return structure
    _ -> returnFail

o6Operators = node (Value (Symbol "+")) <|> node (Value (Symbol "-"))
o6Test :: StateT (ParseState Node (Int, [SemanticStructure])) (Either (Memos Node)) SemanticStructure
o6Test = do
  head <- o7Test
  tail <- many $ do
    symbol <- o6Operators
    exp <- o7Test
    case exp of
      SSValue value -> return [symbol, Value value]
      _ -> returnFail
  if null tail then return head else do
    headNode <- case head of
      SSValue value -> return $ Value value
      _ -> returnFail -- machine argument in operator is currently prohibited.
    nodesToSemanticStructureM headNode tail

o7Operators :: StateT (ParseState Node t) (Either (Memos Node)) Node
o7Operators = node (Value (Symbol "*")) <|> node (Value (Symbol "/"))
o7Test :: StateT (ParseState Node (Int, [SemanticStructure])) (Either (Memos Node)) SemanticStructure
o7Test = do
  head <- o8Test
  tail <- many $ do
    symbol <- o7Operators
    exp <- o8Test
    case exp of
      SSValue value -> return [symbol, Value value]
      _ -> returnFail
  if null tail then return head else do
    headNode <- case head of
      SSValue value -> return $ Value value
      _ -> returnFail -- machine argument in operator is currently prohibited.
    nodesToSemanticStructureM headNode tail

o8Test :: StateT (ParseState Node (Int, [SemanticStructure])) (Either (Memos Node)) SemanticStructure
o8Test = do
  headNode <- head <$> anyChar
  -- headNode <- lift $ modifyApplyBase headBase
  case headNode of
    Value (String v) -> return $ SSValue $ String v
    Value (Number v) -> return $ SSValue $ Number v
    Value (Boolean v) -> return $ SSValue $ Boolean v
    _ -> do
      collected <- many $ charNot $ opeTest <|> node EOF -- todo: section
      trace (show (headNode:collected)) $ return 0
      case resolveMachineRun (headNode:collected) of
        Left error -> trace error returnFail -- todo
        Right (headStructure:tailStructure) -> do
          pushStructures tailStructure
          return headStructure

type MachineRunParseState = (Int, Int, [ApplyParam], Maybe MachineRef)
resolveMachineRun :: [Node] -> Either String [SemanticStructure]
resolveMachineRun ns = (`evalStateT` (0, 0, [], Nothing)) $ do
  forM_ ns (\node -> do
    case node of
      Value (String v) -> handleValue (String v)
      Value (Number v) -> handleValue (Number v)
      Value (Boolean v) -> handleValue (Boolean v)
      Value (Symbol value) -> do
        is <- isMachineAppeared
        if is then handleValue (Symbol value) else handleMachine (MRWithRef value)
      Value (Ref v) -> handleValue (Ref v)
      -- ApplyBase 
      AdditionalParam {} -> handleAdditionalParam node)
  machine <- getMachine
  case machine of
    Nothing -> lift $ Left $ "No machine provided: " ++ show ns
    Just machine -> do
      params <- getParams
      return [MachineRun machine params]
   where
      isMachineAppeared = isJust <$> getMachine
      addParam :: ApplyParam -> StateT MachineRunParseState (Either String) ()
      addParam param = do
        modify $ \(i, j, params, m) -> (i, j, param:params, m)
      getParams :: StateT MachineRunParseState (Either String) [ApplyParam]
      getParams = do
        (_, _, params, _) <- get
        return params
      getNextIndex :: ParamType -> StateT MachineRunParseState (Either String) Int
      getNextIndex In = do
        (i, j, ps, m) <- get
        put (i + 1, j, ps, m)
        return i;
      getNextIndex Out = do
        (i, j, ps, m) <- get
        put (i, j + 1, ps, m)
        return j;
      getMachine = do
        (_, _, _, m) <- get
        return m;
      putMachine :: MachineRef -> StateT MachineRunParseState (Either String) ()
      putMachine machine = do
        modify $ \(i, j, ps, m) -> (i, j, ps, Just machine)
      handleValue :: Value -> StateT MachineRunParseState (Either String) ()
      handleValue value = do
        is <- isMachineAppeared
        unless is $ lift $ Left $ "Value appeared before machine: " ++ show value
        i <- getNextIndex In
        addParam $ Param R2L In i value
      handleAdditionalParam :: Node -> StateT MachineRunParseState (Either String) ()
      handleAdditionalParam (AdditionalParam (Value value) paramType position torelance) = do
        is <- isMachineAppeared
        case position of
          L -> do
            unless (torelance || is) $ lift $ Left $ "AdditionalParam appeared before machine: " ++ show value
            let direction = (case paramType of In -> R2L; Out -> L2R)
            i <- getNextIndex paramType
            addParam $ Param direction paramType i value
          R -> do
            when (is && not torelance) $ lift $ Left $ "AdditionalParam appeared after machine: " ++ show value
            let direction = (case paramType of In -> L2R; Out -> R2L)
            i <- getNextIndex paramType
            addParam $ Param direction paramType i value
      handleMachine :: MachineRef -> StateT MachineRunParseState (Either String) ()
      handleMachine machine = do
        oldMachine <- getMachine
        when (isJust oldMachine) $ lift $ Left "Machine already declared"
        putMachine machine

opeTest = o6Operators <|> o7Operators
