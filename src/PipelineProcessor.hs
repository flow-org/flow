module PipelineProcessor where

import Node
import Control.Monad.State (State, MonadState (get, put), modify, evalState, StateT, MonadTrans (lift), runStateT)
import Debug.Trace (trace)
import Data.Maybe (catMaybes, isJust)
import BaseParser
import Control.Monad.Cont

getNewRef :: StateT Int (Either String) Ref
getNewRef = do
  next <- get
  modify (+ 1)
  return $ DirectedRef $ "__" ++ show next

toAdditionalParam L L2R ref = AdditionalParam (ApplyBase [Value $ Ref ref]) In L True
toAdditionalParam L R2L ref = AdditionalParam (ApplyBase [Value $ Ref ref]) Out L True
toAdditionalParam R L2R ref = AdditionalParam (ApplyBase [Value $ Ref ref]) Out R True
toAdditionalParam R R2L ref = AdditionalParam (ApplyBase [Value $ Ref ref]) In R True

processPipeline :: [Node] -> StateT Int (Either String) [SemanticStructure]
processPipeline [single] = return []
processPipeline (ApplyBase [Value (Ref r1)]:Bind bindType:ApplyBase [Value (Ref r2)]:tail) =
  (RefConnect r1 bindType r2:) <$> processPipeline (ApplyBase [Value (Ref r2)]:tail)
processPipeline [ApplyBase [Value (Ref ref)], Bind (Direction dir), ApplyBase ns] = do
  processMachineRun ns (Just $ toAdditionalParam L dir ref) Nothing
processPipeline (ApplyBase [Value (Ref r1)]:Bind (Direction dir1):ApplyBase ns:Bind (Direction dir2):tail) = do
  newRef <- getNewRef
  head <- processMachineRun ns (Just $ toAdditionalParam L dir1 r1) (Just $ toAdditionalParam R dir2 newRef)
  (head ++) <$> processPipeline (ApplyBase [Value (Ref newRef)]:Bind (Direction dir2):tail)
processPipeline (ApplyBase ns:Bind (Direction dir):ApplyBase [Value (Ref ref)]:tail) = do
  head <- processMachineRun ns (Just $ toAdditionalParam R dir ref) Nothing
  (head ++) <$> processPipeline (ApplyBase [Value (Ref ref)]:tail)
processPipeline (ApplyBase ns1:Bind (Direction dir):ApplyBase ns2:tail) = do
  newRef <- getNewRef
  processPipeline (ApplyBase ns1:Bind (Direction dir):ApplyBase [Value (Ref newRef)]:Bind (Direction dir):ApplyBase ns2:tail)

processMachineRun :: [Node] -> Maybe Node -> Maybe Node -> StateT Int (Either String) [SemanticStructure]
processMachineRun ns ext1 ext2 = do
  nextRef <- get
  let exts = catMaybes [ext1, ext2]
  processApplyBase (ns ++ exts)

type ApplyBaseParseState = ParseState Node (Int, [SemanticStructure])
processApplyBase :: [Node] -> StateT Int (Either String) [SemanticStructure]
processApplyBase nodes = do
  refIndex <- get
  case runStateT baseTest (0, initMemos, nodes ++ [EOF], (refIndex, [])) of
    Left memos -> lift $ Left ("ApplyBase parsing failed: " ++ show memos)
    Right (ss, (_, _, _, (newRefIndex, byproducts))) -> do
      put newRefIndex
      return $ ss : byproducts

getCurrentRef :: StateT ApplyBaseParseState (Either t) Int
getCurrentRef = do
  (_, _, _, (next, _)) <- get
  return next
setCurrentRef :: Int -> StateT ApplyBaseParseState (Either t) ()
setCurrentRef ref = modify $ \(i, memos, ns, (_, sss)) -> (i, memos, ns, (ref, sss))

pushStructures :: [SemanticStructure] -> StateT ApplyBaseParseState (Either t) ()
pushStructures sts = do
  modify $ \(i, memos, ns, (refIndex, _sts)) -> (i, memos, ns, (refIndex, sts ++ _sts))

node :: Node -> StateT ApplyBaseParseState (Either (Memos Node)) Node
node n = do
  (head, _) <- guardText -- not text here
  unless (head == n) returnFail
  forward 1
  return head

nodeLookAhead n = do
  (head, _) <- guardText -- not text here
  unless (head == n) returnFail

baseTest :: StateT ApplyBaseParseState (Either (Memos Node)) SemanticStructure
baseTest = (do
  n <- o6Test
  node EOF
  return n)
  -- <|> do
  --   n <- secTest
  --   node EOF
  --   return n

nodesToSemanticStructureM (head:tail) additionals = do
  let len = length (head:tail)
  folded <- foldM (\a b -> case a of
    (_, nodes, 0, i) -> return (Nothing, b : nodes, 1, i + 1) -- b: operator
    (Nothing, nodes, 1, i) -> do
      if i == len - 1 then do
        state <- get
        case runStateT (resolveMachineRun $ nodes ++ b:additionals) state of
          Left error -> trace error returnFail -- todo
          Right (headStructure, state) -> do
            put state
            return (Just headStructure, [], 0, i + 1)
      else do
        ref <- getCurrentRef
        setCurrentRef (ref + 1)
        state <- get
        let newRef = ApplyBase [Value $ Ref $ DirectedRef $ "__" ++ show ref]
        case runStateT (resolveMachineRun $ nodes ++ b:[AdditionalParam newRef Out R True]) state of
          Left error -> trace error returnFail -- todo
          Right (headStructure, state) -> do
            put state
            case headStructure of
              SSValue value -> return (Just headStructure, [Value value], 0, i + 1)
              MachineRun {} -> do
                pushStructures [headStructure]
                return (Nothing, [AdditionalParam newRef In L True], 0, i + 1)
    ) (Nothing, [head], 0, 1) tail
  case folded of
    (Just structure, _, _, _) -> return structure
    _ -> returnFail

oTest nextTest operatorsTest = do
  head <- nextTest
  tail <- many $ do
    symbol <- operatorsTest
    exp <- nextTest
    case exp of
      SSValue value -> return [symbol, Value value]
      _ -> returnFail
  if null tail then return head else do
    lastAdditionalParams <- do
      params <- manyOnes $ do
        (n, _) <- guardText
        case n of
          AdditionalParam {} -> do
            forward 1
            return n
          _ -> returnFail
      nodeLookAhead EOF
      return params
    headNode <- case head of
      SSValue value -> return $ Value value
      _ -> returnFail -- machine argument in operator is currently prohibited.
    nodesToSemanticStructureM (headNode:tail) lastAdditionalParams

o6Operators = node (Value (Symbol "+")) <|> node (Value (Symbol "-"))
o6Test :: StateT ApplyBaseParseState (Either (Memos Node)) SemanticStructure
o6Test = oTest o7Test o6Operators

o7Operators :: StateT ApplyBaseParseState (Either (Memos Node)) Node
o7Operators = node (Value (Symbol "*")) <|> node (Value (Symbol "/"))
o7Test :: StateT ApplyBaseParseState (Either (Memos Node)) SemanticStructure
o7Test = oTest o8Test o7Operators

o8Test :: StateT ApplyBaseParseState (Either (Memos Node)) SemanticStructure
o8Test = do
  headNode <- head <$> anyChar
  -- headNode <- lift $ modifyApplyBase headBase
  case headNode of
    Value (String v) -> return $ SSValue $ String v
    Value (Number v) -> return $ SSValue $ Number v
    Value (Boolean v) -> return $ SSValue $ Boolean v
    _ -> do
      collected <- many $ charNot $ opeTest <|> node EOF -- todo: section
      state <- get
      case runStateT (resolveMachineRun (headNode:collected)) state of
        Left error -> trace error returnFail -- todo
        Right (headStructure, state) -> do
          put state
          return headStructure

type MachineRunParseState = (Int, Int, Int, [ApplyParam], Maybe MachineRef, [SemanticStructure])
resolveMachineRun :: [Node] -> StateT ApplyBaseParseState (Either String) SemanticStructure
resolveMachineRun ns = do
  ref <- getCurrentRef
  case (`runStateT` (0, 0, ref, [], Nothing, [])) inner of
    Left error -> lift $ Left error
    Right (ss, (_, _, ref, _, _, byproducts)) -> do
      pushStructures byproducts
      setCurrentRef ref
      return ss
  where
  inner = do
    forM_ ns (\node -> do
      case node of
        Value (String v) -> handleValue (String v)
        Value (Number v) -> handleValue (Number v)
        Value (Boolean v) -> handleValue (Boolean v)
        Value (Symbol value) -> do
            is <- isMachineAppeared
            if is then handleValue (Symbol value) else handleMachine (MRWithRef value)
        Value (Ref v) -> handleValue (Ref v)
        ApplyBase ns -> handleApplyBase ns
        AdditionalParam {} -> handleAdditionalParam node)
    machine <- getMachine
    case machine of
      Nothing -> lift $ Left $ "No machine provided: " ++ show ns
      Just machine -> do
        params <- getParams
        return $ MachineRun machine params
  isMachineAppeared = isJust <$> getMachine
  addParam :: ApplyParam -> StateT MachineRunParseState (Either String) ()
  addParam param = do
    modify $ \(i, j, ref, params, m, sss) -> (i, j, ref, param:params, m, sss)
  getParams :: StateT MachineRunParseState (Either String) [ApplyParam]
  getParams = do
    (_, _, _, params, _, _) <- get
    return params
  appendByproducts :: [SemanticStructure] -> StateT MachineRunParseState (Either String) ()
  appendByproducts sss = do
    modify $ \(i, j, ref, ps, m, _sss) -> (i, j, ref, ps, m, _sss ++ sss)
  getNextRef :: StateT MachineRunParseState (Either String) Int
  getNextRef = do
    (_, _, ref, _, _, _) <- get
    return ref
  putNextRef :: Int -> StateT MachineRunParseState (Either String) ()
  putNextRef ref = do
    modify $ \(i, j, _, ps, m, sss) -> (i, j, ref, ps, m, sss)
  getNextIndex :: ParamType -> StateT MachineRunParseState (Either String) Int
  getNextIndex In = do
    (i, j, ref, ps, m, sss) <- get
    put (i + 1, j, ref, ps, m, sss)
    return i;
  getNextIndex Out = do
    (i, j, ref, ps, m, sss) <- get
    put (i, j + 1, ref, ps, m, sss)
    return j;
  getMachine = do
    (_, _, _, _, m, _) <- get
    return m;
  putMachine :: MachineRef -> StateT MachineRunParseState (Either String) ()
  putMachine machine = do
    modify $ \(i, j, ref, ps, m, sss) -> (i, j, ref, ps, Just machine, sss)
  handleValue :: Value -> StateT MachineRunParseState (Either String) ()
  handleValue value = do
    is <- isMachineAppeared
    unless is $ lift $ Left $ "Value appeared before machine: " ++ show value
    i <- getNextIndex In
    addParam $ Param R2L In i value
  handleAdditionalParam :: Node -> StateT MachineRunParseState (Either String) ()
  handleAdditionalParam (AdditionalParam (ApplyBase [Value value]) paramType position torelance) = do
    is <- isMachineAppeared
    case position of
      R -> do
        unless (torelance || is) $ lift $ Left $ "AdditionalParam appeared before machine: " ++ show value
        let direction = (case paramType of In -> R2L; Out -> L2R)
        i <- getNextIndex paramType
        addParam $ Param direction paramType i value
      L -> do
        when (is && not torelance) $ lift $ Left $ "AdditionalParam appeared after machine: " ++ show value
        let direction = (case paramType of In -> L2R; Out -> R2L)
        i <- getNextIndex paramType
        addParam $ Param direction paramType i value
  handleMachine :: MachineRef -> StateT MachineRunParseState (Either String) ()
  handleMachine machine = do
    oldMachine <- getMachine
    when (isJust oldMachine) $ lift $ Left "Machine already declared"
    putMachine machine
  handleApplyBase ns = do
    ref <- getNextRef
    putNextRef (ref + 1)
    let newRef = Ref $ DirectedRef $ "__" ++ show ref
    case runStateT (processPipeline [ApplyBase [Value newRef], Bind (Direction R2L), ApplyBase ns]) (ref + 1) of
      Left error -> lift $ Left error
      Right (sss, ref) -> do
        appendByproducts sss
        putNextRef ref
    handleValue newRef

opeTest = o6Operators <|> o7Operators