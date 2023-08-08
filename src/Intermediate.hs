{-# LANGUAGE LambdaCase #-}
module Intermediate where
import Control.Monad.State (StateT, MonadTrans (lift), execState, execStateT, forM_)
import Syntax
import Control.Monad.State.Class (modify, get, put)
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)
import Control.Monad (forM)
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, modifySTRef, readSTRef, writeSTRef)
import Data.Foldable (find)
-- data IGraph a = INode { label :: a, value :: Intermediate, adjacent :: [IGraph a] } deriving Show
data Intermediate = IVar String | INum Int GenMode | IRef String | IAddress String deriving Show
type NodeId = Int
type EdgeIndex = (String, String)
data INode = INode Intermediate [(EdgeIndex, NodeId)] deriving Show
data IRefState = IRefState { refNodeId :: NodeId, produced :: Bool, consumed :: Bool } deriving Show
data IContext = IContext { addresses :: Map.Map String NodeId, genNodes :: [NodeId] } deriving Show
data IAvailable = IAvailable { aNodeId :: NodeId, outName :: String, seekingInName :: Maybe String } deriving Show

-- new
data IInNode = IInNode { fromNid :: NodeId, fromName :: String, expectedToConnectWith :: Maybe String } deriving Show

data IState = IState {
  -- maybe old
  currentNodes :: Map.Map NodeId INode,
  available :: [IAvailable],
  refs :: Map.Map String IRefState,
  next :: NodeId,
  consumeLabelStack :: [String],
  context :: IContext
} deriving Show

requiredInputs :: String -> [String]
requiredInputs "+" = ["a", "b"]
requiredInputs "-" = ["a", "b"]
requiredInputs "*" = ["a", "b"]
requiredInputs "/" = ["a", "b"]
requiredInputs "input" = []
requiredInputs "output" = ["a"]
requiredInputs "trace" = ["a"]
requiredInputs "merge" = ["a", "b"] -- todo
requiredInputs "if" = ["condition"]

requiredOutputs :: String -> [String]
requiredOutputs "+" = ["result"]
requiredOutputs "-" = ["result"]
requiredOutputs "*" = ["result"]
requiredOutputs "/" = ["result"]
requiredOutputs "input" = ["result"]
requiredOutputs "output" = []
requiredOutputs "trace" = ["result"]
requiredOutputs "merge" = ["result"]
requiredOutputs "if" = ["then", "else"]

appendEdge :: NodeId -> EdgeIndex -> NodeId -> Map.Map NodeId INode -> Map.Map NodeId INode
appendEdge nid eidx nidTo = Map.alter inner nid where
  inner = \case Just (INode value adj) -> Just $ INode value ((eidx, nidTo) : adj)
                Nothing -> Nothing
modifyCurrentNodes :: Monad m => (Map.Map NodeId INode -> Map.Map NodeId INode) -> StateT IState m ()
modifyCurrentNodes f = do
  is <- get
  let nodes = currentNodes is
  put $ is { currentNodes = f nodes }
pickAvailableItemToConsume :: String -> StateT IState (Either String) IAvailable
pickAvailableItemToConsume inName = do
  is <- get
  if null (available is)
  then lift $ Left ("AST Node " ++ show (Map.lookup (next is) $ currentNodes is) ++ " cannot receive sufficient inputs.")
  else case consumeLabelStack is of
    seekingOutName:restStack -> let (c, d) = trace (show $ available is) span (\ia -> outName ia /= seekingOutName) (available is) in
      (case d of
        desired:rest -> do
          modify (\is -> is { consumeLabelStack = restStack, available = c ++ rest })
          return desired
        [] -> lift $ Left ("Specified product " ++ seekingOutName ++ " is not found."))
    [] -> do
      let (a, b) = span (\ia -> seekingInName ia /= Just inName) (available is)
      (case b of
        desired:rest -> modify (\is -> is { available = a ++ rest }) >> return desired
        [] -> let (head:tail) = available is in modify (\is -> is { available = tail }) >> return head)
consume :: [String] -> StateT IState (Either String) ()
consume [] = return ()
consume (inName:xs) = do
  (IAvailable nid outName _) <- pickAvailableItemToConsume inName
  is <- get
  let newNodes = appendEdge nid (outName, inName) (next is) (currentNodes is)
  modify (\is -> is { currentNodes = newNodes })
  consume xs

-- consume inNames = do
--   is <- get
--   if length inNames > length (available is)
--   then lift $ Left ("AST Node " ++ show (Map.lookup (next is) $ currentNodes is) ++ " cannot receive sufficient inputs.")
--   else do
--     let (uses, rest) = splitAt requires (available is)
--     let newNodes = runST $ do
--           nodes <- newSTRef $ currentNodes is
--           forM_ (zip uses (iterate (+1) 0)) (\(id, index) -> do
--             nextNodes <- appendEdge id index (next is) <$> readSTRef nodes
--             -- nextNodes <- Map.alter
--             --   (\case Just (INode value adj) -> Just $ INode value ((index, next is) : adj)
--             --          Nothing -> Nothing) id <$> readSTRef nodes
--             writeSTRef nodes nextNodes)
--           readSTRef nodes
--     put $ is { currentNodes = newNodes, available = rest }
produce :: Monad m => [String] -> StateT IState m ()
produce [] = return ()
produce (outName:xs) = modify (\is -> is { available = IAvailable (next is) outName Nothing : available is}) >> produce xs
-- produce requires =
--   modify $ \is -> is { available = replicate requires (next is) ++ available is }
appendNode :: Monad m => Intermediate -> StateT IState m ()
appendNode node =
  modify $ \is -> is { currentNodes = Map.insert (next is) (INode node []) (currentNodes is) }
nextCounter :: Monad m => StateT IState m ()
nextCounter = modify $ \is -> is { next = next is + 1 }
currentCounter :: Monad m => StateT IState m Int
currentCounter = next <$> get
modifyContext :: Monad m => (IContext -> IContext) -> StateT IState m ()
modifyContext f = modify $ \is -> is { context = f $ context is }

-- convertIntermediate :: Exp -> StateT IState (Either String) ()
-- convertIntermediate (EVar varName) = do
--   appendNode (IVar varName)
--   let (inNames, outNames) = (requiredInputs varName, requiredOutputs varName)
--   consume inNames
--   produce outNames
--   nextCounter
--     -- _ -> lift $ Left (varName ++ " is not defined")
-- convertIntermediate (EConnect arrow l r) = (case arrow of
--   AToLeft _ inLabel outLabel -> inner r l inLabel outLabel
--   AToRight _ inLabel outLabel -> inner l r inLabel outLabel) where
--     inner from to inLabel outLabel = do
--       convertIntermediate from
--       (case inLabel of
--         Just inLabelStr -> modify $ \is -> is { consumeLabelStack = inLabelStr : consumeLabelStack is }
--         Nothing -> return ())
--       -- (case outLabel of
--       --   Just outLabelStr -> ) -- todo: it is needed to memorize the last created edge
--       convertIntermediate to
-- convertIntermediate (ENum i GMPassive) = do
--   appendNode (INum i GMPassive)
--   consume ["a"]
--   produce ["result"]
--   nextCounter
-- convertIntermediate (ENum i gm) = do
--   appendNode (INum i gm)
--   produce ["result"]
--   id <- currentCounter
--   modifyContext $ \ic -> ic { genNodes = id : genNodes ic }
--   nextCounter
-- convertIntermediate (ERef ref) = do -- todo
--   is <- get
--   let refList = refs is
--   let isConsumer = length (available is) > 0 -- this check may be inappropriate
--   case Map.lookup ref refList of
--     Just (IRefState refnid produced consumed) ->
--       if isConsumer
--         then
--           if consumed then lift $ Left ("Ref " ++ ref ++ " is already consumed")
--           else do
--             (IAvailable nid outName _) <- pickAvailableItemToConsume "refIn"
--             modify $ \is -> is { refs = Map.alter (\(Just irs) -> Just (irs { consumed = True })) ref refList }
--             modifyCurrentNodes $ appendEdge nid (outName, "refIn") refnid
--         else
--           if produced then lift $ Left ("Ref " ++ ref ++ " is already produced")
--           else do
--             modify $ \is -> is { available = IAvailable refnid "refOut" Nothing : available is, refs = Map.alter (\(Just irs) -> Just (irs { produced = True })) ref refList }
--     Nothing -> do
--       appendNode (IRef ref)
--       if isConsumer
--         then do
--           modify $ \is -> is { refs = Map.insert ref (IRefState (next is) False True) refList }
--           consume ["refIn"]
--           nextCounter
--         else do
--           modify $ \is -> is { refs = Map.insert ref (IRefState (next is) True False) refList }
--           produce ["refOut"]
--           nextCounter

--   appendNode (IRef ref)
--   consume $ length $ available is
--   produce 1
--   nextCounter
-- convertIntermediate (EAddress address) = do -- todo
--   currentInNodes <- inNodes <$> get
--   appendNode (IAddress address)
--   consume ["addressIn"]
--   produce ["addressOut"]
--   modifyContext $ \ic -> ic { addresses = Map.insert address (next is) (addresses ic) }
--   nextCounter

matchOutsWithExpectedOuts :: [Exp] -> [IInNode] -> ([(IInNode, Exp)], [IInNode])
matchOutsWithExpectedOuts a b = fst $ inner a b where
  inner :: [Exp] -> [IInNode] -> (([(IInNode, Exp)], [IInNode]), [Exp])
  inner [] inNodes = (([], inNodes), [])
  inner outs [] = (([], []), outs)
  inner outs (inNode:ys) =
    let (a, b) = span (\(EOut _ x _) -> x /= Just (fromName inNode)) outs in
    case b of
      hit:rest -> let ((c1, c2), d) = inner (a ++ rest) ys in (((inNode, hit) : c1, c2), d)
      [] -> let ((c1, c2), head:tail) = inner outs ys in (((inNode, head) : c1, c2), tail)
sortExpectedOuts :: [Exp] -> [IInNode] -> [IInNode]
sortExpectedOuts exps inNodes =
  let outs = takeWhile (\out -> case out of
            EOut _ _ _ -> True
            EBi _ _ _ _ _ -> True
            _ -> False) exps in
  let matched = fst $ matchOutsWithExpectedOuts outs inNodes in
  inner matched outs
  where
    inner :: [(IInNode, Exp)] -> [Exp] -> [IInNode]
    inner _ [] = []
    inner matched (x : xs) =
      let (Just (inNode, _)) = find (\(_, e) -> e == x) matched in
        inNode : inner matched xs

matchInsWithInNodes :: [String] -> [IInNode] -> [(NodeId, EdgeIndex)]
matchInsWithInNodes a b = trace (show (a, b)) fst $ inner a b where
  inner :: [String] -> [IInNode] -> ([(NodeId, EdgeIndex)], [IInNode])
  inner [] rest = ([], rest)
  inner (inn:xs) inNodes =
    let (a, b) = span (\inNode -> Just inn /= expectedToConnectWith inNode) inNodes in
    case b of
      hit:rest -> let (c, d) = inner xs (a ++ rest) in ((fromNid hit, (fromName hit, inn)) : c, d)
      [] -> let (c, head:tail) = inner xs inNodes in ((fromNid head, (fromName head, inn)) : c, tail)

  -- let (withNames, withoutNames) = partition (\inNode -> isJust $ expectedToConnectWith inNode) inNodes in
  -- inner outs withNames withoutNames where
  --   inner [] rest1 rest2 = ([], rest1 ++ rest2)
  --   inner outs [] withoutNames =
  --     let dropped = drop (length outs) withoutNames in
  --     (zip withoutNames outs, dropped)
  --   inner ((e@EOut _ (Just name)):xs) withNames withoutNames =
  --     let targetNode = find (\inNode -> expectedToConnectWith )

-- handleMiddle :: Exp -> [IInNode] -> StateT IState (Either String) ([IInNode], [IInNode])
-- handleMiddle (EMiddle e) inNodes = do
--   expectedOuts <- handleMiddleOrPrimitive e inNodes
--   let (matched, rest) = matchOutsWithExpectedOuts outs expectedOuts
--   ar <- forM matched (\(inNode, e) -> handleOut e inNode)
--   if null ar
--     then return (rest, rest)
--     else return (rest, last ar)

-- handleIn :: Exp -> StateT IState (Either String) IInNode
-- handleIn (EIn e _ to) = do -- from ignore : todo
--   (_, inNodes) <- handleMiddle e []
--   let lastNodeInNode = head inNodes
--   return $ lastNodeInNode { expectedToConnectWith = to }

-- handleOut :: Exp -> IInNode -> StateT IState (Either String) [IInNode]
-- handleOut (EOut e _ to) inNode = do
--   let inNodeToUse = inNode { expectedToConnectWith = to }
--   (_, lastNodeInNodes) <- handleMiddle e [inNodeToUse]
--   return lastNodeInNodes

handle :: [Exp] -> [IInNode] -> [IInNode] -> StateT IState (Either String) (Maybe IInNode)
handle (EIn seq _ to : rest) ins _ = do
  result <- handle seq [] []
  let (Just last) = result
  handle rest (last { expectedToConnectWith = to } : ins) []
handle (EMiddle e : rest) ins _ = do
  expectedOuts <- handlePrimitive e ins
  if null rest
    then return $ Just (head expectedOuts)
    else do
      let sorted = sortExpectedOuts rest expectedOuts
      handle rest [] sorted
handle (EOut seq _ _ : rest) _ (headOut : tailOuts) = do
  handle seq [headOut] []
  if null rest
    then if null tailOuts then return Nothing else return $ Just (head tailOuts)
    else handle rest [] tailOuts

handlePrimitive :: Exp -> [IInNode] -> StateT IState (Either String) [IInNode]
handlePrimitive (EVar varName) externalIns = do
  let (inNames, outNames) = (requiredInputs varName, requiredOutputs varName)
  let edges = matchInsWithInNodes inNames externalIns
  counter <- next <$> get
  forM_ edges (\(nid, edgeIndex) -> do
    is <- get
    let newNodes = appendEdge nid edgeIndex counter (currentNodes is)
    modify (\is -> is { currentNodes = newNodes }))
  appendNode (IVar varName)
  nextCounter
  return $ map (\outName -> IInNode counter outName Nothing) outNames
handlePrimitive (ENum i GMPassive) externalIns = do
  let edges = matchInsWithInNodes ["a"] externalIns
  counter <- next <$> get
  forM_ edges (\(nid, edgeIndex) -> do
    is <- get
    let newNodes = appendEdge nid edgeIndex counter (currentNodes is)
    modify (\is -> is { currentNodes = newNodes }))
  appendNode (INum i GMPassive)
  nextCounter
  return [IInNode counter "result" Nothing]
handlePrimitive (ENum i gm) externalIns = do
  counter <- next <$> get
  modifyContext $ \ic -> ic { genNodes = counter : genNodes ic }
  appendNode (INum i gm)
  nextCounter
  return [IInNode counter "result" Nothing]
handlePrimitive (ERef ref) externalIns = do
  is <- get
  let refList = refs is
  let isConsumer = length externalIns > 0 -- this check may be inappropriate
  case Map.lookup ref refList of
    Just (IRefState refnid produced consumed) ->
      if isConsumer
        then
          if consumed then lift $ Left ("Ref " ++ ref ++ " is already consumed")
          else do
            let edges = matchInsWithInNodes ["refIn"] externalIns
            forM_ edges (\(nid, edgeIndex) -> do
              is <- get
              let newNodes = appendEdge nid edgeIndex refnid (currentNodes is)
              modify (\is -> is { currentNodes = newNodes }))
            modify $ \is -> is { refs = Map.alter (\(Just irs) -> Just (irs { consumed = True })) ref refList }
            return []
        else
          -- todo: disable produced check
          if produced then lift $ Left ("Ref " ++ ref ++ " is already produced")
          else do
            modify $ \is -> is { refs = Map.alter (\(Just irs) -> Just (irs { produced = True })) ref refList }
            return [IInNode refnid "refOut" Nothing]
    Nothing -> do
      appendNode (IRef ref)
      if isConsumer
        then do
          modify $ \is -> is { refs = Map.insert ref (IRefState (next is) False True) refList }
          let edges = matchInsWithInNodes ["refIn"] externalIns
          forM_ edges (\(nid, edgeIndex) -> do
            is <- get
            let newNodes = appendEdge nid edgeIndex (next is) (currentNodes is)
            modify (\is -> is { currentNodes = newNodes }))
          nextCounter
          return []
        else do
          modify $ \is -> is { refs = Map.insert ref (IRefState (next is) True False) refList }
          counter <- next <$> get
          let newInNodes = [IInNode counter "refOut" Nothing]
          nextCounter
          return newInNodes

-- do
--   inNodes <- forM ins handleIn
--   expectedOuts <- handleMiddleOrPrimitive e (inNodes ++ externalIns)
--   forM outs handleOut

convert :: [Exp] -> Either String (Map.Map Int INode, IContext)
convert exps = trace (show exps) $ case execStateT (handle exps [] []) $ IState Map.empty [] Map.empty 0 [] (IContext Map.empty []) of
  Left err -> Left err
  Right is -> Right (currentNodes is, context is)

-- contrLinks :: [(a, a)] -> [(a, [a])]
-- contrLinks 

            -- let newLinks = updateWithKey (\k x -> Just $ if elem k uses then next : x else x) links in do