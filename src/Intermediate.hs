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
-- data IGraph a = INode { label :: a, value :: Intermediate, adjacent :: [IGraph a] } deriving Show
data Intermediate = IVar String | INum Int GenMode | IRef String | IAddress String deriving Show
type NodeId = Int
type EdgeIndex = Int
data INode = INode Intermediate [(EdgeIndex, NodeId)] deriving Show
data IRefState = IRefState { refNodeId :: NodeId, produced :: Bool, consumed :: Bool } deriving Show
data IContext = IContext { addresses :: Map.Map String NodeId, genNodes :: [NodeId] } deriving Show
data IState = IState {
  currentNodes :: Map.Map NodeId INode,
  available :: [NodeId],
  refs :: Map.Map String IRefState,
  next :: NodeId,
  context :: IContext
} deriving Show

requiredInputs :: String -> Maybe Int
requiredInputs "+" = Just 2
requiredInputs "-" = Just 2
requiredInputs "*" = Just 2
requiredInputs "/" = Just 2
requiredInputs "input" = Just 0
requiredInputs "output" = Just 1
requiredInputs "trace" = Just 1
requiredInputs "merge" = Just 2 -- todo
requiredInputs _ = Nothing

requiredOutputs :: String -> Maybe Int
requiredOutputs "+" = Just 1
requiredOutputs "-" = Just 1
requiredOutputs "*" = Just 1
requiredOutputs "/" = Just 1
requiredOutputs "input" = Just 1
requiredOutputs "output" = Just 0
requiredOutputs "trace" = Just 1
requiredOutputs "merge" = Just 1
requiredOutputs _ = Nothing

appendEdge :: NodeId -> EdgeIndex -> NodeId -> Map.Map NodeId INode -> Map.Map NodeId INode
appendEdge nid eidx nidTo = Map.alter inner nid where
  inner = \case Just (INode value adj) -> Just $ INode value ((eidx, nidTo) : adj)
                Nothing -> Nothing
modifyCurrentNodes :: Monad m => (Map.Map NodeId INode -> Map.Map NodeId INode) -> StateT IState m ()
modifyCurrentNodes f = do
  is <- get
  let nodes = currentNodes is
  put $ is { currentNodes = f nodes }
consume :: Int -> StateT IState (Either String) ()
consume requires = do
  is <- get
  if requires > length (available is)
  then lift $ Left ("AST Node " ++ show (Map.lookup (next is) $ currentNodes is) ++ " cannot receive sufficient inputs.")
  else do
    let (uses, rest) = splitAt requires (available is)
    let newNodes = runST $ do
          nodes <- newSTRef $ currentNodes is
          forM_ (zip uses (iterate (+1) 0)) (\(id, index) -> do
            nextNodes <- appendEdge id index (next is) <$> readSTRef nodes
            -- nextNodes <- Map.alter
            --   (\case Just (INode value adj) -> Just $ INode value ((index, next is) : adj)
            --          Nothing -> Nothing) id <$> readSTRef nodes
            writeSTRef nodes nextNodes)
          readSTRef nodes
    put $ is { currentNodes = newNodes, available = rest }
produce :: Monad m => Int -> StateT IState m ()
produce requires =
  modify $ \is -> is { available = replicate requires (next is) ++ available is }
appendNode :: Monad m => Intermediate -> StateT IState m ()
appendNode node =
  modify $ \is -> is { currentNodes = Map.insert (next is) (INode node []) (currentNodes is) }
nextCounter :: Monad m => StateT IState m ()
nextCounter = modify $ \is -> is { next = next is + 1 }
currentCounter :: Monad m => StateT IState m Int
currentCounter = next <$> get
modifyContext :: Monad m => (IContext -> IContext) -> StateT IState m ()
modifyContext f = modify $ \is -> is { context = f $ context is }

convertIntermediate :: Exp -> StateT IState (Either String) ()
convertIntermediate (EVar varName) = do
  appendNode (IVar varName)
  case (requiredInputs varName, requiredOutputs varName) of
    (Just inNum, Just outNum) -> do
      consume inNum
      produce outNum
      nextCounter
    _ -> lift $ Left (varName ++ " is not defined")
convertIntermediate (EConnect arrow l r) = (case arrow of
  AToLeft _ -> inner r l
  AToRight _ -> inner l r) where
    inner from to = do
      convertIntermediate from
      convertIntermediate to
convertIntermediate (ENum i gm) = do
  appendNode (INum i gm)
  produce 1
  id <- currentCounter
  modifyContext $ \ic -> ic { genNodes = id : genNodes ic }
  nextCounter
convertIntermediate (ERef ref) = do -- todo
  is <- get
  let refList = refs is
  let isConsumer = length (available is) > 0 -- this check may be inappropriate
  case Map.lookup ref refList of
    Just (IRefState refnid produced consumed) ->
      if isConsumer
        then
          if consumed then lift $ Left ("Ref " ++ ref ++ " is already consumed")
          else do
            let (use : rest) = available is
            modify $ \is -> is { available = rest, refs = Map.alter (\(Just irs) -> Just (irs { consumed = True })) ref refList }
            modifyCurrentNodes $ appendEdge use 0 refnid
        else
          if produced then lift $ Left ("Ref " ++ ref ++ " is already produced")
          else do
            modify $ \is -> is { available = refnid : available is, refs = Map.alter (\(Just irs) -> Just (irs { produced = True })) ref refList }
    Nothing -> do
      appendNode (IRef ref)
      if isConsumer
        then do
          modify $ \is -> is { refs = Map.insert ref (IRefState (next is) False True) refList }
          consume 1
          nextCounter
        else do
          modify $ \is -> is { refs = Map.insert ref (IRefState (next is) True False) refList }
          produce 1
          nextCounter

--   appendNode (IRef ref)
--   consume $ length $ available is
--   produce 1
--   nextCounter
convertIntermediate (EAddress address) = do -- todo
  is <- get
  appendNode (IAddress address)
  consume 1
  produce 1
  modifyContext $ \ic -> ic { addresses = Map.insert address (next is) (addresses ic) }
  nextCounter

convert :: Exp -> Either String (Map.Map Int INode, IContext)
convert exp = case execStateT (convertIntermediate exp) $ IState Map.empty [] Map.empty 0 (IContext Map.empty []) of
  Left err -> Left err
  Right is -> Right (currentNodes is, context is)

-- contrLinks :: [(a, a)] -> [(a, [a])]
-- contrLinks 

            -- let newLinks = updateWithKey (\k x -> Just $ if elem k uses then next : x else x) links in do