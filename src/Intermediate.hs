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
type EdgeIndex = (String, String)
data INode = INode Intermediate [(EdgeIndex, NodeId)] deriving Show
data IRefState = IRefState { refNodeId :: NodeId, produced :: Bool, consumed :: Bool } deriving Show
data IContext = IContext { addresses :: Map.Map String NodeId, genNodes :: [NodeId] } deriving Show
data IAvailable = IAvailable { aNodeId :: NodeId, outName :: String, seekingInName :: Maybe String } deriving Show
data IState = IState {
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

convertIntermediate :: Exp -> StateT IState (Either String) ()
convertIntermediate (EVar varName) = do
  appendNode (IVar varName)
  let (inNames, outNames) = (requiredInputs varName, requiredOutputs varName)
  consume inNames
  produce outNames
  nextCounter
    -- _ -> lift $ Left (varName ++ " is not defined")
convertIntermediate (EConnect arrow l r) = (case arrow of
  AToLeft _ inLabel outLabel -> inner r l inLabel outLabel
  AToRight _ inLabel outLabel -> inner l r inLabel outLabel) where
    inner from to inLabel outLabel = do
      convertIntermediate from
      (case inLabel of
        Just inLabelStr -> modify $ \is -> is { consumeLabelStack = inLabelStr : consumeLabelStack is }
        Nothing -> return ())
      -- (case outLabel of
      --   Just outLabelStr -> ) -- todo: it is needed to memorize the last created edge
      convertIntermediate to
convertIntermediate (ENum i GMPassive) = do
  appendNode (INum i GMPassive)
  consume ["a"]
  produce ["result"]
  nextCounter
convertIntermediate (ENum i gm) = do
  appendNode (INum i gm)
  produce ["result"]
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
            (IAvailable nid outName _) <- pickAvailableItemToConsume "refIn"
            modify $ \is -> is { refs = Map.alter (\(Just irs) -> Just (irs { consumed = True })) ref refList }
            modifyCurrentNodes $ appendEdge nid (outName, "refIn") refnid
        else
          if produced then lift $ Left ("Ref " ++ ref ++ " is already produced")
          else do
            modify $ \is -> is { available = IAvailable refnid "refOut" Nothing : available is, refs = Map.alter (\(Just irs) -> Just (irs { produced = True })) ref refList }
    Nothing -> do
      appendNode (IRef ref)
      if isConsumer
        then do
          modify $ \is -> is { refs = Map.insert ref (IRefState (next is) False True) refList }
          consume ["refIn"]
          nextCounter
        else do
          modify $ \is -> is { refs = Map.insert ref (IRefState (next is) True False) refList }
          produce ["refOut"]
          nextCounter

--   appendNode (IRef ref)
--   consume $ length $ available is
--   produce 1
--   nextCounter
convertIntermediate (EAddress address) = do -- todo
  is <- get
  appendNode (IAddress address)
  consume ["addressIn"]
  produce ["addressOut"]
  modifyContext $ \ic -> ic { addresses = Map.insert address (next is) (addresses ic) }
  nextCounter

convert :: Exp -> Either String (Map.Map Int INode, IContext)
convert exp = case execStateT (convertIntermediate exp) $ IState Map.empty [] Map.empty 0 [] (IContext Map.empty []) of
  Left err -> Left err
  Right is -> Right (currentNodes is, context is)

-- contrLinks :: [(a, a)] -> [(a, [a])]
-- contrLinks 

            -- let newLinks = updateWithKey (\k x -> Just $ if elem k uses then next : x else x) links in do