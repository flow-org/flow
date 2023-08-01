module Intermediate where
import Control.Monad.State (StateT, MonadTrans (lift), execState, execStateT)
import Syntax
import Control.Monad.State.Class (modify, get, put)
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)
import Control.Monad (forM)
-- data IGraph a = INode { label :: a, value :: Intermediate, adjacent :: [IGraph a] } deriving Show
data Intermediate = IVar String | INum Int | IRef String | IAddress String deriving Show
type NodeId = Int
type EdgeIndex = Int
data INode = INode Intermediate [(EdgeIndex, NodeId)] deriving Show
data IContext = IContext { addresses :: Map.Map String NodeId, genNodes :: [NodeId] } deriving Show
data IState = IState { currentNodes :: Map.Map NodeId INode, available :: [NodeId], next :: NodeId, context :: IContext } deriving Show

requiredInputs :: String -> Maybe Int
requiredInputs "+" = Just 2
requiredInputs "-" = Just 2
requiredInputs "*" = Just 2
requiredInputs "/" = Just 2
requiredInputs "input" = Just 0
requiredInputs "output" = Just 1
requiredInputs _ = Nothing

requiredOutputs :: String -> Maybe Int
requiredOutputs "+" = Just 1
requiredOutputs "-" = Just 1
requiredOutputs "*" = Just 1
requiredOutputs "/" = Just 1
requiredOutputs "input" = Just 1
requiredOutputs "output" = Just 0
requiredOutputs _ = Nothing

consume :: Int -> StateT IState (Either String) ()
consume requires = do
  (IState currentNodes available next c) <- get
  if requires > length available then lift $ Left ("AST Node " ++ show (Map.lookup next currentNodes) ++ " cannot receive sufficient inputs.")
  else let (uses, rest) = splitAt requires available in
    let newNodes = foldl (\nodes (id, index) -> Map.alter (\x -> case x of Just (INode value adj) -> Just $ (INode value ((index, next) : adj))
                                                                           Nothing -> Nothing) id nodes) currentNodes $ zip uses (iterate (+1) 0) in
      put $ IState newNodes rest next c
produce :: Monad m => Int -> StateT IState m ()
produce requires =
  modify $ \(IState currentNodes available next c) ->
    IState currentNodes (replicate requires next ++ available) next c
appendNode :: Monad m => Intermediate -> StateT IState m ()
appendNode node =
  modify $ \(IState currentNodes available next c) ->
    IState (Map.insert next (INode node []) currentNodes) available next c
nextCounter :: Monad m => StateT IState m ()
nextCounter = modify $ \(IState currentNodes available next c) -> IState currentNodes available (next + 1) c
currentCounter :: Monad m => StateT IState m Int
currentCounter = do
  (IState _ _ counter _) <- get
  return counter
modifyContext :: Monad m => (IContext -> IContext) -> StateT IState m ()
modifyContext f = modify $ \(IState nodes available next c) -> IState nodes available next $ f c

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
convertIntermediate (ENum i) = do
  appendNode (INum i)
  produce 1
  id <- currentCounter
  modifyContext $ \(IContext addresses genNodes) -> IContext addresses (id : genNodes)
  nextCounter
convertIntermediate (ERef ref) = do -- todo
  (IState currentNodes available next c) <- get
  appendNode (IRef ref)
  consume $ length available
  produce 1
  nextCounter
convertIntermediate (EAddress address) = do -- todo
  (IState currentNodes available next c) <- get
  appendNode (IAddress address)
  consume 1
  produce 1
  modifyContext $ \(IContext addresses genNodes) -> IContext (Map.insert address next addresses) genNodes
  nextCounter

convert :: Exp -> Either String (Map.Map Int INode, IContext)
convert exp = case execStateT (convertIntermediate exp) $ IState Map.empty [] 0 (IContext Map.empty []) of
  Left err -> Left err
  Right x -> let (IState nodes _ _ c) = x in Right (nodes, c)

-- contrLinks :: [(a, a)] -> [(a, [a])]
-- contrLinks 

            -- let newLinks = updateWithKey (\k x -> Just $ if elem k uses then next : x else x) links in do