module Intermediate where
import Control.Monad.State (StateT, MonadTrans (lift), execState, execStateT)
import Syntax
import Control.Monad.State.Class (modify, get, put)
import Data.Map.Strict (Map, alter, empty)
import Debug.Trace (trace)
import Control.Monad (forM)
-- data IGraph a = INode { label :: a, value :: Intermediate, adjacent :: [IGraph a] } deriving Show
data Intermediate = IVar String | INum Int | IRef String deriving Show
data IState a = IState { currentNodes :: [(a, Intermediate)], available :: [a], links :: Map a [a], next :: a } deriving Show

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

consume :: Int -> StateT (IState Int) (Either String) ()
consume requires = do
  (IState currentNodes available links next) <- get
  if requires > length available then lift $ Left ("AST Node " ++ show (head currentNodes) ++ " cannot receive sufficient inputs.")
  else let (uses, rest) = splitAt requires available in
    let newLinks = execState (forM uses (\index -> put . alter (\x -> case x of Just ar -> Just $ next : ar
                                                                                Nothing -> Just [next]) index =<< get)) links in do
      put $ IState currentNodes rest newLinks next
produce :: Monad m => Int -> StateT (IState Int) m ()
produce requires =
  modify $ \(IState currentNodes available links next) ->
    IState currentNodes (replicate requires next ++ available) links next
appendNode :: Monad m => Intermediate -> StateT (IState Int) m ()
appendNode node =
  modify $ \(IState currentNodes available links next) ->
    IState ((next, node) : currentNodes) available links next
nextCounter :: Monad m => StateT (IState Int) m ()
nextCounter = modify $ \(IState currentNodes available links next) -> IState currentNodes available links (next + 1)

convertIntermediate :: Exp -> StateT (IState Int) (Either String) ()
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
  nextCounter
convertIntermediate (ERef ref) = do
  (IState currentNodes available links next) <- get
  appendNode (IRef ref)
  consume $ length available
  produce 1
  nextCounter

convert :: Exp -> Either String (Map Int [Int])
convert exp = case execStateT (convertIntermediate exp) $ IState [] [] empty 0 of
  Left err -> Left err
  Right x -> let (IState n _ links _) = x in Right links

-- contrLinks :: [(a, a)] -> [(a, [a])]
-- contrLinks 

            -- let newLinks = updateWithKey (\k x -> Just $ if elem k uses then next : x else x) links in do