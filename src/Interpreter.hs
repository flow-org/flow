module Interpreter where

import qualified Data.Map.Strict as Map
import Intermediate
import Data.Graph.Inductive (LEdge, LNode, Graph (mkGraph, empty), Gr, inn, lab, out)
import Syntax
import Control.Monad.State (StateT, MonadTrans (lift), MonadState (put, get), forM_)
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef)
import Data.Maybe (isJust)
import Data.List (find)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Debug.Trace

data Value = VInt Int deriving Show

data EvParticle = EvParticle { nodeId :: NodeId, edgeIndex :: EdgeIndex, particleValue :: Value } deriving Show
type HalfParticle = (EdgeIndex, Value)
data EvContext = EvContext { particles :: [EvParticle], time :: Int }
data EvState = EvState { graph :: Gr Intermediate EdgeIndex, ic :: IContext, evc :: EvContext }

emptyEvContext :: EvContext
emptyEvContext = EvContext [] 0

edges :: Map.Map Int INode -> [LEdge Int]
edges m = inner $ Map.toList m where
  inner :: [(Int, INode)] -> [LEdge Int]
  inner [] = []
  inner ((key, INode _ adjs) : rest) = map (\(label, adj) -> (key, adj, label)) adjs ++ inner rest

nodes :: Map.Map Int INode -> [LNode Intermediate]
nodes m = inner $ Map.toList m where
  inner :: [(Int, INode)] -> [LNode Intermediate]
  inner [] = []
  inner ((key, INode node _) : rest) = (key, node) : inner rest

toGraph :: Map.Map Int INode -> Gr Intermediate Int
toGraph m = mkGraph (nodes m) (edges m)

toHalfParticles :: [EvParticle] -> [HalfParticle]
toHalfParticles = map $ \(EvParticle nid eidx v) -> (eidx, v)

toFullInParticles :: NodeId -> [HalfParticle] -> [EvParticle]
toFullInParticles nid = map $ \(eidx, v) -> EvParticle nid eidx v
-- todo
toFullOutParticles :: [LEdge EdgeIndex] -> [HalfParticle] -> [EvParticle]
toFullOutParticles outs hps = map (\((_, v), (_, nextNid, eidx)) -> EvParticle nextNid eidx v) $ zip hps outs

runGraph :: Graph gr => gr Intermediate Int -> IContext -> EvContext -> IO (Maybe ())
runGraph graph ic evc = runMaybeT $ do
  nextEvc <- MaybeT $ factoryNextEvc graph evc
  nextEvcGenerated <- MaybeT $ factoryNextGenerated graph evc (genNodes ic)
  MaybeT $ runGraph graph ic $ EvContext { particles = nextEvc ++ nextEvcGenerated, time = time evc + 1 }

factoryNextEvc :: Graph gr => gr Intermediate EdgeIndex -> EvContext -> IO (Maybe [EvParticle])
factoryNextEvc graph evc = runMaybeT $ loop (particles evc) [] where
    loop [] _ = return []
    loop particles@(EvParticle nid eidx v : rest) muteList = do
      if nid `elem` muteList
        then loop rest muteList
        else do
          (newInFullParticles, newOutFullParticles) <- factoryByNid graph evc nid
          ((newInFullParticles ++ newOutFullParticles) ++) <$> loop rest (nid : muteList)

factoryNextGenerated :: Graph gr => gr Intermediate EdgeIndex -> EvContext -> [Int] -> IO (Maybe [EvParticle])
factoryNextGenerated graph evc genNodes = runMaybeT $ loop genNodes where
  loop :: [Int] -> MaybeT IO [EvParticle]
  loop [] = return []
  loop (nid : rest) = do
    let (_, outParticles, _, outs) = getInOutHalfParticles graph evc nid
    let (Just intermediate) = lab graph nid
    newOutParticles <- MaybeT $ factoryGenValue evc intermediate outParticles (length outs)
    let newOutFullParticles = toFullOutParticles outs newOutParticles
    (newOutFullParticles ++) <$> loop rest

getInOutHalfParticles :: Graph gr =>
  gr a EdgeIndex
  -> EvContext
  -> Int
  -> ([HalfParticle], [HalfParticle], [LEdge EdgeIndex],
      [LEdge EdgeIndex])
getInOutHalfParticles graph evc nid =
  let inns = inn graph nid in
  let inEidxs = map (\(_, _, eidx_) -> eidx_) inns in
  let inParticles = toHalfParticles $ filter (\(EvParticle nid_ eidx_ _) -> nid_ == nid && eidx_ `elem` inEidxs) (particles evc) in
  let outs = out graph nid in
  let outParticles = toHalfParticles $ filter (\(EvParticle nid_ eidx_ _) -> (nid, nid_, eidx_) `elem` outs) (particles evc) in
  (inParticles, outParticles, inns, outs)

factoryByNid :: Graph gr =>
  gr Intermediate EdgeIndex
  -> EvContext -> Int -> MaybeT IO ([EvParticle], [EvParticle])
factoryByNid graph evc nid = do
  let (inParticles, outParticles, inns, outs) = getInOutHalfParticles graph evc nid
  let (Just intermediate) = lab graph nid
  (newInParticles, newOutParticles) <- MaybeT $ factoryValue evc intermediate inParticles (length inns) outParticles (length outs)
  let newInFullParticles = toFullInParticles nid newInParticles
  let newOutFullParticles = toFullOutParticles outs newOutParticles
  return (newInFullParticles, newOutFullParticles)

doOutput v = do
  putStr $ show v
  c <- getChar
  putStrLn ""
  if c == ';'
    then return $ Just ()
    else return Nothing

factoryValue :: EvContext -> Intermediate -> [HalfParticle] -> Int -> [HalfParticle] -> Int -> IO (Maybe ([HalfParticle], [HalfParticle]))
factoryValue _ (IVar "+") inParticles maxIn outParticles _ = do
  if not (null outParticles) || length inParticles < maxIn
  then return $ Just (inParticles, outParticles)
  else return $ Just ([], [(0, VInt (sum $ map (\(_, VInt x) -> x) inParticles))])
factoryValue _ (IVar "output") [(_, v)] _ _ _ = do
  doOutput v
  return $ Just ([], [])
factoryValue _ (IVar "trace") inParticles _ outParticles _ = do
  if not $ null outParticles
  then return $ Just (inParticles, outParticles)
  else do
    let [(_, v)] = inParticles
    doOutput v
    return $ Just ([], [(0, v)])
factoryValue _ (IVar "merge") inParticles _ outParticles _ = do
  if not $ null outParticles
  then return $ Just (inParticles, outParticles)
  else do
    let (_, v):rest = inParticles
    return $ Just (rest, [(0, v)])
factoryValue _ _ inParticles@[(_, v)] _ outParticles _ = do
  if not $ null outParticles
  then return $ Just (inParticles, outParticles)
  else return $ Just ([], [(0, v)])

factoryGenValue :: EvContext -> Intermediate -> [HalfParticle] -> Int -> IO (Maybe [HalfParticle])
factoryGenValue _ (INum i GMAlways) outParticles _ =
  if not $ null outParticles
  then return $ Just []
  else return $ Just [(0, VInt i)]
factoryGenValue evc (INum i GMOnce) outParticles _ =
  if not (null outParticles) || time evc > 0
  then return $ Just []
  else return $ Just [(0, VInt i)]

eval :: Command -> StateT (Maybe EvState) IO ()
eval (CExp exp) = do
  case convert exp of
    Left err -> lift $ putStrLn err
    Right (m, c) -> put $ Just $ EvState (toGraph m) c emptyEvContext
-- eval (CSpawn address exp) = do
--   state <- get
--   case state of
--     Just (graph, IContext addresses) ->
--       (case exp of
--         ENum i -> do
--           (case addresses !? address of
--             Just id -> (case suc graph id of
--                 [x] -> ))

--           return ())