module Interpreter where

import qualified Data.Map.Strict as Map
import Intermediate
import Data.Graph.Inductive (LEdge, LNode, Graph (mkGraph, empty), Gr, inn, lab, out)
import Types
import Control.Monad.State (StateT, MonadTrans (lift), MonadState (put, get), forM_)
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef)
import Data.Maybe (isJust, mapMaybe)
import Data.List (find)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Debug.Trace
import Primitives


emptyEvContext :: EvContext
emptyEvContext = EvContext [] 0 Map.empty

edges :: Map.Map Int INode -> [LEdge EdgeIndex]
edges m = inner $ Map.toList m where
  inner :: [(Int, INode)] -> [LEdge EdgeIndex]
  inner [] = []
  inner ((key, INode _ adjs) : rest) = map (\(label, adj) -> (key, adj, label)) adjs ++ inner rest

nodes :: Map.Map Int INode -> [LNode Intermediate]
nodes m = inner $ Map.toList m where
  inner :: [(Int, INode)] -> [LNode Intermediate]
  inner [] = []
  inner ((key, INode node _) : rest) = (key, node) : inner rest

toGraph :: Map.Map Int INode -> Gr Intermediate EdgeIndex
toGraph m = mkGraph (nodes m) (edges m)

toHalfInParticles :: [EvParticle] -> [HalfParticle]
toHalfInParticles = map $ \(EvParticle nid (_, inName) v) -> (inName, v)
toHalfOutParticles :: [EvParticle] -> [HalfParticle]
toHalfOutParticles = map $ \(EvParticle nid (outName, _) v) -> (outName, v)

toFullInParticles :: [LEdge EdgeIndex] -> [HalfParticle] -> [EvParticle]
toFullInParticles inns hps = mapMaybe (\(_, nextNid, eidx@(_, inName)) -> case find (\(idx, _) -> inName == idx) hps of
                                                                     Just (_, v) -> Just (EvParticle nextNid eidx v)
                                                                     Nothing -> Nothing) inns
-- toFullInParticles nid = map $ \(eidx, v) -> EvParticle nid eidx v
-- todo
toFullOutParticles :: [LEdge EdgeIndex] -> [HalfParticle] -> [EvParticle]
toFullOutParticles outs hps = mapMaybe (\(_, nextNid, eidx@(outName, _)) -> case find (\(idx, _) -> outName == idx) hps of
                                                                     Just (_, v) -> Just (EvParticle nextNid eidx v)
                                                                     Nothing -> Nothing) outs
-- toFullOutParticles outs hps = map (\((_, v), (_, nextNid, eidx)) -> EvParticle nextNid eidx v) $ zip hps outs

runGraphLoop :: Graph gr => gr Intermediate EdgeIndex -> IContext -> EvContext -> IO (Maybe ())
runGraphLoop graph ic evc = runMaybeT $ do
  -- trace (show evc) return ()
  (nextEvP, nss) <- factoryNextEvc graph evc
  nextEvPGenerated <- factoryNextGenerated graph evc (genNodes ic)
  -- lift getChar
  let nextParticles = nextEvP ++ nextEvPGenerated
  -- if null nextParticles then MaybeT $ return Nothing
  --else
  MaybeT $ runGraphLoop graph ic $ evc { particles = nextEvP ++ nextEvPGenerated, time = time evc + 1, nodeStates = nss }

runGraph graph ic = runGraphLoop graph ic (EvContext [] 0 Map.empty)

factoryNextEvc :: Graph gr => gr Intermediate EdgeIndex -> EvContext -> MaybeT IO ([EvParticle], Map.Map NodeId EvNodeState)
factoryNextEvc graph evc = loop (particles evc) [] where
    loop [] _ = return ([], nodeStates evc)
    loop particles@(EvParticle nid eidx v : rest) muteList = do
      if nid `elem` muteList
        then loop rest muteList
        else do
          (newInFullParticles, newOutFullParticles, newNs) <- factoryByNid graph evc nid
          (particles, nss) <- loop rest (nid : muteList) -- todo: tail rec
          let newNss = (case newNs of
                Just x -> Map.insert nid x nss
                Nothing -> nss)
          return (newInFullParticles ++ newOutFullParticles ++ particles, newNss)

factoryNextGenerated :: Graph gr => gr Intermediate EdgeIndex -> EvContext -> [Int] -> MaybeT IO [EvParticle]
factoryNextGenerated graph evc genNodes = loop genNodes where
  loop :: [Int] -> MaybeT IO [EvParticle]
  loop [] = return []
  loop (nid : rest) = do
    let (_, outParticles, _, outs) = getInOutHalfParticles graph evc nid
    let (Just intermediate) = lab graph nid
    outOp <- factoryGenValue evc intermediate outParticles (length outs)
    let newOutFullParticles = toFullOutParticles outs (handleOutOpGen outOp)
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
  let inParticles = toHalfInParticles $ filter (\(EvParticle nid_ eidx_ _) -> nid_ == nid && eidx_ `elem` inEidxs) (particles evc) in
  let outs = out graph nid in
  let outParticles = toHalfOutParticles $ filter (\(EvParticle nid_ eidx_ _) -> (nid, nid_, eidx_) `elem` outs) (particles evc) in
  (inParticles, outParticles, inns, outs)

factoryByNid :: Graph gr =>
  gr Intermediate EdgeIndex
  -> EvContext -> Int -> MaybeT IO ([EvParticle], [EvParticle], Maybe EvNodeState)
factoryByNid graph evc nid = do
  let (inParticles, outParticles, inns, outs) = getInOutHalfParticles graph evc nid
  let (Just intermediate) = lab graph nid
  let ns = Map.lookup nid $ nodeStates evc
  (inOp, outOp, newNs) <- factoryValue intermediate ns inParticles (length inns) outParticles (length outs)
  let newInParticles = handleInOp inOp inParticles
  let newOutParticles = handleOutOpGen outOp
  let newInFullParticles = toFullInParticles inns newInParticles
  let newOutFullParticles = toFullOutParticles outs newOutParticles
  return (newInFullParticles, newOutFullParticles, newNs)

handleInOp :: InOp -> [HalfParticle] -> [HalfParticle]
handleInOp InFlush _ = []
handleInOp (InRemove key) l = filter (\(inName, _) -> inName /= key) l
handleInOp InNoOp l = l

-- handleOutOp :: OutOp -> [HalfParticle] -> [HalfParticle]
-- handleOutOp (OutAppend xs) l = xs ++ l
-- handleOutOp OutNoOp l = l

handleOutOpGen :: OutOp -> [HalfParticle]
handleOutOpGen (OutAppend xs) = xs
handleOutOpGen OutNoOp = []

factoryValue :: Intermediate -> FactoryValue
factoryValue (IVar v) = case getPrimitive v of
  (Just p) -> pEval p
  _ -> \_ _ _ _ _ -> MaybeT $ return Nothing
factoryValue (IImm v GMPassive) = \_ inParticles _ outParticles _ -> do
  if not $ null outParticles
  then return (InNoOp, OutNoOp, Nothing)
  else return (InFlush, OutAppend [("result", v)], Nothing)
factoryValue (IRef name) = \_ inParticles@[(_, v)] _ outParticles maxOuts -> do
  if not $ null outParticles
  then return (InNoOp, OutNoOp, Nothing)
  else do
    -- trace ("ref passed: " ++ name) return ()
    return (InFlush, OutAppend $ map (\i -> ("refOut" ++ show i, v)) [0..maxOuts - 1], Nothing)
factoryValue v = \_ _ _ _ _ -> trace (show v) MaybeT $ return Nothing

factoryGenValue :: EvContext -> Intermediate -> [HalfParticle] -> Int -> MaybeT IO OutOp
factoryGenValue _ (IImm v GMAlways) outParticles _ = do
  if not $ null outParticles
  then return OutNoOp
  else return $ OutAppend [("result", v)]
factoryGenValue evc (IImm v GMOnce) outParticles _ =
  if not (null outParticles) || time evc > 0
  then return OutNoOp
  else return $ OutAppend [("result", v)]
