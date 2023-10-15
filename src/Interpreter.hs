module Interpreter where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Intermediate
import Data.Graph.Inductive (LEdge, LNode, Graph (mkGraph, empty), Gr, inn, lab, out)
import Types
import Control.Monad.State (StateT, MonadTrans (lift), MonadState (put, get), execState, gets, modify, runState)
import Control.Monad.ST (runST)
import Control.Monad (forM_, mzero, forM, when)
import Data.STRef (newSTRef, readSTRef)
import Data.Maybe (isJust, mapMaybe, isNothing, catMaybes)
import Data.List (find)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Debug.Trace ( trace )
import Primitives


emptyEvContext :: EvContext
emptyEvContext = EvContext [] 0 Map.empty (Map.fromList [(0, "root")]) 1

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
toHalfInParticles = map $ \(EvParticle nid (_, inName) v) -> HalfParticle inName v
toHalfOutParticles :: [EvParticle] -> [HalfParticle]
toHalfOutParticles = map $ \(EvParticle nid (outName, _) v) -> HalfParticle outName v

toFullInParticles :: [LEdge EdgeIndex] -> GraphInstanceId -> [HalfParticle] -> [EvParticle]
toFullInParticles inns gid = mapMaybe (\(HalfParticle idx v) -> case find (\(_, _, (_, inName)) -> inName == idx) inns of
                                                  Just (_, nextNid, eidx) -> Just (EvParticle (gid, nextNid) eidx v)
                                                  Nothing -> Nothing)
-- todo
toFullOutParticles :: [LEdge EdgeIndex] -> GraphInstanceId -> [HalfParticle] -> [EvParticle]
toFullOutParticles outs gid = mapMaybe (\(HalfParticle idx v) -> case find (\(_, _, (outName, _)) -> outName == idx) outs of
                                                  Just (_, nextNid, eidx) -> Just (EvParticle (gid, nextNid) eidx v)
                                                  Nothing -> Nothing)

runGraphLoop :: EvState -> IO (Maybe ())
runGraphLoop s = runMaybeT $ do
  -- trace (show s) return ()
  -- lift getChar
  let s' = handleMountNodes s
  let updateNodes = enumerateUpdateNodes s'
  (nextParticles, nss) <- factoryNextEvc s' updateNodes
  MaybeT $ runGraphLoop $ s' { evc = (evc s') { particles = nextParticles, time = time (evc s') + 1, nodeStates = nss } }

runEnv :: IEnv -> IO (Maybe ())
runEnv env = let evenv = fmap (\(ins, outs, (m, ic)) -> (ins, outs, (toGraph m, ic))) env in
  runGraphLoop $ EvState evenv emptyEvContext

factoryNextEvc :: EvState -> [(GraphInstanceId, NodeId)] -> MaybeT IO ([EvParticle], Map.Map (GraphInstanceId, NodeId) EvNodeState)
factoryNextEvc s@(EvState env evc) = loop where
    loop [] = return ([], nodeStates evc)
    loop (nid : rest) = do
      (newInFullParticles, newOutFullParticles, newNs) <- factoryByNid s nid
      (particles, nss) <- loop rest -- todo: tail rec
      let newNss = (case newNs of
            Just x -> Map.insert nid x nss
            Nothing -> Map.delete nid nss)
      return (newInFullParticles ++ newOutFullParticles ++ particles, newNss)

enumerateUpdateNodes :: EvState -> [(GraphInstanceId, NodeId)]
enumerateUpdateNodes s@(EvState env evc) = Set.toList (foldl Set.union Set.empty [a $ particles evc, b $ Map.toList $ nodeStates evc, Set.fromList genNodesAll]) where
  a :: [EvParticle] -> Set.Set (GraphInstanceId, NodeId)
  a [] = Set.empty
  a (EvParticle { nodeId = nid } : rest) = Set.insert nid (a rest)
  b :: [((GraphInstanceId, NodeId), EvNodeState)] -> Set.Set (GraphInstanceId, NodeId)
  b [] = Set.empty
  b ((id, _) : rest) = Set.insert id (b rest)
  genNodesAll :: [(GraphInstanceId, NodeId)]
  genNodesAll = do
    (gid, graphName) <- Map.toList $ graphInstances evc
    (_, _, (_, ic)) <- maybe mzero return $ findEnv s gid
    nid <- genNodes ic
    return (gid, nid)

getInOutHalfParticles :: Graph gr =>
  gr a EdgeIndex
  -> EvContext
  -> (GraphInstanceId, NodeId)
  -> ([HalfParticle], [HalfParticle], [LEdge EdgeIndex],
      [LEdge EdgeIndex])
getInOutHalfParticles graph evc (gid, nid) =
  let inns = inn graph nid in
  let inEidxs = map (\(_, _, eidx_) -> eidx_) inns in
  let inParticles = toHalfInParticles $ filter (\(EvParticle id_ eidx_ _) -> id_ == (gid, nid) && eidx_ `elem` inEidxs) (particles evc) in
  let outs = out graph nid in
  let outParticles = toHalfOutParticles $ filter (\(EvParticle (gid_, nid_) eidx_ _) -> gid_ == gid && (nid, nid_, eidx_) `elem` outs) (particles evc) in
  (inParticles, outParticles, inns, outs)

findEnv :: EvState -> GraphInstanceId -> Maybe ([String], [String], (Gr Intermediate EdgeIndex, IContext))
findEnv (EvState env (EvContext { graphInstances = gi })) gid = do
  graphName <- Map.lookup gid gi
  Map.lookup graphName env
findGraph s gid = do
  (_, _, (g, _)) <- findEnv s gid
  return g

factoryByNid :: EvState -> (GraphInstanceId, NodeId) -> MaybeT IO ([EvParticle], [EvParticle], Maybe EvNodeState)
factoryByNid evs@(EvState env evc) (gid, nid) = do
  graph <- maybe mzero return $ findGraph evs gid
  let (inParticles, outParticles, inns, outs) = getInOutHalfParticles graph evc (gid, nid)
  let (Just intermediate) = lab graph nid
  let ns = Map.lookup (gid, nid) $ nodeStates evc
  (inOp, outOp, newNs) <- factoryValue intermediate ns inParticles (length inns) outParticles (length outs)
  let newInParticles = handleInOp inOp inParticles
  let newOutParticles = handleOutOpGen outOp
  let newInFullParticles = toFullInParticles inns gid newInParticles
  let newOutFullParticles = toFullOutParticles outs gid newOutParticles
  return (newInFullParticles, newOutFullParticles, newNs)

handleInOp :: InOp -> [HalfParticle] -> [HalfParticle]
handleInOp InFlush _ = []
handleInOp (InRemove key) l = filter (\(HalfParticle inName _) -> inName /= key) l
handleInOp InNoOp l = l

handleOutOpGen :: OutOp -> [HalfParticle]
handleOutOpGen (OutAppend xs) = xs
handleOutOpGen OutNoOp = []

factoryValue :: Intermediate -> FactoryValue
factoryValue (IVar v) = case getPrimitive v of
  (Just p) -> pEval p
  _ -> \_ _ _ _ _ -> mzero
factoryValue (IImm v IGMPassive) = \_ [inParticle] _ outParticles _ -> do
  if not $ null outParticles
  then return (InNoOp, OutNoOp, Nothing)
  else return (InFlush, OutAppend [HalfParticle "result" $ passGhost v], Nothing)
factoryValue (IImm v IGMAlways) = \_ _ _ outParticles _ -> do
  if not $ null outParticles
  then return (InNoOp, OutNoOp, Nothing)
  else return (InNoOp, OutAppend [HalfParticle "result" v], Nothing)
factoryValue (IRef name) = \_ [HalfParticle _ v] _ outParticles maxOuts -> do
  if not $ null outParticles
  then return (InNoOp, OutNoOp, Nothing)
  else do
    return (InFlush, OutAppend $ map (\i -> HalfParticle ("refOut" ++ show i) v) [0..maxOuts - 1], Nothing)
factoryValue (IMount _) = \ns _ _ _ _ -> return (InNoOp, OutNoOp, ns)
factoryValue v = \_ _ _ _ _ -> trace (show v) mzero -- todo

handleMountNodes :: EvState -> EvState
handleMountNodes s = (`execState` s) $ do
  s <- get
  let mountNodesWithGid = (do
        gid <- Map.keys $ graphInstances (evc s)
        (_, _, (_, ic)) <- maybe mzero return $ findEnv s gid
        zip (repeat gid) (mountNodes ic))
  forM_ mountNodesWithGid $ \(gid, nid) -> do
    s <- get
    let (Just currentGraph) = findGraph s gid
    let (inParticles, outParticles, inns, outs) = getInOutHalfParticles currentGraph (evc s) (gid, nid)
    -- input
    let ns = case Map.lookup (gid, nid) (nodeStates $ evc s) of
          Just (EvMount id) -> Just id
          _ -> Nothing
    when (isNothing ns && length inParticles == length inns) $ do
      if any (\(HalfParticle _ v) -> isGhost v) inParticles
      then do
        let (_, toNode, (fromName, toName)) = head outs
        let newParticle = EvParticle (gid, toNode) (fromName, toName) VGhost
        modify $ \s -> s { evc = (evc s) { particles = newParticle : filter
          (\(EvParticle (gid_, nid_) (_, to) _) -> not $ gid_ == gid && nid_ == nid) (particles $ evc s) } }
      else do
        nss <- gets (nodeStates . evc)
        -- instantiate a new graph
        newGid <- gets (nextGraphInstanceId . evc)
        let (Just (IMount mountName)) = lab currentGraph nid
        modify $ \s -> s { evc = (evc s) {
            graphInstances = Map.insert newGid mountName (graphInstances $ evc s),
            nextGraphInstanceId = 1 + nextGraphInstanceId (evc s),
            nodeStates = Map.insert (gid, nid) (EvMount newGid) (nodeStates $ evc s)
          }}
        s <- get
        let (Just (_, _, (mountedGraph, mountedIc))) = findEnv s newGid
        forM_ inParticles $ \(HalfParticle name value) -> do
          let (Just mountedNid) = Map.lookup name $ addresses mountedIc
          s <- get
          let newParticle = EvParticle (newGid, mountedNid) ("_mountIn", "refIn") value
          modify (\s -> s { evc = (evc s) { particles = newParticle : particles (evc s) }})
        -- flush
        modify $ \s -> s { evc = (evc s) { particles = filter
          (\(EvParticle (gid_, nid_) (_, to) _) -> not $ gid_ == gid && nid_ == nid) (particles $ evc s) } }
    -- output
    nss <- gets (nodeStates . evc)
    case Map.lookup (gid, nid) nss of
      Just (EvMount mountedGid) -> do
        s <- get
        let (Just (_, _, (mountedGraph, mountedIc))) = findEnv s mountedGid
        when (null outParticles) $ do
          s <- get
          let (outOps, newS) = (`runState` s) (forM outs $ \(_, _, eidx@(from, to)) -> do
                let (Just mountedNid) = Map.lookup from $ addresses mountedIc
                s <- get
                let (argInParticles, argOutParticles, argInns, argOuts) = getInOutHalfParticles mountedGraph (evc s) (mountedGid, mountedNid)
                case argOutParticles of
                  [HalfParticle name value] -> do
                    let [(_, nidTo, _)] = argOuts
                    modify (\s -> s { evc = (evc s) { particles = filter
                      (\(EvParticle (gid_, nid_) (from, to) _) -> not $ gid_ == mountedGid && nid_ == nidTo) (particles $ evc s)} })
                    return (Just $ OutAppend [HalfParticle from value])
                  _ -> return Nothing)
          let newOutOps = catMaybes outOps
          when (length newOutOps == length outs) $ do
            put newS
            let newOutParticles = newOutOps >>= handleOutOpGen
            let newOutFullParticles = toFullOutParticles outs gid newOutParticles
            modify $ \s -> s { evc = (evc s) {
              particles = newOutFullParticles ++ filter (\EvParticle { nodeId = (gid_, _) } -> gid_ /= mountedGid) (particles (evc s)),
              nodeStates = Map.delete (gid, nid) (nodeStates $ evc s) } }
      _ -> return ()
