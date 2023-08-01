module Interpreter where

import qualified Data.Map.Strict as Map
import Intermediate (INode (INode), Intermediate (IVar, INum), convert, IContext (genNodes))
import Data.Graph.Inductive (LEdge, LNode, Graph (mkGraph, empty), Gr, inn, lab, out)
import Syntax
import Control.Monad.State (StateT, MonadTrans (lift), MonadState (put, get), forM_)
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef)
import Data.Maybe (isJust)
import Data.List (find)

data Value = VInt Int deriving Show

type NodeId = Int
type EdgeIndex = Int
type Particle = (NodeId, EdgeIndex, Value)
type EvContext = [Particle]
type EvState = (Gr Intermediate EdgeIndex, IContext, EvContext)

emptyEvContext :: EvContext
emptyEvContext = []

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

runGraph :: Graph gr => gr Intermediate Int -> IContext -> EvContext -> IO ()
runGraph graph ic evc = do
  nextEvc <- factoryNextEvc graph evc
  nextEvcGenerated <- factoryNextGenerated graph evc (genNodes ic)
  -- print nextEvcGenerated
  -- return ()
  runGraph graph ic $ nextEvc ++ nextEvcGenerated

factoryNextEvc graph evc = loop evc [] where
    loop [] _ = return []
    loop ((nid, eidx, v) : rest) muteList = do
      if nid `elem` muteList
        then loop rest muteList
        else do
          let inns = map (\(_, _, label) -> label) $ inn graph nid
          let restParticles = filter (\(nid_, eidx_, _) -> nid_ == nid && eidx `elem` inns) rest
          if length restParticles == length inns - 1
            then do
              let (Just intermediate) = lab graph nid
              values <- factoryValue intermediate ((nid, eidx, v) : restParticles)
              let [(_, nextNid, nextEidx)] = out graph nid
              (map (\(_,value) -> (nextNid, nextEidx, value)) values ++) <$> loop rest (nid : muteList)
            else loop rest muteList

factoryNextGenerated graph evc genNodes = loop genNodes where
  loop [] = return []
  loop (nid : rest) = do
    let outs = out graph nid
    let occupied = isJust $ find (\(nextNid, eidx, _) -> (nid, nextNid, eidx) `elem` outs) evc
    if occupied then loop rest
    else do
      let (Just intermediate) = lab graph nid
      values <- factoryValue intermediate []
      let [(_, nextNid, nextEidx)] = outs
      (map (\(_,value) -> (nextNid, nextEidx, value)) values ++) <$> loop rest

factoryValue :: Intermediate -> EvContext -> IO [(Int, Value)]
factoryValue (IVar "+") particles =
  return [(0, VInt (sum $ map (\(_, _, VInt x) -> x) particles))]
factoryValue (IVar "output") [(_,_,v)] = do
  print $ show v
  getChar
  return []
factoryValue (INum i) [] =
  return [(0, VInt i)]

eval :: Command -> StateT (Maybe EvState) IO ()
eval (CExp exp) = do
  case convert exp of
    Left err -> lift $ putStrLn err
    Right (m, c) -> put $ Just (toGraph m, c, emptyEvContext)
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