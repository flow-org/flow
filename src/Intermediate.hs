{-# LANGUAGE LambdaCase #-}
module Intermediate where
import Control.Monad.State (StateT, MonadTrans (lift), execState, execStateT, forM_, unless)
import Types
import Control.Monad.State.Class (modify, get, put)
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)
import Control.Monad (forM)
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, modifySTRef, readSTRef, writeSTRef)
import Data.Foldable (find)
import Primitives
import Data.Maybe (isJust)

appendEdge :: NodeId -> EdgeIndex -> NodeId -> Map.Map NodeId INode -> Map.Map NodeId INode
appendEdge nid eidx nidTo = Map.alter inner nid where
  inner = \case Just (INode value adj) -> Just $ INode value ((eidx, nidTo) : adj)
                Nothing -> Nothing
appendNode :: Monad m => Intermediate -> StateT IState m ()
appendNode node =
  modify $ \is -> is { currentNodes = Map.insert (next is) (INode node []) (currentNodes is) }
nextCounter :: Monad m => StateT IState m ()
nextCounter = modify $ \is -> is { next = next is + 1 }
modifyContext :: Monad m => (IContext -> IContext) -> StateT IState m ()
modifyContext f = modify $ \is -> is { context = f $ context is }

availableArgToAvailable :: IAvailableArg -> IAvailable
availableArgToAvailable (IAvailableArg nid (IArg name) c) = IAvailable nid name c
availableArgToAvailable (IAvailableArg nid (ISpread prefix) c) = IAvailable nid (prefix ++ "0") c

availableArgsToAvailables :: [IAvailableArg] -> Int -> [IAvailable]
availableArgsToAvailables a b = fst $ inner a b where
  inner :: [IAvailableArg] -> Int -> ([IAvailable], Int)
  inner [] n = ([], n)
  inner (i@(IAvailableArg _ (IArg _) _) : rest) n = let (a, b) = inner rest (n - 1) in (availableArgToAvailable i : a, b)
  inner (IAvailableArg nid (ISpread prefix) c : rest) n =
    let (a, b) = inner rest n in
    (map (\j -> IAvailable nid (prefix ++ show j) c) [0..(b - 1)] ++ a, 0)

matchOutsWithExpectedOuts :: [ExpWithInfo] -> [IAvailableArg] -> Either ExpInfo ([(IAvailable, ExpWithInfo)], [IAvailable])
matchOutsWithExpectedOuts a b = fst <$> inner a (availableArgsToAvailables b (length a)) where
  inner :: [ExpWithInfo] -> [IAvailable] -> Either ExpInfo (([(IAvailable, ExpWithInfo)], [IAvailable]), [ExpWithInfo])
  inner [] inNodes = return (([], inNodes), [])
  inner outs [] = return (([], []), outs)
  inner outs (inNode:ys) = do
    (a, b) <- spanM (\n -> do
          name <- (case n of
              (EOut { exFrom = x }, _) -> return x
              (EBi { outFrom =  x }, _) -> return x
              (_, info) -> Left info)
          return $ name /= Just (fromName inNode)) outs
    case b of
      hit:rest -> do
        ((c1, c2), d) <- inner (a ++ rest) ys
        return (((inNode, hit) : c1, c2), d)
      [] -> do
        ((c1, c2), exps) <- inner outs ys
        case exps of
          head:tail -> return (((inNode, head) : c1, c2), tail)
          [] -> Left (ExpInfo (-1) (-1))
  spanM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
  spanM _ [] = return ([], [])
  spanM f (x:xs) = do
    result <- f x
    if result then do
      (a, b) <- spanM f xs
      return (x : a, b)
    else return ([], x:xs)
sortExpectedOuts :: [ExpWithInfo] -> [IAvailableArg] -> Either ExpInfo [IAvailable]
sortExpectedOuts exps inNodes = do
  let outs = takeWhile (\out -> case out of
            (EOut {}, _) -> True
            (EBi {}, _) -> True
            _ -> False) exps
  matched <- fst <$> matchOutsWithExpectedOuts outs inNodes
  return $ inner matched outs
  where
    inner :: [(IAvailable, ExpWithInfo)] -> [ExpWithInfo] -> [IAvailable]
    inner _ [] = []
    inner matched (x : xs) =
      let (Just (inNode, _)) = find (\(_, e) -> e == x) matched in -- todo: exhaustive
        inNode : inner matched xs

argsToStrings :: [IArg] -> Int -> [String]
argsToStrings a b = fst $ inner a b where
  inner :: [IArg] -> Int -> ([String], Int)
  inner [] n = ([], n)
  inner (IArg name : rest) n = let (a, b) = inner rest (n - 1) in (name : a, b) -- todo: tail rec
  inner (ISpread prefix : rest) n =
    let (a, b) = inner rest n in
    (map (\i -> prefix ++ show i) [0..(b - 1)] ++ a, 0)

matchInsWithInNodes :: [IArg] -> [IAvailable] -> Maybe [(NodeId, EdgeIndex)]
matchInsWithInNodes a b = let (f, s) = inner (argsToStrings a (length b)) b in
  if null s then Just f else Nothing where
  inner :: [String] -> [IAvailable] -> ([(NodeId, EdgeIndex)], [IAvailable])
  inner [] rest = ([], rest)
  inner (inn:xs) inNodes =
    let (a, b) = span (\inNode -> Just inn /= expectedToConnectWith inNode) inNodes in
    case b of
      hit:rest -> let (c, d) = inner xs (a ++ rest) in ((fromNid hit, (fromName hit, inn)) : c, d)
      [] -> let (c, head:tail) = inner xs inNodes in ((fromNid head, (fromName head, inn)) : c, tail)

handle :: [ExpWithInfo] -> [IAvailable] -> [IAvailable] -> StateT IState (Either IError) (Maybe IAvailable)
handle ((EIn { exSeq = seq, exTo = to }, _) : rest) ins _ = do
  result <- handle seq [] []
  let (Just last) = result
  handle rest (last { expectedToConnectWith = to } : ins) []
handle ((EMiddle { exPrim = e }, info) : rest) ins _ = do
  expectedOuts <- handlePrimitive e ins
  if null rest
    then return $ Just (availableArgToAvailable $ head expectedOuts)
    else case sortExpectedOuts rest expectedOuts of
      Right sorted -> handle rest [] sorted
      Left info_ -> lift $ Left $ InternalError "sorting outputs error" [info, info_]
handle ((EOut { exSeq = seq }, _) : rest) _ (headOut : tailOuts) = do
  last <- handle seq [headOut] []
  if null rest
    then if null tailOuts
      then return last
      else return $ Just (head tailOuts)
    else handle rest [] tailOuts
handle ((EBi { exSeq = seq, inTo = to }, _) : rest) ins (headOut : tailOuts) = do
  result <- handle seq [headOut] []
  let (Just last) = result
  handle rest (last { expectedToConnectWith = to } : ins) tailOuts
handle ((_, info) : _) ins outs = do
  lift $ Left $ FailedHandlingExpError info ins outs
handle [] ins outs = lift $ Left $ InternalError "empty handling" []

handlePrimitive :: ExpWithInfo -> [IAvailable] -> StateT IState (Either IError) [IAvailableArg]
handlePrimitive (EVar { exVarName = varName }, info) externalIns = do
  let prim = getPrimitive varName
  unless (isJust prim) $ lift $ Left $ PrimitiveNotFoundError info varName externalIns
  let (Just p) = prim -- exhaustive because of the unless guard above
  let (inNames, outNames) = (pInns p, pOuts p)
  case matchInsWithInNodes inNames externalIns of
    Nothing -> lift $ Left $ ArgsMatchingError info inNames externalIns
    Just edges -> do
      counter <- next <$> get
      forM_ edges (\(nid, edgeIndex) -> do
        is <- get
        let newNodes = appendEdge nid edgeIndex counter (currentNodes is)
        modify (\is -> is { currentNodes = newNodes }))
      appendNode (IVar varName)
      nextCounter
      return $ map (\arg -> IAvailableArg counter arg Nothing) outNames
handlePrimitive (EImm { exImm = i, exGM = gm }, _) [] = do
  counter <- next <$> get
  modifyContext $ \ic -> ic { genNodes = counter : genNodes ic }
  appendNode (IImm i (case gm of
    EGMAlways -> IGMAlways
    EGMNormal -> IGMOnce))
  nextCounter
  return [IAvailableArg counter (IArg "result") Nothing]
handlePrimitive (EImm { exImm = i, exGM = EGMNormal }, info) externalIns = do
  case matchInsWithInNodes [IArg "arg0"] externalIns of
    Nothing -> lift $ Left $ ArgsMatchingError info [IArg "arg0"] externalIns
    Just edges -> do
      counter <- next <$> get
      forM_ edges (\(nid, edgeIndex) -> do
        is <- get
        let newNodes = appendEdge nid edgeIndex counter (currentNodes is)
        modify (\is -> is { currentNodes = newNodes }))
      appendNode (IImm i IGMPassive)
      nextCounter
      return [IAvailableArg counter (IArg "result") Nothing]
handlePrimitive (ERef { exRefName = ref }, info) externalIns = do
  is <- get
  let refList = refs is
  let isConsumer = length externalIns > 0 -- this check may be inappropriate
  case Map.lookup ref refList of
    Just (IRefState refnid count consumed) ->
      if isConsumer
        then
          if consumed then lift $ Left $ MultiDrivenRefError info ref
          else do
            case matchInsWithInNodes [IArg "refIn"] externalIns of
              Nothing -> lift $ Left $ ArgsMatchingError info [IArg "refIn"] externalIns
              Just edges -> do
                forM_ edges (\(nid, edgeIndex) -> do
                  is <- get
                  let newNodes = appendEdge nid edgeIndex refnid (currentNodes is)
                  modify (\is -> is { currentNodes = newNodes }))
                modify $ \is -> is { refs = Map.alter (\(Just irs) -> Just (irs { consumed = True })) ref refList }
                return []
        else do
            modify $ \is -> is { refs = Map.alter (\(Just irs) -> Just (irs { usedCount = count + 1 })) ref refList }
            return [IAvailableArg refnid (IArg $ "refOut" ++ show count) Nothing]
    Nothing -> do
      appendNode (IRef ref)
      if isConsumer
        then do
          modify $ \is -> is { refs = Map.insert ref (IRefState (next is) 0 True) refList }
          case matchInsWithInNodes [IArg "refIn"] externalIns of
            Nothing -> lift $ Left $ ArgsMatchingError info [IArg "refIn"] externalIns
            Just edges -> do
              forM_ edges (\(nid, edgeIndex) -> do
                is <- get
                let newNodes = appendEdge nid edgeIndex (next is) (currentNodes is)
                modify (\is -> is { currentNodes = newNodes }))
              nextCounter
              return []
        else do
          modify $ \is -> is { refs = Map.insert ref (IRefState (next is) 1 False) refList }
          counter <- next <$> get
          let newInNodes = [IAvailableArg counter (IArg "refOut0") Nothing]
          nextCounter
          return newInNodes
handlePrimitive (_, info) externalIns = lift $ Left $ FailedHandlingPrimitiveExpError info externalIns

defaultIState = IState Map.empty Map.empty 0 (IContext Map.empty [])

convert :: [ExpWithInfo] -> Either IError (Map.Map Int INode, IContext)
convert exps = case execStateT (handle exps [] []) defaultIState of
  Left err -> Left err
  Right is -> Right (currentNodes is, context is)

convertMultiline :: [(String, [ExpWithInfo])] -> Either String (Map.Map Int INode, IContext)
convertMultiline exps = case inner exps defaultIState of
  Left err -> Left err
  Right is -> Right (currentNodes is, context is)
  where
    inner :: [(String, [ExpWithInfo])] -> IState -> Either String IState
    inner [] s = Right s
    inner ((raw, x):xs) s = case execStateT (handle x [] []) s of
      Right newState -> inner xs newState
      Left err -> Left $ handleError err raw

infoToString :: ExpInfo -> String -> String
infoToString (ExpInfo from to) s = take (to - from) (drop from s) ++ " (from " ++ show from ++ " to " ++ show to ++ ")"
handleError :: IError -> String -> String
handleError (FailedHandlingExpError info ins outs) s =
  "Failed to handle the expression " ++ infoToString info s ++ ".\ninternal: " ++ show ins ++ "\n" ++ show outs
handleError (FailedHandlingPrimitiveExpError info ins) s =
  "Failed to handle the primitive expression " ++ infoToString info s ++ ".\ninternal: " ++ show ins
handleError (PrimitiveNotFoundError info name ins) s =
  "The primitive " ++ name ++ " is not found. " ++ infoToString info s ++"\ninternal: " ++ show ins
handleError (MultiDrivenRefError info name) s =
  "The ref " ++ name ++ " is multi-driven. " ++ infoToString info s
handleError (ArgsMatchingError info args ins) s =
  "The arguments of " ++ infoToString info s ++ " are not appropriate. Appropriate args: " ++ show args ++ "\ninternal: " ++ show ins
handleError (InternalError msg infos) s =
  "Sorry, an internal error occurred. " ++ msg ++ "\ndump: " ++ show (map (\i -> infoToString i s) infos)
