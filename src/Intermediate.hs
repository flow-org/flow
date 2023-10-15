{-# LANGUAGE LambdaCase #-}
module Intermediate where
import Control.Monad.State (StateT, MonadTrans (lift), execState, execStateT, evalState, gets)
import Types
import Control.Monad.State.Class (modify, get, put)
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)
import Control.Monad (when, mzero, unless, forM_, forM)
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, modifySTRef, readSTRef, writeSTRef)
import Data.Foldable (find)
import Primitives
import Data.Maybe (isJust, isNothing, mapMaybe)
import Control.Monad.Trans.State (State)
import Data.List (sortOn, sort)

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

headOfArgs :: IArgs -> Maybe String
headOfArgs x = case x of
  INormalArgs args -> inner args
  IWithSpread ([], spread) -> Just $ spread ++ "0"
  IWithSpread (args, _) -> inner args
  IWithOptionals (args, _) -> inner args
  where
    inner args = if null args then Nothing else Just $ head args

argsToStrings :: IArgs -> Int -> Maybe [String]
argsToStrings (INormalArgs args) n | length args == n = Just args
argsToStrings (IWithSpread (normals, spread)) n | length normals <= n =
  let spreads = n - length normals in
  Just $ normals ++ map (\i -> spread ++ show i) [0..(spreads - 1)]
argsToStrings (IWithOptionals (normals, optionals)) n | length normals <= n && n <= length normals + length optionals =
  Just $ normals ++ take (n - length normals) optionals
argsToStrings _ _ = Nothing

matchArgNamesWithArgs :: [String] -> [(t, Maybe String)] -> [(t, String)]
matchArgNamesWithArgs [] _ = []
matchArgNamesWithArgs (argName : rest) args = case splice (\(_, n) -> n == Just argName) args of
  (Just (id, _), restArgs) -> (id, argName) : matchArgNamesWithArgs rest restArgs
  _ -> let (Just (id, _), restArgs) = splice (\(_, n) -> isNothing n) args in (id, argName) : matchArgNamesWithArgs rest restArgs -- todo: error handling
  where
    splice :: (t -> Bool) -> [t] -> (Maybe t, [t])
    splice _ [] = (Nothing, [])
    splice f ar = let (a, b) = break f ar in
      if null b then (Nothing, ar) else (Just $ head b, a ++ tail b)

matchInsWithInNodes :: IArgs -> [IAvailable] -> Maybe [(NodeId, EdgeIndex)]
matchInsWithInNodes a b = do
  argNames <- argsToStrings a (length b)
  let argConverted = map (\(IAvailable id fromName maybeArg _) -> ((id, fromName), maybeArg)) (sort b)
  let matched = matchArgNamesWithArgs argNames argConverted
  return $ map (\((id, fromName), arg) -> (id, (fromName, arg))) matched

matchOutsWithOutNodes :: [ExpWithInfo] -> IAvailableArgs -> Either ExpInfo [IAvailable]
matchOutsWithOutNodes exps inNodes = do
  let outs = takeWhile (\out -> case out of
            (EOut {}, _) -> True
            (EBi {}, _) -> True
            _ -> False) exps
  let convertedOuts = mapMaybe (\(idx, out) -> case out of
            (exp@EOut { exArrow = Arrow { aFrom = f } }, _) -> Just ((idx, exp), f)
            (exp@EBi { exOutArrow = Arrow { aFrom = f } }, _) -> Just ((idx, exp), f)
            _ -> Nothing) $ zip (iterate (+ 1) 0) outs
  argNames <- maybe (Left ExpInfoNA) return $ argsToStrings (aFromArg inNodes) (length outs)
  let matched = matchArgNamesWithArgs argNames convertedOuts
  return $ map (\((_, _), name) -> IAvailable (aFromNid inNodes) name Nothing False) $ sortOn (\((idx, _), _) -> idx) matched

handle :: [ExpWithInfo] -> [IAvailable] -> [IAvailable] -> StateT IState (Either IError) (Maybe IAvailable)
handle ((EIn { exSeq = seq, exArrow = Arrow { aTo = to }, exPrioritized = prioritized }, _) : rest) ins _ = do
  result <- handle seq [] []
  let (Just last) = result
  handle rest (last { expectedToConnectWith = to, prioritized = prioritized } : ins) []
handle ((EMiddle { exPrim = e }, info) : rest) ins _ = do
  expectedOuts <- handlePrimitive e $ reverse ins
  if null rest
    then do
      let argName = headOfArgs (aFromArg expectedOuts)
      return $ argName >>= \name -> return $ IAvailable (aFromNid expectedOuts) name Nothing False
    else case matchOutsWithOutNodes rest expectedOuts of
      Right sorted -> handle rest [] sorted
      Left info_ -> lift $ Left $ InternalError "matching outputs error" [info, info_]
handle ((EOut { exSeq = seq, exArrow = Arrow { aTo = to } }, _) : rest) _ (headOut : tailOuts) = do
  last <- handle seq [headOut { expectedToConnectWith = to }] []
  if null rest
    then if null tailOuts
      then return last
      else return $ Just (head tailOuts)
    else handle rest [] tailOuts
handle ((EBi { exSeq = seq, exOutArrow = Arrow { aTo = outTo }, exInArrow = Arrow { aTo = inTo } }, _) : rest) ins (headOut : tailOuts) = do
  result <- handle seq [headOut { expectedToConnectWith = outTo }] []
  let (Just last) = result
  handle rest (last { expectedToConnectWith = inTo } : ins) tailOuts
handle ((_, info) : _) ins outs = do
  lift $ Left $ FailedHandlingExpError info ins outs
handle [] ins outs = lift $ Left $ InternalError "empty handling" []

handlePrimitive :: ExpWithInfo -> [IAvailable] -> StateT IState (Either IError) IAvailableArgs
handlePrimitive (EVar { exVarName = varName }, info) externalIns = do
  let prim = getPrimitive varName
  case prim of
    Just p -> do
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
          -- todo: temporary impl
          when (varName == "blink") $ modifyContext $ \ic -> ic { genNodes = counter : genNodes ic }
          nextCounter
          return $ IAvailableArgs counter outNames
    Nothing -> do
      s <- get
      let counter = next s
      let (Just (_, ins, outs)) = find (\(name, _, _) -> name == varName) (funcInfos s)
      let inArgs = INormalArgs ins -- todo: support other kinds of args
      case matchInsWithInNodes inArgs externalIns of
        Nothing -> lift $ Left $ ArgsMatchingError info inArgs externalIns
        Just edges -> do
          forM_ edges (\(nid, edgeIndex) -> do
            is <- get
            let newNodes = appendEdge nid edgeIndex counter (currentNodes is)
            modify (\is -> is { currentNodes = newNodes }))
          appendNode (IMount varName)
          modifyContext $ \ic -> ic { mountNodes = counter : mountNodes ic }
          nextCounter
          let outArgs = IAvailableArgs counter (INormalArgs outs)
          return outArgs
handlePrimitive (EImm { exImm = i }, _) [] = do
  counter <- next <$> get
  modifyContext $ \ic -> ic { genNodes = counter : genNodes ic }
  appendNode (IImm i IGMAlways)
  nextCounter
  return $ IAvailableArgs counter (INormalArgs ["result"])
handlePrimitive (EImm { exImm = i }, info) externalIns = do
  case matchInsWithInNodes (INormalArgs ["arg0"]) externalIns of
    Nothing -> lift $ Left $ ArgsMatchingError info (INormalArgs ["arg0"]) externalIns
    Just edges -> do
      counter <- next <$> get
      forM_ edges (\(nid, edgeIndex) -> do
        is <- get
        let newNodes = appendEdge nid edgeIndex counter (currentNodes is)
        modify (\is -> is { currentNodes = newNodes }))
      appendNode (IImm i IGMPassive)
      nextCounter
      return $ IAvailableArgs counter (INormalArgs ["result"])
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
            case matchInsWithInNodes (INormalArgs ["refIn"]) externalIns of
              Nothing -> lift $ Left $ ArgsMatchingError info (INormalArgs ["refIn"]) externalIns
              Just edges -> do
                forM_ edges (\(nid, edgeIndex) -> do
                  is <- get
                  let newNodes = appendEdge nid edgeIndex refnid (currentNodes is)
                  modify (\is -> is { currentNodes = newNodes }))
                modify $ \is -> is { refs = Map.alter (\(Just irs) -> Just (irs { consumed = True })) ref refList }
                return $ IAvailableArgs refnid (INormalArgs [])
        else do
            modify $ \is -> is { refs = Map.alter (\(Just irs) -> Just (irs { usedCount = count + 1 })) ref refList }
            return $ IAvailableArgs refnid (INormalArgs ["refOut" ++ show count])
    Nothing -> do
      appendNode (IRef ref)
      counter <- gets next
      modifyContext (\ic -> ic { addresses = Map.insert ref counter (addresses ic) })
      if isConsumer
        then do
          modify $ \is -> is { refs = Map.insert ref (IRefState (next is) 0 True) refList }
          case matchInsWithInNodes (INormalArgs ["refIn"]) externalIns of
            Nothing -> lift $ Left $ ArgsMatchingError info (INormalArgs ["refIn"]) externalIns
            Just edges -> do
              forM_ edges (\(nid, edgeIndex) -> do
                is <- get
                let newNodes = appendEdge nid edgeIndex (next is) (currentNodes is)
                modify (\is -> is { currentNodes = newNodes }))
              nextCounter
              return $ IAvailableArgs counter (INormalArgs [])
        else do
          modify $ \is -> is { refs = Map.insert ref (IRefState (next is) 1 False) refList }
          counter <- next <$> get
          let newInNodes = IAvailableArgs counter (INormalArgs ["refOut0"])
          nextCounter
          return newInNodes
handlePrimitive (_, info) externalIns = lift $ Left $ FailedHandlingPrimitiveExpError info externalIns

defaultIState = IState Map.empty Map.empty 0 (IContext Map.empty [] [] []) 

envToFuncInfos :: Env -> [FuncInfo]
envToFuncInfos env = map (\(name, (ins, outs, _)) -> (name, ins, outs)) $ Map.toList env

convertEnv :: Env -> Either String IEnv
convertEnv env = Map.fromList <$> forM (Map.toList env) (\(name, (ins, outs, expses)) ->
  let funcInfos = envToFuncInfos env in
  (\expses -> (name, (ins, outs, expses))) <$> convertMultiline (expses ++ convertIns ins ++ convertOuts outs) funcInfos)
  where
    convertIns :: [String] -> Expses
    convertIns = map (\inn -> ("", [
      (EMiddle (EVar "_mountIn", ExpInfoNA), ExpInfoNA),
      (EOut [(EMiddle (ERef inn Nothing, ExpInfoNA), ExpInfoNA)] arrowNormal, ExpInfoNA)]))
    convertOuts :: [String] -> Expses
    convertOuts = map (\out -> ("", [
      (EMiddle (ERef out Nothing, ExpInfoNA), ExpInfoNA),
      (EOut [(EMiddle (EVar "_mountOut", ExpInfoNA), ExpInfoNA)] arrowNormal, ExpInfoNA)]))

convertMultiline :: Expses -> [FuncInfo] -> Either String (Map.Map Int INode, IContext)
convertMultiline exps fns = case inner exps $ defaultIState fns of
  Left err -> Left err
  Right is -> Right (currentNodes is, context is)
  where
    inner :: Expses -> IState -> Either String IState
    inner [] s = Right s
    inner ((raw, x):xs) s = case execStateT (handle x [] []) s of
      Right newState -> inner xs newState
      Left err -> Left $ handleError err raw

infoToString :: ExpInfo -> String -> String
infoToString (ExpInfo from to) s = take (to - from) (drop from s) ++ " (from " ++ show from ++ " to " ++ show to ++ ")"
infoToString ExpInfoNA _ = "???"
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
