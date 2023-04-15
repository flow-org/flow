module PipelineProcessor where

import Node
import Control.Monad.State (State, MonadState (get, put), modify, evalState, StateT, MonadTrans (lift))
import ApplyBaseParser (parseApplyBase)
import Debug.Trace (trace)
import Data.Maybe (catMaybes)

getNewRef :: StateT Int (Either String) Ref
getNewRef = do
  next <- get
  modify (+ 1)
  return $ DirectedRef $ "__" ++ show next

toAdditionalParam L L2R ref = AdditionalParam (Value $ Ref ref) In L True
toAdditionalParam L R2L ref = AdditionalParam (Value $ Ref ref) Out L True
toAdditionalParam R L2R ref = AdditionalParam (Value $ Ref ref) Out R True
toAdditionalParam R R2L ref = AdditionalParam (Value $ Ref ref) In R True

processPipeline :: [Node] -> StateT Int (Either String) [SemanticStructure]
processPipeline [single] = return []
processPipeline (ApplyBase [Value (Ref r1)]:Bind bindType:ApplyBase [Value (Ref r2)]:tail) =
  (RefConnect r1 bindType r2:) <$> processPipeline (ApplyBase [Value (Ref r2)]:tail)
processPipeline [ApplyBase [Value (Ref ref)], Bind (Direction dir), ApplyBase ns] = do
  trace (show (ref, dir, ns)) (return 0)
  processMachineRun ns (Just $ toAdditionalParam L dir ref) Nothing
processPipeline (ApplyBase [Value (Ref r1)]:Bind (Direction dir1):ApplyBase ns:Bind (Direction dir2):tail) = do
  newRef <- getNewRef
  head <- processMachineRun ns (Just $ toAdditionalParam L dir1 r1) (Just $ toAdditionalParam R dir2 newRef)
  (head ++) <$> processPipeline (ApplyBase [Value (Ref newRef)]:Bind (Direction dir2):tail)
processPipeline (ApplyBase ns:Bind (Direction dir):ApplyBase [Value (Ref ref)]:tail) = do
  trace (show (ns, dir, ref, tail)) $ return ()
  head <- processMachineRun ns (Just $ toAdditionalParam R dir ref) Nothing
  trace (show (ApplyBase [Value (Ref ref)]:tail)) $ return ()
  (head ++) <$> processPipeline (ApplyBase [Value (Ref ref)]:tail)
processPipeline (ApplyBase ns1:Bind (Direction dir):ApplyBase ns2:tail) = do
  newRef <- getNewRef
  processPipeline (ApplyBase ns1:Bind (Direction dir):ApplyBase [Value (Ref newRef)]:Bind (Direction dir):ApplyBase ns2:tail)

processMachineRun :: [Node] -> Maybe Node -> Maybe Node -> StateT Int (Either String) [SemanticStructure]
processMachineRun ns ext1 ext2 = do
  nextRef <- get
  let exts = catMaybes [ext1, ext2]
  let application = parseApplyBase (ns ++ exts) nextRef
  case application of
    Left memos -> lift $ Left $ show memos
    Right (refNum, sss) -> do
      put refNum
      return sss
