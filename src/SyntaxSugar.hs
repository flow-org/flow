{-# LANGUAGE LambdaCase #-}
module SyntaxSugar where
import Types
import Control.Monad.State (StateT, evalStateT, MonadTrans (lift), modify, gets)
import Control.Monad ((>=>))
import Debug.Trace

newtype SugarState = SugarState { refId :: Int } deriving Show

handleSugarRoot :: [ExpWithInfo] -> Either String [ExpWithInfo]
handleSugarRoot exps = case evalStateT (handleSugarIn exps) (SugarState 0) of
  Right exps -> Right exps
  Left err -> Left $ show err

newRef :: StateT SugarState (Either SError) String
newRef = do
  id <- gets refId
  modify (\s -> s { refId = id + 1 })
  return $ "_sug_" ++ show id

type Handler = StateT SugarState (Either SError) [ExpWithInfo]

handleSugarIn :: [ExpWithInfo] -> Handler
handleSugarIn ((EMiddle inner, info) : rest) = (++) <$> handleMiddle inner <*> handleSugarAll rest
handleSugarIn x = handleSugarAll x -- todo

spanMiddle = span (\case
  (EIn {}, _) -> True
  (EMiddle {}, _) -> True
  _ -> False)

handleSugarAll :: [ExpWithInfo] -> Handler
handleSugarAll [] = return []
handleSugarAll ((EMiddle inner, info) : rest) = (++) <$> handleMiddle inner <*> handleSugarAll rest
handleSugarAll ((e@EIn { exSeq = seq, exArrow = Arrow { aType = AControl } }, info) : rest) = do
  -- todo consider exArrow.aFrom
  inner <- handleSugarIn seq
  let (untilMiddle, newRest) = spanMiddle rest
  newUntilMiddle <- handleSugarAll untilMiddle
  ((newUntilMiddle ++ [
      (EBi [
        (EIn inner (arrowNormal { aTo = Just "en" }) False, info),
        (EMiddle (EVar "control", info), info)
      ] arrowNormal arrowNormal, info),
      (EMiddle (EVar "id", info), info)
    ]) ++) <$> handleSugarAll newRest
handleSugarAll ((e@EIn { exSeq = seq }, info) : rest) = do
  inner <- handleSugarIn seq
  ((e { exSeq = inner }, info) :) <$> handleSugarAll rest
handleSugarAll ((e@EOut { exSeq = seq, exArrow = Arrow { aType = AControl, aFrom = afrom } }, info) : rest) = do
  let (untilMiddle, newSeq) = spanMiddle seq
  newUntilMiddle <- handleSugarAll untilMiddle
  inner <- handleSugarAll newSeq
  ((EOut ([
      (EIn newUntilMiddle arrowNormal False, info),
      (EMiddle (EVar "control", info), info)
    ] ++ inner) (Arrow ANormal afrom (Just "en")), info) :) <$> handleSugarAll rest
handleSugarAll ((e@EOut { exSeq = seq }, info) : rest) = do
  inner <- handleSugarAll seq
  ((e { exSeq = inner }, info) :) <$> handleSugarAll rest
handleSugarAll ((e@EBi { exSeq = seq, exOutArrow = Arrow { aType = AControl, aFrom = afrom }, exInArrow = Arrow { aType = AControl, aFrom = afrom2 } }, info) : rest) = do
  let (untilMiddle, newSeq) = spanMiddle seq
  newUntilMiddle <- handleSugarAll untilMiddle
  inner <- handleSugarAll newSeq
  let (untilMiddle2, newRest) = spanMiddle rest
  newUntilMiddle2 <- handleSugarAll untilMiddle2
  (((EBi ([
      (EIn newUntilMiddle arrowNormal False, info),
      (EMiddle (EVar "control", info), info)
    ] ++ inner) (Arrow ANormal afrom (Just "en")) (Arrow ANormal afrom2 (Just "en")), info) : [
      (EIn untilMiddle2 arrowNormal False, info),
      (EMiddle (EVar "control", info), info),
      (EBi [(EVar "id", info)] arrowNormal arrowNormal, info)
    ]) ++) <$> handleSugarAll newRest
handleSugarAll ((e@EBi { exSeq = seq, exOutArrow = Arrow { aType = AControl, aFrom = afrom }, exInArrow = inArrow }, info) : rest) = do
  let (untilMiddle, newSeq) = spanMiddle seq
  newUntilMiddle <- handleSugarAll untilMiddle
  inner <- handleSugarAll newSeq
  ((EBi ([
      (EIn newUntilMiddle arrowNormal False, info),
      (EMiddle (EVar "control", info), info)
    ] ++ inner) (Arrow ANormal afrom (Just "en")) inArrow, info) :) <$> handleSugarAll rest
handleSugarAll ((e@EBi { exSeq = seq, exOutArrow = outArrow, exInArrow = Arrow { aType = AControl, aFrom = afrom } }, info) : rest) = do
  let (untilMiddle, newRest) = spanMiddle rest
  newUntilMiddle <- handleSugarAll untilMiddle
  (((EBi seq outArrow (Arrow ANormal afrom (Just "en")), info) : [
      (EIn untilMiddle arrowNormal False, info),
      (EMiddle (EVar "control", info), info),
      (EBi [(EVar "id", info)] arrowNormal arrowNormal, info)
    ]) ++) <$> handleSugarAll newRest
handleSugarAll ((e@EBi { exSeq = seq }, info) : rest) = do
  inner <- handleSugarAll seq
  ((e { exSeq = inner }, info) :) <$> handleSugarAll rest
handleSugarAll ((_, info) : _) = lift $ Left (SyntaxSugarHandlingError info)

handleMiddle :: ExpWithInfo -> Handler
handleMiddle (EComposed opeName args, info) = do
  ins <- mapM (handleMiddle >=> (\exps -> return  (EIn exps arrowNormal True, info))) args
  return (ins ++ [(EMiddle (EVar opeName, info), info)])
handleMiddle (ESectionRightHand opeName inner, info) = do
  newInner <- handleMiddle inner
  return [
    (EIn newInner (arrowNormal { aTo = Just "arg1" }) True, info),
    (EMiddle (EVar opeName, info), info)]
handleMiddle (ESectionLeftHand opeName inner, info) = do
  newInner <- handleMiddle inner
  return [
    (EIn newInner (arrowNormal { aTo = Just "arg0" }) True, info),
    (EMiddle (EVar opeName, info), info)]
handleMiddle (ERef name (Just exp), info) = do
  newExp <- handleMiddle exp
  return [
    (EIn [(EMiddle (ERef name Nothing, info), info)] arrowNormal True, info),
    (EIn (newExp ++ [(EOut [(EMiddle (EVar "once", info), info)] arrowNormal, info)]) arrowNormal True, info),
    (EMiddle (EVar "merge", info), info)]
handleMiddle (x, info) = return [(EMiddle (x, info), info)]
