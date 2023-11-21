module Primitives where

import Types
import Data.List (find, minimum, sort)
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import Control.Monad.Trans.Class (lift)
import Control.Monad (guard, unless, mzero)
import Debug.Trace
import Control.Applicative (Alternative(empty))
import Data.Maybe (isJust)
import GHC.GHCi.Helpers (flushAll)

data Primitive = Primitive {
  pInns :: IArgs,
  pOuts :: IArgs,
  pEval :: FactoryValue }

foldL2FactoryValue :: (Value -> Value -> Value) -> Value -> FactoryValue
foldL2FactoryValue f z _ inParticles maxIn outParticles _ = do
  if not (null outParticles) || length inParticles < maxIn
  then return (InNoOp, OutNoOp, Nothing)
  else
    if any (\(HalfParticle _ v) -> isGhost v) inParticles
      then return (InFlush, OutAppend [HalfParticle "result" VGhost], Nothing)
      else return (InFlush, OutAppend [HalfParticle "result" $ foldl (\a (HalfParticle _ x) -> f a x) z (sort inParticles)], Nothing)
arithFoldL2FactoryValue :: (Int -> Int -> Int) -> Int -> FactoryValue
arithFoldL2FactoryValue f z = foldL2FactoryValue (\(VNum x) (VNum y) -> VNum (f x y)) (VNum z)
twoOpe2FactoryValue :: (Value -> Value -> Value) -> FactoryValue
twoOpe2FactoryValue f _ inParticles _ outParticles _ = do
  if not (null outParticles) || length inParticles < 2
  then return (InNoOp, OutNoOp, Nothing)
  else do
    let [HalfParticle _ x, HalfParticle _ y] = sort inParticles
    if any isGhost [x, y]
      then return (InFlush, OutAppend [HalfParticle "result" VGhost], Nothing)
      else return (InFlush, OutAppend [HalfParticle "result" $ f x y], Nothing)
arith2Ope2FactoryValue :: (Int -> Int -> Int) -> FactoryValue
arith2Ope2FactoryValue f = twoOpe2FactoryValue (\(VNum x) (VNum y) -> VNum (f x y))
logic2Ope2FactoryValue :: (Value -> Value -> Bool) -> FactoryValue
logic2Ope2FactoryValue f = twoOpe2FactoryValue (\x y -> VBool (f x y))
arithLogic2Ope2FactoryValue :: (Int -> Int -> Bool) -> FactoryValue
arithLogic2Ope2FactoryValue f = twoOpe2FactoryValue (\(VNum x) (VNum y) -> VBool (f x y))
foldL12FactoryValue :: (Value -> Value -> Value) -> FactoryValue
foldL12FactoryValue f _ inParticles maxIn outParticles _ = do
  if not (null outParticles) || length inParticles < maxIn
  then return (InNoOp, OutNoOp, Nothing)
  else
    if any (\(HalfParticle _ v) -> isGhost v) inParticles
      then return (InFlush, OutAppend [HalfParticle "result" VGhost], Nothing)
      else let ar = sort inParticles in
        let (HalfParticle _ h) = head ar in
          return (InFlush, OutAppend [HalfParticle "result" $ foldl (\a (HalfParticle _ x) -> f a x) h (tail ar)], Nothing)

doOutput :: (Show a) => a -> MaybeT IO ()
doOutput v = do
  lift $ putStr $ show v
  lift flushAll
  c <- lift getLine
  guard (c == "")
  return ()

normalResult = INormalArgs ["result"]
argSpread = IWithSpread ([], "arg")
arg1Arity = INormalArgs ["arg0"]
arg2Arity = INormalArgs ["arg0", "arg1"]
argEmpty = INormalArgs []

primitives :: Map.Map String Primitive
primitives = Map.fromList [
    ("+", Primitive argSpread normalResult $ arithFoldL2FactoryValue (+) 0),
    ("-", Primitive arg2Arity normalResult $ arith2Ope2FactoryValue (-)),
    ("*", Primitive argSpread normalResult $ arithFoldL2FactoryValue (*) 1),
    ("/", Primitive arg2Arity normalResult $ arith2Ope2FactoryValue div),
    ("%", Primitive arg2Arity normalResult $ arith2Ope2FactoryValue mod),
    ("==", Primitive arg2Arity normalResult $ logic2Ope2FactoryValue (==)),
    ("!=", Primitive arg2Arity normalResult $ logic2Ope2FactoryValue (/=)),
    (">=", Primitive arg2Arity normalResult $ arithLogic2Ope2FactoryValue (>=)),
    ("<=", Primitive arg2Arity normalResult $ arithLogic2Ope2FactoryValue (<=)),
    (">", Primitive arg2Arity normalResult $ arithLogic2Ope2FactoryValue (>)),
    ("<", Primitive arg2Arity normalResult $ arithLogic2Ope2FactoryValue (<)),
    ("++", Primitive argSpread normalResult $
      foldL12FactoryValue (\x y -> case (x, y) of
                                    (VString x, VString y) -> VString (x ++ y)
                                    (VList x, VList y) -> VList (x ++ y))),
    ("&&", Primitive argSpread normalResult $ foldL2FactoryValue (\(VBool x) (VBool y) -> VBool (x && y)) $ VBool True),
    ("||", Primitive argSpread normalResult $ foldL2FactoryValue (\(VBool x) (VBool y) -> VBool (x || y)) $ VBool False),
    ("not", Primitive arg1Arity normalResult $ \_ [HalfParticle _ v] _ _ _ -> do
      if v == VBool True
        then return (InFlush, OutAppend [HalfParticle "result" (VBool False)], Nothing)
        else return (InFlush, OutAppend [HalfParticle "result" (VBool True)], Nothing)),
    ("output", Primitive arg1Arity argEmpty $ \_ [HalfParticle _ v] _ _ _ -> do
      unless (isGhost v) (doOutput v)
      return (InFlush, OutNoOp, Nothing)),
    ("output!", Primitive arg1Arity argEmpty $ \_ [HalfParticle _ v] _ _ _ -> do
      doOutput v
      return (InFlush, OutNoOp, Nothing)),
    ("trace", Primitive arg1Arity normalResult $ \_ inParticles _ outParticles _ -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, Nothing)
      else do
        let [HalfParticle _ v] = inParticles
        unless (isGhost v) $ doOutput v
        return (InFlush, OutAppend [HalfParticle "result" v], Nothing)),
    ("trace!", Primitive arg1Arity normalResult $ \_ inParticles _ outParticles _ -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, Nothing)
      else do
        let [HalfParticle _ v] = inParticles
        doOutput v
        return (InFlush, OutAppend [HalfParticle "result" v], Nothing)),
    ("input", Primitive (INormalArgs ["request"]) normalResult $ \_ inParticles _ outParticles _ -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, Nothing)
      else do
        lift $ putStr "Input: "
        lift flushAll
        c <- lift getLine
        return (InFlush, OutAppend [HalfParticle "result" (VString c)], Nothing)),
    ("merge", Primitive argSpread normalResult $ \ns inParticles inns outParticles _ -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, ns)
      else if null inParticles then return (InNoOp, OutNoOp, Nothing)
      else do
        let suitableArgIndex = (case ns of
              Just (EvMerge i) -> i
              _ -> inns)
        let inNameToNum x = read (drop 3 x) :: Int
        let targetParticle = find (\(HalfParticle inName _) -> inNameToNum inName > suitableArgIndex) (sort inParticles)
        let (HalfParticle inName v) = case targetParticle of
              Just v -> v
              _ -> minimum inParticles
        return (InRemove inName, OutAppend [HalfParticle "result" v], Just (EvMerge $ inNameToNum inName))),
    ("copy", Primitive arg1Arity (IWithSpread ([], "copy")) $ \_ inParticles@[HalfParticle _ v] _ outParticles maxOuts -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, Nothing)
      else do
        return (InFlush, OutAppend $ map (\i -> HalfParticle ("copy" ++ show i) v) [0..maxOuts - 1], Nothing)),
    ("if", Primitive (IWithOptionals (["condition"], ["value"])) (IWithOptionals (["then"], ["else"])) $ \_ inParticles ins outParticles _ -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, Nothing)
      else do
        let c = find (\(HalfParticle name _) -> name == "condition") inParticles
        let vParticle = find (\(HalfParticle name _) -> name == "value") inParticles
        let v =
              if ins == 2
                then vParticle
                else (case c of
                  Just (HalfParticle _ v) -> Just (HalfParticle "value" v)
                  _ -> Nothing)
        case (c, v) of
          (Just (HalfParticle _ VGhost), Just _) -> return (InFlush, OutAppend [HalfParticle "then" VGhost], Nothing)
          (Just (HalfParticle _ (VBool True)), Just (HalfParticle _ v)) -> return (InFlush, OutAppend [HalfParticle "then" v], Nothing)
          (Just (HalfParticle _ _), Just (HalfParticle _ v)) -> return (InFlush, OutAppend [HalfParticle "else" v], Nothing)
          _ -> return (InNoOp, OutNoOp, Nothing)),
    -- deprecated
    ("control", Primitive (INormalArgs ["en", "value"]) normalResult $ \ns inParticles _ outParticles _ -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, ns)
      else do
        let v = find (\(HalfParticle name _) -> name == "value") inParticles
        if v == Just (HalfParticle "value" VGhost)
        then return (InFlush, OutAppend [HalfParticle "result" VGhost], Nothing)
        else do
          let newNs = (case v of
                Just (HalfParticle _ v) -> Just $ EvNSControl v
                _ -> ns)
          let en = find (\(HalfParticle name _) -> name == "en") inParticles
          case (newNs, en) of
            (Just (EvNSControl v), Just _) -> return (InFlush, OutAppend [HalfParticle "result" v], Nothing)
            _ -> return (InRemove "value", OutNoOp, newNs)),
    -- deprecated
    ("shot", Primitive (INormalArgs ["en", "value"]) normalResult $ \ns inParticles _ outParticles _ -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, ns)
      else do
        let v = find (\(HalfParticle name _) -> name == "value") inParticles
        if v == Just (HalfParticle "value" VGhost)
        then return (InFlush, OutAppend [HalfParticle "result" VGhost], Nothing)
        else do
          let newNs = (case v of
                Just (HalfParticle _ v) -> Just $ EvNSControl v
                _ -> ns)
          let en = find (\(HalfParticle name _) -> name == "en") inParticles
          case (newNs, en) of
            (Just (EvNSControl v), Just _) -> return (InFlush, OutAppend [HalfParticle "result" v], newNs)
            _ -> return (InRemove "value", OutNoOp, newNs)),
    ("id", Primitive arg1Arity normalResult $ \_ inParticles@[HalfParticle _ v] _ outParticles maxOuts -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, Nothing)
      else do
        return (InFlush, OutAppend [HalfParticle "result" v], Nothing)),
    ("once", Primitive arg1Arity normalResult $ \ns inParticles _ outParticles maxOuts ->
      if null inParticles
        then return (InNoOp, OutNoOp, ns)
        else do
          let [HalfParticle _ v] = inParticles
          if isGhost v then
            if null outParticles then return (InFlush, OutAppend [HalfParticle "result" VGhost], ns)
              else return (InNoOp, OutNoOp, ns)
          else case ns of
            Just EvOnce -> return (InNoOp, OutNoOp, ns)
            _ -> do
              if not $ null outParticles
              then return (InNoOp, OutNoOp, ns)
              else do
                return (InFlush, OutAppend [HalfParticle "result" v], Just EvOnce)),
    ("exit", Primitive arg1Arity argEmpty $ \_ _ _ _ _ -> empty),
    ("_mountIn", Primitive argEmpty (INormalArgs ["_mountIn"]) $ \ns _ _ _ _ -> return (InNoOp, OutNoOp, ns)),
    ("_mountOut", Primitive (INormalArgs ["_mountOut"]) argEmpty $ \ns _ _ _ _ -> return (InNoOp, OutNoOp, ns)),
    ("map", Primitive arg1Arity normalResult $ \ns inParticles _ outParticles _ ->
      if not (null outParticles)
      then return (InNoOp, OutNoOp, ns)
      else case ns of
        (Just (EvMap (x : rest))) -> do
          return (InNoOp, OutAppend [HalfParticle "result" x], Just $ EvMap rest)
        (Just (EvMap [])) -> do
          return (InNoOp, OutAppend [HalfParticle "result" VGhost], Nothing)
        _ ->
          if null inParticles then return (InNoOp, OutNoOp, Nothing)
          else do
            let (HalfParticle _ v) = head inParticles
            if isGhost v then return (InFlush, OutAppend [HalfParticle "result" v], ns)
            else let (VList l) = v in return (InFlush, OutNoOp, Just $ EvMap l)),
    ("unmap", Primitive arg1Arity normalResult $ \ns inParticles _ outParticles _ -> do
      if null inParticles then return (InNoOp, OutNoOp, ns)
      else do
        let (HalfParticle _ v) = head inParticles
        let list = case ns of
              Just (EvMap x) -> x
              _ -> []
        if isGhost v
        then if null outParticles then return (InFlush, OutAppend [HalfParticle "result" $ VList $ reverse list], Nothing)
            else return (InNoOp, OutNoOp, ns)
        else return (InFlush, OutNoOp, Just (EvMap (v : list)))),
    ("pair", Primitive (INormalArgs ["fst", "snd"]) normalResult $ \_ inParticles _ outParticles _ -> do
      if not (null outParticles) || length inParticles /= 2
      then return (InNoOp, OutNoOp, Nothing)
      else do
        let [HalfParticle _ a, HalfParticle _ b] = sort inParticles
        return (InFlush, OutAppend [HalfParticle "result" $ VPair a b], Nothing)),
    ("unpair", Primitive arg1Arity (INormalArgs ["fst", "snd"]) $ \_ inParticles _ outParticles _ -> do
      if not (null outParticles)
      then return (InNoOp, OutNoOp, Nothing)
      else do
        let [HalfParticle _ (VPair a b)] = inParticles
        return (InFlush, OutAppend [HalfParticle "fst" a, HalfParticle "snd" b], Nothing)),
    ("blink", Primitive argEmpty normalResult $ \ns _ _ outParticles _ -> do
      if not (null outParticles) then return (InNoOp, OutNoOp, ns)
      else case ns of
        Just (EvBlink 1) -> return (InNoOp, OutAppend [HalfParticle "result" $ VNum 1], Just (EvBlink 0))
        _ -> return (InNoOp, OutAppend [HalfParticle "result" $ VNum 0], Just (EvBlink 1))),
    ("int", Primitive arg1Arity normalResult $ \_ inParticles _ outParticles _ -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, Nothing)
      else do
        let [HalfParticle _ v] = inParticles
        let newValue =
              case v of
                VString s -> VNum (read s)
                VNum i -> VNum i
                VBool True -> VNum 1
                VBool False -> VNum 0
                VGhost -> VGhost
                _ -> VNum undefined
        return (InFlush, OutAppend [HalfParticle "result" newValue], Nothing)),
    ("str", Primitive arg1Arity normalResult $ \_ inParticles _ outParticles _ -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, Nothing)
      else do
        let [HalfParticle _ v] = inParticles
        let newValue =
              case v of
                VGhost -> VGhost
                _ -> VString (show v)
        return (InFlush, OutAppend [HalfParticle "result" newValue], Nothing)),
    ("bool", Primitive arg1Arity normalResult $ \_ inParticles _ outParticles _ -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, Nothing)
      else do
        let [HalfParticle _ v] = inParticles
        let newValue =
              case v of
                VString "" -> VBool False
                VString _ -> VBool True
                VNum 0 -> VBool False
                VNum _ -> VBool True
                VBool b -> VBool b
                VGhost -> VGhost
                VList [] -> VBool False
                VList _ -> VBool True
                VPair _ _ -> VBool True
        return (InFlush, OutAppend [HalfParticle "result" newValue], Nothing))
  ]

getPrimitive :: String -> Maybe Primitive
getPrimitive name = Map.lookup name primitives
