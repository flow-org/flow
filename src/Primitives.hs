module Primitives where

import Types
import Data.List (find, minimumBy)
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import Control.Monad.Cont (MonadTrans(lift), MonadPlus (mzero), guard)
import Debug.Trace

data Primitive = Primitive {
  pInns :: [IArg],
  pOuts :: [IArg],
  pEval :: FactoryValue }

foldL2FactoryValue :: (Value -> Value -> Value) -> Value -> FactoryValue
foldL2FactoryValue f z _ inParticles maxIn outParticles _ = do
  if not (null outParticles) || length inParticles < maxIn
  then return (InNoOp, OutNoOp, Nothing)
  else return (InFlush, OutAppend [("result", foldl (\a (_, x) -> f a x) z inParticles)], Nothing)
arithFoldL2FactoryValue :: (Int -> Int -> Int) -> Int -> FactoryValue
arithFoldL2FactoryValue f z = foldL2FactoryValue (\(VNum x) (VNum y) -> VNum (f x y)) (VNum z)
twoOpe2FactoryValue :: (Value -> Value -> Value) -> FactoryValue
twoOpe2FactoryValue f _ inParticles _ outParticles _ = do
  if not (null outParticles) || length inParticles < 2
  then return (InNoOp, OutNoOp, Nothing)
  else do
    let [(_, x), (_, y)] = inParticles
    return (InFlush, OutAppend [("result", f x y)], Nothing)
arith2Ope2FactoryValue :: (Int -> Int -> Int) -> FactoryValue
arith2Ope2FactoryValue f = twoOpe2FactoryValue (\(VNum x) (VNum y) -> VNum (f x y))

doOutput :: (Show a) => a -> MaybeT IO ()
doOutput v = do
  lift $ putStr $ show v
  c <- lift getChar
  lift $ putStrLn ""
  guard (c == ';')
  return ()

primitives :: Map.Map String Primitive
primitives = Map.fromList [
    ("+", Primitive [ISpread "arg"] [IArg "result"] $ arithFoldL2FactoryValue (+) 0),
    ("-", Primitive [IArg "arg0", IArg "arg1"] [IArg "result"] $ arith2Ope2FactoryValue (-)),
    ("*", Primitive [ISpread "arg"] [IArg "result"] $ arithFoldL2FactoryValue (*) 1),
    ("/", Primitive [IArg "arg0", IArg "arg1"] [IArg "result"] $ arith2Ope2FactoryValue div),
    ("==", Primitive [IArg "arg0", IArg "arg1"] [IArg "result"] $ arith2Ope2FactoryValue (\a b -> if a == b then 1 else 0)),
    ("++", Primitive [ISpread "arg"] [IArg "result"] $ foldL2FactoryValue (\(VString x) (VString y) -> VString (x ++ y)) $ VString ""),
    ("output", Primitive [IArg "arg0"] [] $ \_ [(_, v)] _ _ _ -> do
      doOutput v
      return (InFlush, OutNoOp, Nothing)),
    ("trace", Primitive [IArg "arg0"] [IArg "result"] $ \_ inParticles _ outParticles _ -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, Nothing)
      else do
        let [(_, v)] = inParticles
        doOutput v
        return (InFlush, OutAppend [("result", v)], Nothing)),
    ("merge", Primitive [ISpread "arg"] [IArg "result"] $ \_ inParticles _ outParticles _ -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, Nothing)
      else do
        let (inName, v) = minimumBy (\(n1, _) (n2, _) -> compare n1 n2) inParticles
        return (InRemove inName, OutAppend [("result", v)], Nothing)),
    ("copy", Primitive [IArg "arg0"] [ISpread "copy"] $ \_ inParticles@[(_, v)] _ outParticles maxOuts -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, Nothing)
      else do
        return (InFlush, OutAppend $ map (\i -> ("copy" ++ show i, v)) [0..maxOuts - 1], Nothing)),
    ("if", Primitive [IArg "condition"] [IArg "then", IArg "else"] $ \_ inParticles@[(_, v)] _ outParticles _ -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, Nothing)
      else do
        case v of
          VNum 0 -> return (InFlush, OutAppend [("else", v)], Nothing)
          _      -> return (InFlush, OutAppend [("then", v)], Nothing)),
    ("control", Primitive [IArg "en", IArg "value"] [IArg "result"] $ \ns inParticles _ outParticles _ -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, ns)
      else do
        let v = find (\(name, _) -> name == "value") inParticles
        let newNs = (case v of
              Just (_, v) -> Just $ EvNSControl v
              _ -> ns)
        let en = find (\(name, _) -> name == "en") inParticles
        case (newNs, en) of
          (Just (EvNSControl v), Just _) -> return (InFlush, OutAppend [("result", v)], Nothing)
          _ -> return (InRemove "value", OutNoOp, newNs))
  ]

getPrimitive :: String -> Maybe Primitive
getPrimitive name = Map.lookup name primitives
