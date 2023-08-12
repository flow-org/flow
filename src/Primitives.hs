module Primitives where

import Types
import Data.List (find)
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import Control.Monad.Cont (MonadTrans(lift))

data Primitive = Primitive {
  pInns :: [String],
  pOuts :: [String],
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
  if c == ';'
    then return ()
    else MaybeT $ return Nothing

primitives :: Map.Map String Primitive
primitives = Map.fromList [
    ("+", Primitive ["a", "b"] ["result"] $ arithFoldL2FactoryValue (+) 0),
    ("-", Primitive ["a", "b"] ["result"] $ arith2Ope2FactoryValue (-)),
    ("*", Primitive ["a", "b"] ["result"] $ arithFoldL2FactoryValue (*) 1),
    ("/", Primitive ["a", "b"] ["result"] $ arith2Ope2FactoryValue div),
    ("==", Primitive ["a", "b"] ["result"] $ arith2Ope2FactoryValue (\a b -> if a == b then 1 else 0)),
    ("++", Primitive ["a", "b"] ["result"] $ foldL2FactoryValue (\(VString x) (VString y) -> VString (x ++ y)) $ VString ""),
    ("output", Primitive ["a"] [] $ \_ [(_, v)] _ _ _ -> do
      doOutput v
      return (InFlush, OutNoOp, Nothing)),
    ("trace", Primitive ["a"] ["result"] $ \_ inParticles _ outParticles _ -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, Nothing)
      else do
        let [(_, v)] = inParticles
        doOutput v
        return (InFlush, OutAppend [("result", v)], Nothing)),
    ("merge", Primitive ["a", "b"] ["result"] $ \_ inParticles _ outParticles _ -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, Nothing)
      else do
        let (inName, v):rest = inParticles
        return (InRemove inName, OutAppend [("result", v)], Nothing)),
    ("copy", Primitive ["a"] ["copy0", "copy1"] $ \_ inParticles@[(_, v)] _ outParticles maxOuts -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, Nothing)
      else do
        return (InFlush, OutAppend $ map (\i -> ("copy" ++ show i, v)) [0..maxOuts - 1], Nothing)),
    ("if", Primitive ["condition"] ["then", "else"] $ \_ inParticles@[(_, v)] _ outParticles _ -> do
      if not $ null outParticles
      then return (InNoOp, OutNoOp, Nothing)
      else do
        case v of
          VNum 0 -> return (InFlush, OutAppend [("else", v)], Nothing)
          _      -> return (InFlush, OutAppend [("then", v)], Nothing)),
    ("control", Primitive ["en", "value"] ["result"] $ \ns inParticles _ outParticles _ -> do
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
