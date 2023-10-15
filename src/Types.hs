module Types where

import qualified Data.Map.Strict as Map
import Data.Graph.Inductive (Gr)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.List (intercalate)

data IGenMode = IGMAlways | IGMPassive deriving (Show, Eq)
data Value =
    VNum Int
  | VString String
  | VBool Bool
  | VList [Value]
  | VPair Value Value
  | VGhost deriving Eq
instance Show Value where
  show (VNum i) = show i
  show (VString s) = s
  show (VBool b) = show b
  show (VList l) = "[" ++ intercalate ", " (map show l) ++ "]"
  show (VPair a b) = "(" ++ show a ++ ", " ++ show b ++ ")"
  show VGhost = "<ghost>"

passGhost VGhost = VGhost
passGhost v = v
isGhost VGhost = True
isGhost _ = False

data ArrowType = ANormal | AControl deriving (Show, Eq)
data Arrow = Arrow { aType :: ArrowType, aFrom :: Maybe String, aTo :: Maybe String } deriving (Show, Eq)
arrowNormal = Arrow ANormal Nothing Nothing

data ExpInfo = ExpInfo { fromPos :: Int, toPos :: Int } | ExpInfoNA deriving (Show, Eq)

data Exp =
    EMiddle { exPrim :: ExpWithInfo }
  | EIn { exSeq :: [ExpWithInfo], exArrow :: Arrow, exPrioritized :: Bool }
  | EOut { exSeq :: [ExpWithInfo], exArrow :: Arrow }
  | EBi { exSeq :: [ExpWithInfo], exOutArrow :: Arrow, exInArrow :: Arrow }
  | EComposed { exOpeName :: String, exArgs :: [ExpWithInfo] }
  | ESectionRightHand { exOpeName :: String, exPrim :: ExpWithInfo }
  | ESectionLeftHand { exOpeName :: String, exPrim :: ExpWithInfo }
  | EVar { exVarName :: String }
  | EImm { exImm :: Value }
  | ERef { exRefName :: String, exDefaultValue :: Maybe ExpWithInfo }
  deriving (Show, Eq)
type ExpWithInfo = (Exp, ExpInfo)
data Command = 
    CDecl [ExpWithInfo]
  | CName { cName :: String, cIns :: [String], cOuts :: [String], cContent :: [[ExpWithInfo]] }
  | CExit
  | CLoad String deriving Show

newtype SError = SyntaxSugarHandlingError ExpInfo deriving Show

type Expses = [(String, [ExpWithInfo])]
type Env = Map.Map String ([String], [String], Expses)
type IEnv = Map.Map String ([String], [String], (Map.Map Int INode, IContext))

data IArgs = INormalArgs [String] | IWithSpread ([String], String) | IWithOptionals ([String], [String]) deriving Show

data Intermediate =
    IVar String
  | IImm Value IGenMode
  | IMount String
  | IRef String
  | IAddress String deriving Show
type NodeId = Int
type EdgeIndex = (String, String)
data INode = INode Intermediate [(EdgeIndex, NodeId)] deriving Show
data IRefState = IRefState { refNodeId :: NodeId, usedCount :: Int, consumed :: Bool } deriving Show
data IContext = IContext {
  addresses :: Map.Map String NodeId,
  genNodes :: [NodeId],
  mountNodes :: [NodeId],
  unmountNodes :: [NodeId] } deriving Show

data IAvailable = IAvailable { fromNid :: NodeId, fromName :: String, expectedToConnectWith :: Maybe String, prioritized :: Bool } deriving (Show, Eq)
instance Ord IAvailable where
  compare (IAvailable { prioritized = a }) (IAvailable { prioritized = b }) = compare b a -- prioritize True
data IAvailableArgs = IAvailableArgs { aFromNid :: NodeId, aFromArg :: IArgs } deriving Show

type FuncInfo = (String, [String], [String])

data IState = IState {
  -- maybe old
  currentNodes :: Map.Map NodeId INode,
  refs :: Map.Map String IRefState,
  next :: NodeId,
  context :: IContext,
  funcInfos :: [FuncInfo]
} deriving Show

data IError =
    FailedHandlingExpError { info :: ExpInfo, ins :: [IAvailable], outs :: [IAvailable] }
  | FailedHandlingPrimitiveExpError { info :: ExpInfo, ins :: [IAvailable] }
  -- todo: this error will be deprecated (custom var)
  | PrimitiveNotFoundError { info :: ExpInfo, name :: String, ins :: [IAvailable] }
  | MultiDrivenRefError { info :: ExpInfo, name :: String }
  | ArgsMatchingError { info :: ExpInfo, args :: IArgs, ins :: [IAvailable] }
  | InternalError { msg :: String, infos :: [ExpInfo] } deriving Show

data EvParticle = EvParticle { nodeId :: (GraphInstanceId, NodeId), edgeIndex :: EdgeIndex, particleValue :: Value } deriving Show
data HalfParticle = HalfParticle String Value deriving (Show, Eq)
instance Ord HalfParticle where
  compare (HalfParticle a _) (HalfParticle b _) = compare a b

type EvEnv = Map.Map String ([String], [String], (Gr Intermediate EdgeIndex, IContext))
type GraphInstanceId = Int

data EvNodeState =
    EvNSControl Value 
  | EvOnce 
  | EvMount GraphInstanceId
  | EvMap [Value]
  | EvMerge Int
  | EvBlink Int deriving Show
data EvContext = EvContext {
    particles :: [EvParticle],
    time :: Int,
    nodeStates :: Map.Map (GraphInstanceId, NodeId) EvNodeState,
    graphInstances :: Map.Map GraphInstanceId String,
    nextGraphInstanceId :: GraphInstanceId
  } deriving Show
data EvState = EvState { env :: EvEnv, evc :: EvContext } deriving Show

data InOp = InFlush | InRemove String | InNoOp deriving (Show, Eq)
data OutOp = OutAppend [HalfParticle] | OutNoOp deriving (Show, Eq)

type FactoryValue = Maybe EvNodeState -> [HalfParticle] -> Int -> [HalfParticle] -> Int -> MaybeT IO (InOp, OutOp, Maybe EvNodeState)
