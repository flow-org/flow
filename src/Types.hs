module Types where

import qualified Data.Map.Strict as Map
import Data.Graph.Inductive (Gr)

data ArrowMode = Normal | Buf | Notify deriving Show
data Arrow =
    AToLeft  ArrowMode (Maybe String) (Maybe String)
  | AToRight ArrowMode (Maybe String) (Maybe String) deriving Show
data GenMode = GMAlways | GMOnce | GMPassive deriving (Show, Eq)
data Value = VNum Int | VString String deriving Eq
instance Show Value where
  show (VNum i) = show i
  show (VString s) = s

data Exp =
    EMiddle { exPrim :: Exp }
  | EIn { exSeq :: [Exp], exFrom :: Maybe String, exTo :: Maybe String }
  | EOut { exSeq :: [Exp], exFrom :: Maybe String, exTo :: Maybe String }
  | EBi { exSeq :: [Exp], outFrom :: Maybe String, outTo :: Maybe String, inFrom :: Maybe String, inTo :: Maybe String }
  | EVar String
  | EImm Value GenMode
  | ERef String
  | EAddress String deriving (Show, Eq)
data Command = 
    CImRun [Exp]
  | CDecl [Exp]
  | CRun
  | CExit
  | CLoad String deriving Show

data Intermediate = IVar String | IImm Value GenMode | IRef String | IAddress String deriving Show
type NodeId = Int
type EdgeIndex = (String, String)
data INode = INode Intermediate [(EdgeIndex, NodeId)] deriving Show
data IRefState = IRefState { refNodeId :: NodeId, usedCount :: Int, consumed :: Bool } deriving Show
data IContext = IContext { addresses :: Map.Map String NodeId, genNodes :: [NodeId] } deriving Show

-- new
data IAvailable = IAvailable { fromNid :: NodeId, fromName :: String, expectedToConnectWith :: Maybe String } deriving Show

data IState = IState {
  -- maybe old
  currentNodes :: Map.Map NodeId INode,
  refs :: Map.Map String IRefState,
  next :: NodeId,
  context :: IContext
} deriving Show

data EvParticle = EvParticle { nodeId :: NodeId, edgeIndex :: EdgeIndex, particleValue :: Value } deriving Show
type HalfParticle = (String, Value)
newtype EvNodeState = EvNSControl Value deriving Show
data EvContext = EvContext { particles :: [EvParticle], time :: Int, nodeStates :: Map.Map NodeId EvNodeState } deriving Show
data EvState = EvState { graph :: Gr Intermediate EdgeIndex, ic :: IContext, evc :: EvContext }

data InOp = InFlush | InRemove String | InNoOp deriving Show
data OutOp = OutAppend [HalfParticle] | OutNoOp deriving Show

type FactoryValue = Maybe EvNodeState -> [HalfParticle] -> Int -> [HalfParticle] -> Int -> IO (Maybe (InOp, OutOp, Maybe EvNodeState))
