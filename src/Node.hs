module Node where
import Control.Monad (liftM)
import Control.Monad.Cont (ap)

data BindType = Direction Direction | Bi deriving (Show, Eq)
data Direction = R2L | L2R deriving (Show, Eq)
data ParamType = In | Out deriving (Show, Eq)
data Position = R | L deriving (Show, Eq)
data Ref = GeneralRef String | DirectedRef String deriving (Show, Eq)
data Value =
  String String |
  Number Int |
  Boolean Bool |
  Symbol String |
  Ref Ref deriving (Show, Eq)
data Node =
  Value Value |
  AdditionalParam [Node] ParamType Position Bool |
  ApplyBase [Node] |
  Machine [Node] [Node] |
  Function [Node] Node |
  -- end: exp
  MachineParam ParamType [Node] |
  -- start: statement
  Def Node Node |
  Pipeline [Node] |
  Bind BindType |
  Bypass Node Node |
  -- end: statement
  -- start: semantic analysis
  Application Node [Node] | -- curried
  SectionApplyFromRight Node Node |
  -- end: semantic analysis
  EOF
  deriving (Show, Eq)

data ExternalParam =
  ExternalParam Direction ParamType Ref deriving (Show, Eq)

data MachineParamDef = NoRef ParamType String | WithRef ParamType Ref deriving (Show, Eq)
data MachineRef = MRWithDef SemanticStructure | MRWithRef String deriving (Show, Eq)

data SemanticStructure =
  RefConnect Ref BindType Ref |
  MachineDef [MachineParamDef] [SemanticStructure] |
  SSValue Value |
  MachineRun MachineRef [ApplyParam] deriving (Show, Eq)

data ApplyParam = Param Direction ParamType Int Value deriving (Show, Eq)

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

instance Functor (ContT r m) where
    fmap = liftM

instance Applicative (ContT r m) where
    pure = return
    (<*>) = ap

instance Monad (ContT r m) where
    return a = ContT $ \f -> f a
    ContT a >>= f = ContT $ \g -> a $ \a' -> runContT (f a') g

callCC :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
callCC f = ContT $ \g ->
    runContT (f $ ContT . const . g) g

getCC :: ContT r m (ContT r m a)
getCC = callCC $ \exit ->
    let a = exit a
     in return a
