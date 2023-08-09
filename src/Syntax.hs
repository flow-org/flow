module Syntax where

data ArrowMode = Normal | Buf | Notify deriving Show
data Arrow =
    AToLeft  ArrowMode (Maybe String) (Maybe String)
  | AToRight ArrowMode (Maybe String) (Maybe String) deriving Show
data GenMode = GMAlways | GMOnce | GMPassive deriving (Show, Eq)
data Exp =
    EMiddle { exPrim :: Exp }
  | EIn { exSeq :: [Exp], exFrom :: Maybe String, exTo :: Maybe String }
  | EOut { exSeq :: [Exp], exFrom :: Maybe String, exTo :: Maybe String }
  | EBi { exSeq :: [Exp], outFrom :: Maybe String, outTo :: Maybe String, inFrom :: Maybe String, inTo :: Maybe String }
  | EVar String
  | ENum Int GenMode
  | ERef String
  | EAddress String deriving (Show, Eq)
data Command = 
    CImRun [Exp]
  | CDecl [Exp]
  | CRun
  | CExit deriving Show
