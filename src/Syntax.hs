module Syntax where

data ArrowMode = Normal | Buf | Notify deriving Show
data Arrow =
    AToLeft  ArrowMode (Maybe String) (Maybe String)
  | AToRight ArrowMode (Maybe String) (Maybe String) deriving Show
data GenMode = GMAlways | GMOnce | GMPassive deriving Show
data Exp =
    EMiddle { exPrim :: Exp }
  | EIn { exSeq :: [Exp], exFrom :: Maybe String, exTo :: Maybe String }
  | EOut { exSeq :: [Exp], exFrom :: Maybe String, exTo :: Maybe String }
  | EBi { exSeq :: [Exp], inFrom :: Maybe String, inTo :: Maybe String, outFrom :: Maybe String, outTo :: Maybe String }
  | EVar String
  | ENum Int GenMode
  | ERef String
  | EAddress String deriving Show
data Command = CExp Exp | CDecl String Exp | CSpawn String Exp deriving Show
