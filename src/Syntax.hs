module Syntax where

data ArrowMode = Normal | Buf | Notify deriving Show
data Arrow =
    AToLeft  ArrowMode (Maybe String) (Maybe String)
  | AToRight ArrowMode (Maybe String) (Maybe String) deriving Show
data GenMode = GMAlways | GMOnce | GMPassive deriving Show
data Exp = EMiddle [Exp] Exp [Exp] | EIn Exp (Maybe String) (Maybe String) | EOut Exp (Maybe String) (Maybe String) | EVar String | ENum Int GenMode | ERef String | EAddress String deriving Show
data Command = CExp Exp | CDecl String Exp | CSpawn String Exp deriving Show
