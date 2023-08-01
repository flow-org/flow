module Syntax where

data ArrowMode = Normal | Buf | Notify deriving Show
data Arrow = AToLeft ArrowMode | AToRight ArrowMode deriving Show
data Exp = EVar String | EConnect Arrow Exp Exp | ENum Int | ERef String | EAddress String deriving Show
data Command = CExp Exp | CDecl String Exp | CSpawn String Exp deriving Show
