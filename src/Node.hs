module Node where

data BindType = R2L | L2R | Bi deriving (Show, Eq)
data Node =
  String String |
  Number Int |
  Boolean Bool |
  Symbol String |
  ConnectorDef Node Node |
  Connection [Node] |
  Bind BindType |
  ApplyBase [Node] |
  Application Node Node | -- curried
  SectionApplyFromRight Node Node |
  EOF
  deriving (Show, Eq)