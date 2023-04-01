module Node where

data Node =
  String String |
  Number Int |
  Boolean Bool |
  Symbol String |
  ConnectorDef Node Node |
  Connection [Node] |
  ApplyBase [Node] |
  Application Node Node | -- curried
  SectionApplyFromRight Node Node |
  EOF
  deriving (Show, Eq)