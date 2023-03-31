module Node where

data Node =
  String String |
  Number Int |
  Boolean Bool |
  Symbol String |
  ConnectorDef Node Node |
  Connection [Node] |
  ApplyBase [Node] |
  Parens Node |
  Application Node Node -- curried
  deriving (Show)