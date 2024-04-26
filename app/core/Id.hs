module Core.Id where

newtype Id a
  = Id Int
  deriving (Show, Eq, Ord)