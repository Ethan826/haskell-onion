module Core.Id where

newtype Id a
  = Id {unId :: Int}
  deriving (Show, Eq, Ord)