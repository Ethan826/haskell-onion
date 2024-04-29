{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core.Human where

import Core.Id (Id)
import Data.Text (Text)

type HumanId = Id Human

data Human = Human
  { humanName :: Text
  , humanId :: HumanId
  }
  deriving (Show, Eq)