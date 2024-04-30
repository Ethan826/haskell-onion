module Core.Organization where

import Core.Human (HumanId)
import Core.Id (Id)
import Data.Text (Text)
import Data.Vector (Vector)

type OrganizationId = Id Organization

data Organization = Organization
  { organizationName :: Text
  , organizationId :: OrganizationId
  , organizationHumanIds :: Vector HumanId
  }
  deriving (Show, Eq)