module Core.Organization where

import Core.Human (HumanId)
import Core.Id (Id)
import Data.Text (Text)

type OrganizationId = Id Organization

data Organization = Organization
  { organizationName :: Text
  , organizationId :: OrganizationId
  , organizationHumanIds :: [HumanId]
  }
  deriving (Show, Eq)