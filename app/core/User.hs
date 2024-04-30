module Core.User where

import Core.Human (Human)
import Core.Id (Id)
import Core.Organization (Organization)

data UserId
  = HumanId (Id Human)
  | OrganizationId (Id Organization)
  deriving (Show, Eq)

data User
  = HumanUser Human
  | OrganizationUser Organization
  deriving (Show, Eq)