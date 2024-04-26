module Core.User where

import Core.Human (Human)
import Core.Organization (Organization)

data User
  = HumanUser Human
  | OrganizationUser Organization
  deriving (Show, Eq)