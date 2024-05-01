module Services.UserRepository where

import Core.Human (Human)
import Core.Organization (OrganizationId)
import Core.User (User, UserId)
import Data.Vector (Vector)

class (Monad m) => UserRepository m where
  getUserById :: UserId -> m (Maybe User)
  getAllHumansInOrganization :: OrganizationId -> m (Vector Human)