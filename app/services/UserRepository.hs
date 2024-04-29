module Services.UserRepository where

import Core.Human (Human, HumanId)
import Core.Organization (OrganizationId)
import Core.User (User, UserId)

class (Monad m) => UserRepository m where
  -- createUser :: User -> m ()
  getUserById :: HumanId -> m (Maybe User)
  getAllHumansInOrganization :: OrganizationId -> m [Human]