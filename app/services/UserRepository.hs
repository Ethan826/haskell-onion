module Services.UserRepository where

import Core.Human (Human, HumanId)
import Core.Organization (OrganizationId)
import Core.User (User)

class (Monad m) => UserRepository m where
  createUser :: User -> m ()
  getUserById :: HumanId -> m (Maybe User)
  getAllHumansInOrganization :: OrganizationId -> m [Human]