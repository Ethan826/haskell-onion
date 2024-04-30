CREATE TYPE user_discriminator AS ENUM ('human', 'organization');

CREATE TABLE 
  users
  ( id SERIAL NOT NULL PRIMARY KEY
  , name TEXT NOT NULL
  , discriminator user_discriminator NOT NULL
  );

CREATE TABLE
  organization_humans
  ( organization_id INT NOT NULL REFERENCES users(id) ON DELETE CASCADE
  , human_id INT NOT NULL REFERENCES users(id) ON DELETE CASCADE
  ,  PRIMARY KEY 
     ( organization_id
     , human_id
     )
  );

CREATE OR REPLACE FUNCTION check_discriminator()
RETURNS TRIGGER AS $$
BEGIN
  IF NOT EXISTS
    (SELECT 1
     FROM users
     WHERE id = NEW.human_id 
       AND discriminator = 'human')
  THEN
    RAISE EXCEPTION 'human_id must correspond to a user with discriminator human';
  END IF;

  IF NOT EXISTS 
    (SELECT 1 
     FROM users
     WHERE id = NEW.organization_id 
       AND discriminator = 'organization')
  THEN
    RAISE EXCEPTION 'organization_id must correspond to a user with discriminator organization';
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trigger_check_discriminator
BEFORE INSERT OR UPDATE ON organization_humans
FOR EACH ROW EXECUTE PROCEDURE check_discriminator();

CREATE INDEX IF NOT EXISTS idx_users_discriminator_id ON users(discriminator, id);
CREATE INDEX IF NOT EXISTS idx_organization_humans_organization_id ON organization_humans(organization_id);
CREATE INDEX IF NOT EXISTS idx_organization_humans_human_id ON organization_humans(human_id);

-----------
-- Seeds --
-----------

INSERT INTO users (name, discriminator) 
  VALUES ('Ethan Kent', 'human')
  , ('Brandon Shiaw', 'human')
  , ('Cam Kidman', 'human')
  , ('Corrine Olson', 'human')
  , ('Ethan Glasser-Camp', 'human')
  , ('Ivo Marjavonić', 'human')
  , ('Jacob Nave', 'human')
  , ('Laurent Huberdeau', 'human')
  , ('Matt Long', 'human')
  ;

INSERT INTO users (name, discriminator)
  VALUES ('Mercury', 'organization')
  , ('Engineers', 'organization')
  , ('Managers', 'organization')
  , ('Product People', 'organization')
  ;
  
WITH
  mercury_organization_id AS
    (SELECT id
     FROM users 
     WHERE name = 'Mercury' 
       AND discriminator = 'organization' LIMIT 1)
  , mercury_human_ids AS (
    SELECT id FROM users WHERE name IN 
      (
      , 'Ethan Kent'
      , 'Brandon Shiaw'
      , 'Cam Kidman'
      , 'Corrine Olson'
      , 'Ethan Glasser-Camp'
      , 'Ivo Marjavonić'
      , 'Jacob Nave',
      , 'Laurent Huberdeau'
      , 'Matt Long'
    ) AND discriminator = 'human'
  )
INSERT INTO organization_humans
  (organization_id
  , human_id
  )
SELECT mo.id AS organization_id
       , mh.id AS human_id
FROM mercury_organization_id mo
CROSS JOIN mercury_human_ids mh;

WITH
  engineers_organization_id AS
    (SELECT id 
     FROM users
     WHERE name = 'Engineers' 
       AND discriminator = 'organization' LIMIT 1)
  , engineer_human_ids AS 
    (SELECT id 
     FROM users 
     WHERE name IN
     ( 'Ethan Kent'
     , 'Cam Kidman'
     , 'Ethan Glasser-Camp'
     , 'Ivo Marjavonić'
     , 'Jacob Nave'
     , 'Laurent Huberdeau'
     , 'Matt Long'
     )
       AND discriminator = 'human'
  )
INSERT INTO organization_humans (organization_id, human_id)
SELECT eo.id AS organization_id, eh.id AS human_id
FROM engineers_organization_id eo
CROSS JOIN engineer_human_ids eh;

WITH
  product_organization_id AS
    (SELECT id 
     FROM users
     WHERE name = 'Product People' 
       AND discriminator = 'organization' LIMIT 1)
  , product_human_ids AS
    (SELECT id
     FROM users
     WHERE name IN ('Brandon Shiaw') 
       AND discriminator = 'human')
INSERT INTO organization_humans (organization_id, human_id)
SELECT po.id AS organization_id, ph.id AS human_id
FROM product_organization_id po
CROSS JOIN product_human_ids ph;

WITH
  management_organization_id AS
    (SELECT id 
     FROM users
     WHERE name = 'Managers' 
       AND discriminator = 'organization' LIMIT 1)
  , management_human_ids AS
    (SELECT id
     FROM users 
     WHERE name IN ('Corrine Olson') 
       AND discriminator = 'human')
INSERT INTO organization_humans (organization_id, human_id)
SELECT mo.id AS organization_id, mh.id AS human_id
FROM management_organization_id mo
CROSS JOIN management_human_ids mh;