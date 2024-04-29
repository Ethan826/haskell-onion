CREATE TABLE 
  humans 
  ( id SERIAL NOT NULL PRIMARY KEY
  , name TEXT NOT NULL
  );

CREATE TABLE 
  organizations
  ( id SERIAL NOT NULL PRIMARY KEY
  , name TEXT NOT NULL
  );

CREATE TABLE
  organization_humans
  ( organization_id INT NOT NULL REFERENCES organizations(id) ON DELETE CASCADE
  , human_id INT NOT NULL REFERENCES humans(id) ON DELETE CASCADE
  ,  PRIMARY KEY 
     ( organization_id
     , human_id
     )
  );

CREATE INDEX idx_humans_name ON humans(name);
CREATE INDEX idx_organizations_name ON organizations(name);
CREATE INDEX idx_organization_humans_org_id ON organization_humans(organization_id);
CREATE INDEX idx_organization_humans_human_id ON organization_humans(human_id);

-----------
-- Seeds --
-----------

INSERT INTO humans (name) 
  VALUES ('Ethan Kent')
  , ('Brandon Shiaw')
  , ('Cam Kidman')
  , ('Corrine Olson')
  , ('Ethan Glasser-Camp')
  , ('Ivo Marjavonić')
  , ('Jacob Nave')
  , ('Laurent Huberdeau')
  , ('Matt Long')
  ;

INSERT INTO organizations (name)
  VALUES ('Mercury')
  , ('Engineers')
  , ('Managers')
  , ('Product People')
  ;

WITH
  mercury_organization_id AS
    (SELECT id FROM organizations WHERE name = 'Mercury' LIMIT 1)
  , mercury_ids AS
    (SELECT id FROM humans WHERE name IN
       ( 'Ethan Kent'
       , 'Brandon Shiaw'
       , 'Cam Kidman'
       , 'Corrine Olson'
       , 'Ethan Glasser-Camp'
       , 'Ivo Marjavonić'
       , 'Jacob Nave'
       , 'Laurent Huberdeau'
       , 'Matt Long'
       )
    )
INSERT INTO organization_humans
  ( organization_id
  , human_id
  )
SELECT mo.id AS organization_id
       , m.id AS human_id
FROM mercury_organization_id mo
CROSS JOIN mercury_ids m;

WITH
  engineers_organization_id AS
    (SELECT id FROM organizations WHERE name = 'Engineers' LIMIT 1)
  , engineer_ids AS
    (SELECT id FROM humans WHERE name IN
       ( 'Ethan Kent'
       , 'Cam Kidman'
       , 'Ethan Glasser-Camp'
       , 'Ivo Marjavonić'
       , 'Jacob Nave'
       , 'Laurent Huberdeau'
       , 'Matt Long'
       )
    )
INSERT INTO organization_humans
  ( organization_id
  , human_id
  )
SELECT eo.id AS organization_id
       , e.id AS human_id
FROM engineers_organization_id eo
CROSS JOIN engineer_ids e;

WITH
  product_organization_id AS
    (SELECT id FROM organizations WHERE name = 'Product People' LIMIT 1)
  , product_ids AS
    (SELECT id FROM humans WHERE name IN ('Brandon Shiaw'))
INSERT INTO organization_humans
  ( organization_id
  , human_id
  )
SELECT po.id AS organization_id
       , p.id AS human_id
FROM product_organization_id po
CROSS JOIN product_ids p;

WITH
  management_organization_id AS
    (SELECT id FROM organizations WHERE name = 'Managers' LIMIT 1)
  , management_ids AS
    (SELECT id FROM humans WHERE name IN ('Corrine Olson'))
INSERT INTO organization_humans
  ( organization_id
  , human_id
  )
SELECT mo.id AS organization_id
       , p.id AS human_id
FROM management_organization_id mo
CROSS JOIN management_ids p;