CREATE TABLE accounts 
  ( id INTEGER NOT NULL UNIQUE PRIMARY KEY AUTOINCREMENT
  , balance_cents	INTEGER NOT NULL
  , user_id	INTEGER NOT NULL
  , user_type TEXT NOT NULL CHECK(user_type IN ('organization', 'human'))
  );

-----------
-- Seeds --
-----------

-- Note that data inconsistencies are possible here, but that's an artifact of
-- demonstrating the ability to connect different data sources that lack the
-- constraints possible in a single data source.
INSERT INTO accounts 
  ( balance_cents
  , user_id
  , user_type
  )
VALUES (100000 , 1 , 'human')
  , (200000 , 2 , 'human')
  , (500000 , 3 , 'human')
  , (50000000 , 4 , 'human')
  , (100000000000, 1, 'organization')
  ;