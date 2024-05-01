#!/bin/bash

psql -f ./users-down-v1.sql -d experiment_v1
psql -f ./users-down-v2.sql -d experiment_v2
psql -f ./users-up-v1.sql -d experiment_v1
psql -f ./users-up-v2.sql -d experiment_v2

sqlite3 accounts.db < ./accounts-down.sql
sqlite3 accounts.db < ./accounts-up.sql