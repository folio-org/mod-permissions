CREATE ROLE myuniversity PASSWORD 'myuniversity' NOSUPERUSER NOCREATEDB INHERIT LOGIN;

CREATE SCHEMA myuniversity AUTHORIZATION myuniversity;

CREATE TABLE myuniversity.permissions_users (_id SERIAL PRIMARY KEY, jsonb JSONB NOT NULL);
GRANT ALL ON myuniversity.permissions_users TO myuniversity;
GRANT ALL ON myuniversity.permissions_users__id_seq TO myuniversity;

CREATE TABLE myuniversity.permissions (_id SERIAL PRIMARY KEY, jsonb JSONB NOT NULL);
GRANT ALL ON myuniversity.permissions TO myuniversity;
GRANT ALL ON myuniversity.permissions__id_seq TO myuniversity;
