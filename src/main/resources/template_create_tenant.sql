CREATE ROLE myuniversity_mymodule PASSWORD 'myuniversity' NOSUPERUSER NOCREATEDB INHERIT LOGIN;

GRANT myuniversity_mymodule TO CURRENT_USER;

CREATE SCHEMA myuniversity_mymodule AUTHORIZATION myuniversity_mymodule;

CREATE EXTENSION IF NOT EXISTS "pgcrypto";

CREATE TABLE IF NOT EXISTS myuniversity_mymodule.permissions_users (_id UUID PRIMARY KEY DEFAULT gen_random_uuid(), jsonb JSONB NOT NULL);
GRANT ALL ON myuniversity_mymodule.permissions_users TO myuniversity_mymodule;

CREATE TABLE IF NOT EXISTS myuniversity_mymodule.permissions (_id UUID PRIMARY KEY DEFAULT gen_random_uuid(), jsonb JSONB NOT NULL);
GRANT ALL ON myuniversity_mymodule.permissions TO myuniversity_mymodule;
