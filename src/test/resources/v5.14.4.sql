--
-- PostgreSQL database cluster dump
--

SET default_transaction_read_only = off;

SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;

--
-- Roles
--

CREATE ROLE kiwi_mod_permissions;
ALTER ROLE kiwi_mod_permissions WITH SUPERUSER INHERIT NOCREATEROLE NOCREATEDB LOGIN NOREPLICATION NOBYPASSRLS PASSWORD 'md5f9b7ed31047f5d0f57bf6a152924b3ea';
--
-- User Configurations
--

--
-- User Config "kiwi_mod_permissions"
--

ALTER ROLE kiwi_mod_permissions SET search_path TO '$user';


--
-- PostgreSQL database dump
--

-- Dumped from database version 12.8
-- Dumped by pg_dump version 12.10 (Ubuntu 12.10-0ubuntu0.20.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: kiwi_mod_permissions; Type: SCHEMA; Schema: -; Owner: kiwi_mod_permissions
--

CREATE SCHEMA kiwi_mod_permissions;


ALTER SCHEMA kiwi_mod_permissions OWNER TO kiwi_mod_permissions;

--
-- Name: pg_trgm; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pg_trgm WITH SCHEMA public;


--
-- Name: EXTENSION pg_trgm; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION pg_trgm IS 'text similarity measurement and index searching based on trigrams';


--
-- Name: unaccent; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS unaccent WITH SCHEMA public;


--
-- Name: EXTENSION unaccent; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION unaccent IS 'text search dictionary that removes accents';


--
-- Name: concat_array_object(jsonb); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.concat_array_object(jsonb_array jsonb) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE
    AS $_$
  SELECT string_agg(value::text, ' ') FROM jsonb_array_elements_text($1);
$_$;



--
-- Name: concat_array_object_values(jsonb, text); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.concat_array_object_values(jsonb_array jsonb, field text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE
    AS $_$
  SELECT string_agg(value->>$2, ' ') FROM jsonb_array_elements($1);
$_$;



--
-- Name: concat_array_object_values(jsonb, text, text, text); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.concat_array_object_values(jsonb_array jsonb, field text, filterkey text, filtervalue text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE
    AS $_$
SELECT string_agg(value->>$2, ' ') FROM jsonb_array_elements($1) WHERE value->>$3 = $4;
$_$;



--
-- Name: concat_space_sql(text[]); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.concat_space_sql(VARIADIC text[]) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE
    AS $_$ select concat_ws(' ', VARIADIC $1);
$_$;



--
-- Name: count_estimate(text); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.count_estimate(query text) RETURNS bigint
    LANGUAGE plpgsql STABLE STRICT
    AS $$
DECLARE
  count bigint;
  est_count bigint;
  q text;
BEGIN
  est_count = kiwi_mod_permissions.count_estimate_smart2(1000, 1000, query);
  IF est_count > 4*1000 THEN
    RETURN est_count;
  END IF;
  q = 'SELECT COUNT(*) FROM (' || query || ' LIMIT 1000) x';
  EXECUTE q INTO count;
  IF count < 1000 THEN
    RETURN count;
  END IF;
  IF est_count < 1000 THEN
    RETURN 1000;
  END IF;
  RETURN est_count;
END;
$$;



--
-- Name: count_estimate_default(text); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.count_estimate_default(query text) RETURNS bigint
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $$
DECLARE
  rows bigint;
  q text;
BEGIN
  q = 'SELECT COUNT(*) FROM (' || query || ' LIMIT 1000) x';
  EXECUTE q INTO rows;
  IF rows < 1000 THEN
    return rows;
  END IF;
  rows = kiwi_mod_permissions.count_estimate_smart2(1000, 1000, query);
  IF rows < 1000 THEN
    return 1000;
  END IF;
  RETURN rows;
END;
$$;



--
-- Name: count_estimate_smart2(bigint, bigint, text); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.count_estimate_smart2(rows bigint, lim bigint, query text) RETURNS bigint
    LANGUAGE plpgsql STRICT
    AS $$
DECLARE
  rec   record;
  cnt bigint;
BEGIN
  IF rows = lim THEN
      FOR rec IN EXECUTE 'EXPLAIN ' || query LOOP
        cnt := substring(rec."QUERY PLAN" FROM ' rows=([[:digit:]]+)');
        EXIT WHEN cnt IS NOT NULL;
      END LOOP;
      RETURN cnt;
  END IF;
  RETURN rows;
END;
$$;



--
-- Name: f_unaccent(text); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.f_unaccent(text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE
    AS $_$
        SELECT public.unaccent('public.unaccent', $1)  -- schema-qualify function and dictionary
      $_$;



--
-- Name: first_array_object_value(jsonb, text, text, text); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.first_array_object_value(jsonb_array jsonb, field text, filterkey text, filtervalue text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE
    AS $_$
SELECT value->>$2 FROM jsonb_array_elements($1) WHERE value->>$3 = $4 LIMIT 1;
$_$;



--
-- Name: get_tsvector(text); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.get_tsvector(text) RETURNS tsvector
    LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE
    AS $_$
  SELECT to_tsvector('simple', translate($1, '&', ','));
$_$;



--
-- Name: next_uuid(uuid); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.next_uuid(uuid) RETURNS uuid
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uuid text;
  digit text;
BEGIN
  uuid = $1;
  FOR i IN REVERSE 36..1 LOOP
    digit := substring(uuid from i for 1);
    -- skip minus, version byte M and variant byte N
    CONTINUE WHEN digit = '-' OR i = 15 OR i = 20;
    CASE digit
      WHEN '0' THEN digit := '1';
      WHEN '1' THEN digit := '2';
      WHEN '2' THEN digit := '3';
      WHEN '3' THEN digit := '4';
      WHEN '4' THEN digit := '5';
      WHEN '5' THEN digit := '6';
      WHEN '6' THEN digit := '7';
      WHEN '7' THEN digit := '8';
      WHEN '8' THEN digit := '9';
      WHEN '9' THEN digit := 'a';
      WHEN 'a' THEN digit := 'b';
      WHEN 'b' THEN digit := 'c';
      WHEN 'c' THEN digit := 'd';
      WHEN 'd' THEN digit := 'e';
      WHEN 'e' THEN digit := 'f';
      WHEN 'f' THEN digit := '0';
      ELSE NULL;
    END CASE;
    uuid = overlay(uuid placing digit from i);
    EXIT WHEN digit <> '0';
  END LOOP;
  RETURN uuid;
END;
$_$;



--
-- Name: normalize_digits(text); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.normalize_digits(text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE
    AS $_$
  SELECT    translate((regexp_match($1, '^([0-9 \t-]*(?:\*[ \t]*)?)(.*)'))[1], E' \t-', '')
         || CASE WHEN (regexp_match($1, '^([0-9 \t-]*(?:\*[ \t]*)?)(.*)'))[1] = '' THEN ''
                 WHEN (regexp_match($1, '^([0-9 \t-]*(?:\*[ \t]*)?)(.*)'))[2] = '' THEN ''
                 ELSE ' '
            END
         || (regexp_match($1, '^([0-9 \t-]*(?:\*[ \t]*)?)(.*)'))[2];
$_$;



--
-- Name: permissions_set_md(); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.permissions_set_md() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  input text;
  createdDate timestamp;
BEGIN
  input = NEW.jsonb->'metadata'->>'createdDate';
  IF input IS NULL THEN
    RETURN NEW;
  END IF;
  -- time stamp without time zone?
  IF (input::timestamp::timestamptz = input::timestamptz) THEN
    -- createdDate already has no time zone, normalize using ::timestamp
    createdDate = input::timestamp;
  ELSE
    -- createdDate has a time zone string
    -- normalize using ::timestamptz, convert to '+00' time zone and remove time zone string
    createdDate = input::timestamptz AT TIME ZONE '+00';
  END IF;
  NEW.jsonb = jsonb_set(NEW.jsonb, '{metadata,createdDate}', to_jsonb(createdDate));
  NEW.creation_date = createdDate;
  NEW.created_by = NEW.jsonb->'metadata'->>'createdByUserId';
  RETURN NEW;
END;
$$;



--
-- Name: permissions_users_set_md(); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.permissions_users_set_md() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  input text;
  createdDate timestamp;
BEGIN
  input = NEW.jsonb->'metadata'->>'createdDate';
  IF input IS NULL THEN
    RETURN NEW;
  END IF;
  -- time stamp without time zone?
  IF (input::timestamp::timestamptz = input::timestamptz) THEN
    -- createdDate already has no time zone, normalize using ::timestamp
    createdDate = input::timestamp;
  ELSE
    -- createdDate has a time zone string
    -- normalize using ::timestamptz, convert to '+00' time zone and remove time zone string
    createdDate = input::timestamptz AT TIME ZONE '+00';
  END IF;
  NEW.jsonb = jsonb_set(NEW.jsonb, '{metadata,createdDate}', to_jsonb(createdDate));
  NEW.creation_date = createdDate;
  NEW.created_by = NEW.jsonb->'metadata'->>'createdByUserId';
  RETURN NEW;
END;
$$;



--
-- Name: rmb_internal_index(text, text, text, text); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.rmb_internal_index(atable text, aname text, tops text, newdef text) RETURNS void
    LANGUAGE plpgsql
    AS $_$
DECLARE
  olddef text;
  namep CONSTANT text = concat(aname, '_p');
  prepareddef text;
BEGIN
  IF tops = 'DELETE' THEN
    -- use case insensitive %s, not case sensitive %I
    -- no SQL injection because the names are hard-coded in schema.json
    EXECUTE format('DROP INDEX IF EXISTS %s', aname);
    EXECUTE 'DELETE FROM kiwi_mod_permissions.rmb_internal_index WHERE name = $1' USING aname;
    RETURN;
  END IF;
  SELECT def INTO olddef      FROM kiwi_mod_permissions.rmb_internal_index WHERE name = aname;
  SELECT def INTO prepareddef FROM kiwi_mod_permissions.rmb_internal_index WHERE name = namep;
  prepareddef = replace(prepareddef, concat(' ', namep, ' ON '), concat(' ', aname, ' ON '));
  IF prepareddef = newdef THEN
    EXECUTE format('DROP INDEX IF EXISTS %s', aname);
    EXECUTE format('ALTER INDEX IF EXISTS %s RENAME TO %s', namep, aname);
    EXECUTE 'DELETE FROM rmb_internal_index WHERE name = $1' USING namep;
    EXECUTE 'INSERT INTO rmb_internal_analyze VALUES ($1)' USING atable;
  ELSIF olddef IS DISTINCT FROM newdef THEN
    EXECUTE format('DROP INDEX IF EXISTS %s', aname);
    EXECUTE newdef;
    EXECUTE 'INSERT INTO rmb_internal_analyze VALUES ($1)' USING atable;
  END IF;
  EXECUTE 'INSERT INTO kiwi_mod_permissions.rmb_internal_index VALUES ($1, $2, FALSE) '
          'ON CONFLICT (name) DO UPDATE SET def = EXCLUDED.def, remove = EXCLUDED.remove' USING aname, newdef;
END
$_$;



--
-- Name: set_id_in_jsonb(); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.set_id_in_jsonb() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  NEW.jsonb = jsonb_set(NEW.jsonb, '{id}', to_jsonb(NEW.id));
  RETURN NEW;
END;
$$;



--
-- Name: set_permissions_md_json(); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.set_permissions_md_json() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  if NEW.creation_date IS NULL then
    RETURN NEW;
  end if;

  NEW.jsonb = jsonb_set(NEW.jsonb, '{metadata,createdDate}', to_jsonb(NEW.creation_date));
  if NEW.created_by IS NULL then
    NEW.jsonb = NEW.jsonb #- '{metadata,createdByUserId}';
  else
    NEW.jsonb = jsonb_set(NEW.jsonb, '{metadata,createdByUserId}', to_jsonb(NEW.created_by));
  end if;
  RETURN NEW;
END;
$$;



--
-- Name: set_permissions_users_md_json(); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.set_permissions_users_md_json() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  if NEW.creation_date IS NULL then
    RETURN NEW;
  end if;

  NEW.jsonb = jsonb_set(NEW.jsonb, '{metadata,createdDate}', to_jsonb(NEW.creation_date));
  if NEW.created_by IS NULL then
    NEW.jsonb = NEW.jsonb #- '{metadata,createdByUserId}';
  else
    NEW.jsonb = jsonb_set(NEW.jsonb, '{metadata,createdByUserId}', to_jsonb(NEW.created_by));
  end if;
  RETURN NEW;
END;
$$;



--
-- Name: tsquery_and(text); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.tsquery_and(text) RETURNS tsquery
    LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE
    AS $_$
  SELECT to_tsquery('simple', string_agg(CASE WHEN length(v) = 0 OR v = '*' THEN ''
                                              WHEN right(v, 1) = '*' THEN '''' || left(v, -1) || ''':*'
                                              ELSE '''' || v || '''' END,
                                         '&'))
  FROM (SELECT regexp_split_to_table(translate($1, '&''', ',,'), ' +')) AS x(v);
$_$;



--
-- Name: tsquery_or(text); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.tsquery_or(text) RETURNS tsquery
    LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE
    AS $_$
  SELECT replace(kiwi_mod_permissions.tsquery_and($1)::text, '&', '|')::tsquery;
$_$;



--
-- Name: tsquery_phrase(text); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.tsquery_phrase(text) RETURNS tsquery
    LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE
    AS $_$
  SELECT replace(kiwi_mod_permissions.tsquery_and($1)::text, '&', '<->')::tsquery;
$_$;



--
-- Name: upsert(text, uuid, anyelement); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.upsert(text, uuid, anyelement) RETURNS uuid
    LANGUAGE plpgsql
    AS $_$
DECLARE
  ret uuid;
BEGIN
  EXECUTE format('UPDATE kiwi_mod_permissions.%I SET jsonb=$3 WHERE id=$2 RETURNING id', $1)
          USING $1, $2, $3 INTO ret;
  IF ret IS NOT NULL THEN
    RETURN ret;
  END IF;
  EXECUTE format('INSERT INTO kiwi_mod_permissions.%I (id, jsonb) VALUES ($2, $3) RETURNING id', $1)
          USING $1, $2, $3 INTO STRICT ret;
  RETURN ret;
END;
$_$;



--
-- Name: uuid_larger(uuid, uuid); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.uuid_larger(uuid, uuid) RETURNS uuid
    LANGUAGE plpgsql
    AS $_$
BEGIN
  IF $1 IS NULL THEN
    RETURN $2;
  END IF;
  IF $2 IS NULL THEN
    RETURN $1;
  END IF;
  IF $1 > $2 THEN
    RETURN $1;
  ELSE
    RETURN $2;
  END IF;
END;
$_$;



--
-- Name: uuid_smaller(uuid, uuid); Type: FUNCTION; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE FUNCTION kiwi_mod_permissions.uuid_smaller(uuid, uuid) RETURNS uuid
    LANGUAGE plpgsql
    AS $_$
BEGIN
  IF $1 IS NULL THEN
    RETURN $2;
  END IF;
  IF $2 IS NULL THEN
    RETURN $1;
  END IF;
  IF $1 < $2 THEN
    RETURN $1;
  ELSE
    RETURN $2;
  END IF;
END;
$_$;



--
-- Name: max(uuid); Type: AGGREGATE; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE AGGREGATE kiwi_mod_permissions.max(uuid) (
    SFUNC = kiwi_mod_permissions.uuid_larger,
    STYPE = uuid,
    COMBINEFUNC = kiwi_mod_permissions.uuid_larger,
    SORTOP = OPERATOR(pg_catalog.>),
    PARALLEL = safe
);



--
-- Name: min(uuid); Type: AGGREGATE; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE AGGREGATE kiwi_mod_permissions.min(uuid) (
    SFUNC = kiwi_mod_permissions.uuid_smaller,
    STYPE = uuid,
    COMBINEFUNC = kiwi_mod_permissions.uuid_smaller,
    SORTOP = OPERATOR(pg_catalog.<),
    PARALLEL = safe
);



SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: permissions; Type: TABLE; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE TABLE kiwi_mod_permissions.permissions (
    id uuid NOT NULL,
    jsonb jsonb NOT NULL,
    creation_date timestamp without time zone,
    created_by text
);



--
-- Name: permissions_users; Type: TABLE; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE TABLE kiwi_mod_permissions.permissions_users (
    id uuid NOT NULL,
    jsonb jsonb NOT NULL,
    creation_date timestamp without time zone,
    created_by text
);



--
-- Name: rmb_internal; Type: TABLE; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE TABLE kiwi_mod_permissions.rmb_internal (
    id integer NOT NULL,
    jsonb jsonb NOT NULL
);



--
-- Name: rmb_internal_analyze; Type: TABLE; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE TABLE kiwi_mod_permissions.rmb_internal_analyze (
    tablename text
);



--
-- Name: rmb_internal_id_seq; Type: SEQUENCE; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE SEQUENCE kiwi_mod_permissions.rmb_internal_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;



--
-- Name: rmb_internal_id_seq; Type: SEQUENCE OWNED BY; Schema: kiwi_mod_permissions; Owner: postgres
--

ALTER SEQUENCE kiwi_mod_permissions.rmb_internal_id_seq OWNED BY kiwi_mod_permissions.rmb_internal.id;


--
-- Name: rmb_internal_index; Type: TABLE; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE TABLE kiwi_mod_permissions.rmb_internal_index (
    name text NOT NULL,
    def text NOT NULL,
    remove boolean NOT NULL
);



--
-- Name: rmb_job; Type: TABLE; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE TABLE kiwi_mod_permissions.rmb_job (
    id uuid NOT NULL,
    jsonb jsonb NOT NULL
);



--
-- Name: rmb_internal id; Type: DEFAULT; Schema: kiwi_mod_permissions; Owner: postgres
--

ALTER TABLE ONLY kiwi_mod_permissions.rmb_internal ALTER COLUMN id SET DEFAULT nextval('kiwi_mod_permissions.rmb_internal_id_seq'::regclass);


--
-- Data for Name: permissions; Type: TABLE DATA; Schema: kiwi_mod_permissions; Owner: postgres
--

COPY kiwi_mod_permissions.permissions (id, jsonb, creation_date, created_by) FROM stdin;
\.


--
-- Data for Name: permissions_users; Type: TABLE DATA; Schema: kiwi_mod_permissions; Owner: postgres
--

COPY kiwi_mod_permissions.permissions_users (id, jsonb, creation_date, created_by) FROM stdin;
9b283803-dfca-4d3f-8410-2a8a64ce0033	{"id": "9b283803-dfca-4d3f-8410-2a8a64ce0033", "userId": "956f39c5-92e3-4c26-bcdc-1827674710cf", "metadata": {"createdDate": "2022-05-06T13:16:32.837", "updatedDate": "2022-05-06T13:16:32.837+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
c9b58745-ad2a-4f94-b8f4-1bfbb61bf840	{"id": "c9b58745-ad2a-4f94-b8f4-1bfbb61bf840", "userId": "a0dadce9-06ed-4b23-9fc0-6b5238aa92d8", "metadata": {"createdDate": "2022-05-06T13:16:32.878", "updatedDate": "2022-05-06T13:16:32.878+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
37deb207-f5e0-4389-8eee-ff3b53b94d5b	{"id": "37deb207-f5e0-4389-8eee-ff3b53b94d5b", "userId": "6302b991-3223-4bc7-ae66-795d161f64ab", "metadata": {"createdDate": "2022-05-06T13:16:32.883", "updatedDate": "2022-05-06T13:16:32.883+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
936148a4-706f-4969-ad15-dea8f73559ea	{"id": "936148a4-706f-4969-ad15-dea8f73559ea", "userId": "3ada4a0c-e554-4749-8809-fee35fe2c7ad", "metadata": {"createdDate": "2022-05-06T13:16:32.881", "updatedDate": "2022-05-06T13:16:32.881+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
3984415f-0a3b-497e-9834-81e3372c4183	{"id": "3984415f-0a3b-497e-9834-81e3372c4183", "userId": "f1dc9a7e-492b-4f2b-848a-115b5919d589", "metadata": {"createdDate": "2022-05-06T13:16:32.879", "updatedDate": "2022-05-06T13:16:32.879+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
a9a82929-3577-476c-8c3d-651aa4c839ec	{"id": "a9a82929-3577-476c-8c3d-651aa4c839ec", "userId": "87c329f1-2220-4a8a-b750-ded39bbe9769", "metadata": {"createdDate": "2022-05-06T13:16:32.916", "updatedDate": "2022-05-06T13:16:32.916+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
6586de32-4614-429f-a5da-b2a2c562a890	{"id": "6586de32-4614-429f-a5da-b2a2c562a890", "userId": "45e77e83-60a3-4031-89d5-81222043dec6", "metadata": {"createdDate": "2022-05-06T13:16:32.926", "updatedDate": "2022-05-06T13:16:32.926+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
442439c2-c99a-4d38-a83f-02c84f456e7e	{"id": "442439c2-c99a-4d38-a83f-02c84f456e7e", "userId": "e505acd3-925e-4e2c-a255-8d11e25ba046", "metadata": {"createdDate": "2022-05-06T13:16:32.931", "updatedDate": "2022-05-06T13:16:32.931+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
0d0ecd33-d936-491b-b7de-efb676090a56	{"id": "0d0ecd33-d936-491b-b7de-efb676090a56", "userId": "8f7a47c4-d66f-4dba-9255-f74507a2ecee", "metadata": {"createdDate": "2022-05-06T13:16:32.935", "updatedDate": "2022-05-06T13:16:32.935+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
829d5a09-82aa-4c08-9e9a-af16a89cab29	{"id": "829d5a09-82aa-4c08-9e9a-af16a89cab29", "userId": "2075c729-a9b8-43db-860c-60a3cc31a949", "metadata": {"createdDate": "2022-05-06T13:16:32.937", "updatedDate": "2022-05-06T13:16:32.937+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
b6ec1719-82e4-4138-83bc-2e487355bd4d	{"id": "b6ec1719-82e4-4138-83bc-2e487355bd4d", "userId": "48861bba-0d73-4277-8f44-f3b65b038017", "metadata": {"createdDate": "2022-05-06T13:16:32.946", "updatedDate": "2022-05-06T13:16:32.946+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
8466bdcf-a0d2-4380-99cd-fead25a211ab	{"id": "8466bdcf-a0d2-4380-99cd-fead25a211ab", "userId": "2884afb0-5bec-45c4-b9f4-0bc525bc0322", "metadata": {"createdDate": "2022-05-06T13:16:32.954", "updatedDate": "2022-05-06T13:16:32.954+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
880e3b5a-5d5e-4c22-be3d-957e7d822e0f	{"id": "880e3b5a-5d5e-4c22-be3d-957e7d822e0f", "userId": "dc3cd5a5-4235-48c3-b9d3-a958863f7498", "metadata": {"createdDate": "2022-05-06T13:16:32.956", "updatedDate": "2022-05-06T13:16:32.956+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
8a216ad7-1ae3-4286-924e-a3ae47d58723	{"id": "8a216ad7-1ae3-4286-924e-a3ae47d58723", "userId": "63ff6975-5d7f-46c1-983a-dba27d163c4a", "metadata": {"createdDate": "2022-05-06T13:16:32.96", "updatedDate": "2022-05-06T13:16:32.960+00:00"}, "permissions": []}	2000-01-01 13:16:32.96	\N
fbe6e91c-561b-4f1b-a2d6-e91fb8c46d9b	{"id": "fbe6e91c-561b-4f1b-a2d6-e91fb8c46d9b", "userId": "e76cf4c9-e15a-414e-81b9-672a90fb2745", "metadata": {"createdDate": "2022-05-06T13:16:32.963", "updatedDate": "2022-05-06T13:16:32.963+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
7f8b0504-c858-455b-9323-cda53d0385d8	{"id": "7f8b0504-c858-455b-9323-cda53d0385d8", "userId": "89066e1d-0691-4514-ae37-586cf746d3f4", "metadata": {"createdDate": "2022-05-06T13:16:32.972", "updatedDate": "2022-05-06T13:16:32.972+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
39e30e87-5ae4-4f1f-9abf-fbbdecc49ddd	{"id": "39e30e87-5ae4-4f1f-9abf-fbbdecc49ddd", "userId": "d4849a05-4066-4129-ae56-3dfc39498e36", "metadata": {"createdDate": "2022-05-06T13:16:32.976", "updatedDate": "2022-05-06T13:16:32.976+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
d1a5731b-b23b-4ae8-9810-7fd29f7bf524	{"id": "d1a5731b-b23b-4ae8-9810-7fd29f7bf524", "userId": "70e0c050-e842-4eee-9632-967a49e43bb2", "metadata": {"createdDate": "2022-05-06T13:16:32.981", "updatedDate": "2022-05-06T13:16:32.981+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
e08c3ff6-7f81-424c-87aa-fcd974cdea24	{"id": "e08c3ff6-7f81-424c-87aa-fcd974cdea24", "userId": "43f60acf-2557-48e9-b457-12783925444f", "metadata": {"createdDate": "2022-05-06T13:16:32.984", "updatedDate": "2022-05-06T13:16:32.984+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
d0e8a640-3ef6-46d7-a5b5-0cea9f3f2b8e	{"id": "d0e8a640-3ef6-46d7-a5b5-0cea9f3f2b8e", "userId": "beaffbac-e56d-4e32-a653-b631945f060c", "metadata": {"createdDate": "2022-05-06T13:16:32.989", "updatedDate": "2022-05-06T13:16:32.989+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
9f2aca54-9479-4e07-9d9e-5261781d514f	{"id": "9f2aca54-9479-4e07-9d9e-5261781d514f", "userId": "17b26a4a-e481-4b86-8949-5ef6570eb622", "metadata": {"createdDate": "2022-05-06T13:16:32.999", "updatedDate": "2022-05-06T13:16:32.999+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
3e32da39-e50a-4a45-a072-1184af5d0a05	{"id": "3e32da39-e50a-4a45-a072-1184af5d0a05", "userId": "260f1870-7dee-452d-a379-301f063febda", "metadata": {"createdDate": "2022-05-06T13:16:32.997", "updatedDate": "2022-05-06T13:16:32.997+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
5d7c754e-e4bf-47fd-8fcd-87a31cb61150	{"id": "5d7c754e-e4bf-47fd-8fcd-87a31cb61150", "userId": "a8a11126-40b9-45f0-aa6e-9408e57c4b47", "metadata": {"createdDate": "2022-05-06T13:16:33.005", "updatedDate": "2022-05-06T13:16:33.005+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
119ab4f1-2983-4ed7-b58a-dd7589a4429d	{"id": "119ab4f1-2983-4ed7-b58a-dd7589a4429d", "userId": "ab60d124-5d41-49c5-8aae-2ef2cd3704c2", "metadata": {"createdDate": "2022-05-06T13:16:33.007", "updatedDate": "2022-05-06T13:16:33.007+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
67ab7d4c-fa4c-4e99-805e-bef7cc8a41a5	{"id": "67ab7d4c-fa4c-4e99-805e-bef7cc8a41a5", "userId": "c2f1cefa-2ebb-4a7a-a420-84dcf2f89cf5", "metadata": {"createdDate": "2022-05-06T13:16:33.011", "updatedDate": "2022-05-06T13:16:33.011+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
628f0dc1-8b6b-4a98-a032-c201b37423b9	{"id": "628f0dc1-8b6b-4a98-a032-c201b37423b9", "userId": "c2a2f428-ab5f-46ce-b3ed-7d0ab39a0096", "metadata": {"createdDate": "2022-05-06T13:16:33.019", "updatedDate": "2022-05-06T13:16:33.019+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
a06048e0-8f13-4d22-9ce1-41b993b0d73e	{"id": "a06048e0-8f13-4d22-9ce1-41b993b0d73e", "userId": "fa18e51c-50d9-4fe3-ab58-c592ea30328a", "metadata": {"createdDate": "2022-05-06T13:16:33.022", "updatedDate": "2022-05-06T13:16:33.022+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
257a045b-62ff-4bb8-9c14-98d8b447c4f1	{"id": "257a045b-62ff-4bb8-9c14-98d8b447c4f1", "userId": "2e5f9cc4-46ab-4dfe-b40a-8493296353fb", "metadata": {"createdDate": "2022-05-06T13:16:33.024", "updatedDate": "2022-05-06T13:16:33.024+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
2650a06f-1672-4a2f-9c5f-0a27b565df3b	{"id": "2650a06f-1672-4a2f-9c5f-0a27b565df3b", "userId": "86344e52-979a-45da-ad44-9edcc05c5312", "metadata": {"createdDate": "2022-05-06T13:16:33.045", "updatedDate": "2022-05-06T13:16:33.045+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
750ed627-060b-4a3f-8e49-474f6623c03d	{"id": "750ed627-060b-4a3f-8e49-474f6623c03d", "userId": "0a246f61-d85f-42b6-8dcc-48d25a46690b", "metadata": {"createdDate": "2022-05-06T13:16:33.066", "updatedDate": "2022-05-06T13:16:33.066+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
a389d10b-8666-42a9-9b24-171a703ae89c	{"id": "a389d10b-8666-42a9-9b24-171a703ae89c", "userId": "8616dd12-e244-4047-834a-db0c6cd1477b", "metadata": {"createdDate": "2022-05-06T13:16:33.094", "updatedDate": "2022-05-06T13:16:33.094+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
7d5d9fc0-598a-4f37-b683-aff60eb38e0a	{"id": "7d5d9fc0-598a-4f37-b683-aff60eb38e0a", "userId": "e5e950e0-3f56-4ff1-8e86-671cdbc37688", "metadata": {"createdDate": "2022-05-06T13:16:33.104", "updatedDate": "2022-05-06T13:16:33.104+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
5b71acd9-ef3d-43e1-9c39-b24ff3551b0c	{"id": "5b71acd9-ef3d-43e1-9c39-b24ff3551b0c", "userId": "c0af9380-d277-4820-a607-15a2d9c50ba6", "metadata": {"createdDate": "2022-05-06T13:16:33.117", "updatedDate": "2022-05-06T13:16:33.117+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
e8bb6f44-eb82-4dfc-8391-439daa76edca	{"id": "e8bb6f44-eb82-4dfc-8391-439daa76edca", "userId": "251cfa92-24a7-45db-9f0d-acd2da450b50", "metadata": {"createdDate": "2022-05-06T13:16:33.131", "updatedDate": "2022-05-06T13:16:33.131+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
b57f0450-1468-494a-a202-62b4d3c8725c	{"id": "b57f0450-1468-494a-a202-62b4d3c8725c", "userId": "5bec815c-72b9-452f-ac19-bc2793c94537", "metadata": {"createdDate": "2022-05-06T13:16:33.15", "updatedDate": "2022-05-06T13:16:33.150+00:00"}, "permissions": []}	2000-01-01 13:16:33.15	\N
e7ed679a-d907-4ac4-8747-8d6824f99acd	{"id": "e7ed679a-d907-4ac4-8747-8d6824f99acd", "userId": "0d6d38bf-aaf6-4976-a2ce-7ff787195982", "metadata": {"createdDate": "2022-05-06T13:16:33.163", "updatedDate": "2022-05-06T13:16:33.163+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
2953b38e-e0aa-4e72-ae0a-e3ba329a13b5	{"id": "2953b38e-e0aa-4e72-ae0a-e3ba329a13b5", "userId": "f1c2d681-faba-4950-918f-bf58d914ba1f", "metadata": {"createdDate": "2022-05-06T13:16:33.169", "updatedDate": "2022-05-06T13:16:33.169+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
61845ad0-f340-427f-b48b-b82e6fa8fdcf	{"id": "61845ad0-f340-427f-b48b-b82e6fa8fdcf", "userId": "ae2f6ce7-386a-4c5a-9fcf-e5f517a88ced", "metadata": {"createdDate": "2022-05-06T13:16:33.185", "updatedDate": "2022-05-06T13:16:33.185+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
3eb7ae58-0f2b-461d-86df-59ffce54111e	{"id": "3eb7ae58-0f2b-461d-86df-59ffce54111e", "userId": "32134c21-fae8-497e-b2d2-daa1ba421070", "metadata": {"createdDate": "2022-05-06T13:16:33.19", "updatedDate": "2022-05-06T13:16:33.190+00:00"}, "permissions": []}	2000-01-01 13:16:33.19	\N
030b0bac-d739-4e89-9106-18d9267f36b7	{"id": "030b0bac-d739-4e89-9106-18d9267f36b7", "userId": "6b5a896c-c6f4-4a28-a89a-2e2ca6ff0d0e", "metadata": {"createdDate": "2022-05-06T13:16:33.201", "updatedDate": "2022-05-06T13:16:33.201+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
f0573c8e-14e3-4c29-b2f6-0cca7a397373	{"id": "f0573c8e-14e3-4c29-b2f6-0cca7a397373", "userId": "52e47672-d456-40b6-9f2d-6597d3e9f942", "metadata": {"createdDate": "2022-05-06T13:16:33.207", "updatedDate": "2022-05-06T13:16:33.207+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
84bca7c6-e099-47da-a98e-044a61c455b6	{"id": "84bca7c6-e099-47da-a98e-044a61c455b6", "userId": "7597bd13-9f57-4cd1-a7cf-dc0ac7375280", "metadata": {"createdDate": "2022-05-06T13:16:33.224", "updatedDate": "2022-05-06T13:16:33.224+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
ac9cfda8-42f8-4df2-9ab9-dc0e55989651	{"id": "ac9cfda8-42f8-4df2-9ab9-dc0e55989651", "userId": "b7f677aa-e2db-4bb5-81f8-beee547bce68", "metadata": {"createdDate": "2022-05-06T13:16:33.232", "updatedDate": "2022-05-06T13:16:33.232+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
39770927-caf3-483f-9571-80516d930bb5	{"id": "39770927-caf3-483f-9571-80516d930bb5", "userId": "bf93cf45-4c02-4a34-aad0-9ed949109630", "metadata": {"createdDate": "2022-05-06T13:16:33.258", "updatedDate": "2022-05-06T13:16:33.258+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
61604871-2e78-4728-9db5-ad5a898edd76	{"id": "61604871-2e78-4728-9db5-ad5a898edd76", "userId": "abad6503-a51b-4fec-a1cd-b5f672b1ff7b", "metadata": {"createdDate": "2022-05-06T13:16:33.268", "updatedDate": "2022-05-06T13:16:33.268+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
129c0698-74d6-4276-ac78-a4a42e43d5fc	{"id": "129c0698-74d6-4276-ac78-a4a42e43d5fc", "userId": "5cc5bd09-d90e-4484-8058-74c237165877", "metadata": {"createdDate": "2022-05-06T13:16:33.28", "updatedDate": "2022-05-06T13:16:33.280+00:00"}, "permissions": []}	2000-01-01 13:16:33.28	\N
58eeced3-1e5b-40ac-901b-be6de1ef53d1	{"id": "58eeced3-1e5b-40ac-901b-be6de1ef53d1", "userId": "7dd96d33-6abf-4394-8768-647c76d79412", "metadata": {"createdDate": "2022-05-06T13:16:33.298", "updatedDate": "2022-05-06T13:16:33.298+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
15485084-db3b-419f-975f-8c7e8f4455d8	{"id": "15485084-db3b-419f-975f-8c7e8f4455d8", "userId": "fbc2b501-a8cc-43a7-8d8c-b68067b63a33", "metadata": {"createdDate": "2022-05-06T13:16:33.309", "updatedDate": "2022-05-06T13:16:33.309+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
163496a4-f7f3-4444-8289-f3d0ec0473e7	{"id": "163496a4-f7f3-4444-8289-f3d0ec0473e7", "userId": "a49cefad-7447-4f2f-9004-de32e7a6cc53", "metadata": {"createdDate": "2022-05-06T13:16:33.318", "updatedDate": "2022-05-06T13:16:33.318+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
a6c2530c-1f10-4a7a-9782-431a36e65614	{"id": "a6c2530c-1f10-4a7a-9782-431a36e65614", "userId": "4a5e1aa3-0733-45d9-b9cc-836b4e92d6ea", "metadata": {"createdDate": "2022-05-06T13:16:33.327", "updatedDate": "2022-05-06T13:16:33.327+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
a9c9836f-04dc-4e06-8dba-03c44744d202	{"id": "a9c9836f-04dc-4e06-8dba-03c44744d202", "userId": "0ab0736b-57ba-404c-9b17-d94de2cf4d9a", "metadata": {"createdDate": "2022-05-06T13:16:33.345", "updatedDate": "2022-05-06T13:16:33.345+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
5bb4cbab-0751-46d2-8ddc-1d42155dcd87	{"id": "5bb4cbab-0751-46d2-8ddc-1d42155dcd87", "userId": "5ed8a4be-f0d8-459d-9e9a-27f2e8c155af", "metadata": {"createdDate": "2022-05-06T13:16:33.364", "updatedDate": "2022-05-06T13:16:33.364+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
b36fcc5b-7afd-4bb1-8728-dcf4de79ec7e	{"id": "b36fcc5b-7afd-4bb1-8728-dcf4de79ec7e", "userId": "6b5f249c-7df1-4c2f-afc2-0ef6fc21b701", "metadata": {"createdDate": "2022-05-06T13:16:33.382", "updatedDate": "2022-05-06T13:16:33.382+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
704b5215-0404-44c2-94c4-06de558350d8	{"id": "704b5215-0404-44c2-94c4-06de558350d8", "userId": "4ca6da61-a9fa-4226-850d-43aa2d89f9a6", "metadata": {"createdDate": "2022-05-06T13:16:33.397", "updatedDate": "2022-05-06T13:16:33.397+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
b873f306-8b41-44e8-88a8-c1e1b1fcc11d	{"id": "b873f306-8b41-44e8-88a8-c1e1b1fcc11d", "userId": "65ec5d8b-c3f6-41d4-8026-fba1f7cae715", "metadata": {"createdDate": "2022-05-06T13:16:33.031", "updatedDate": "2022-05-06T13:16:33.031+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
25d3d34c-415f-498b-85c8-b8f10a7014f5	{"id": "25d3d34c-415f-498b-85c8-b8f10a7014f5", "userId": "f7a0a518-6ff3-4531-b54b-e630d61aede0", "metadata": {"createdDate": "2022-05-06T13:16:33.046", "updatedDate": "2022-05-06T13:16:33.046+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
83996b32-38d2-4d51-886d-6f698687bb06	{"id": "83996b32-38d2-4d51-886d-6f698687bb06", "userId": "47f7eaea-1a18-4058-907c-62b7d095c61b", "metadata": {"createdDate": "2022-05-06T13:16:33.068", "updatedDate": "2022-05-06T13:16:33.068+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
7b6e32b2-884e-45fa-b49f-9edda6440eb6	{"id": "7b6e32b2-884e-45fa-b49f-9edda6440eb6", "userId": "f303e908-30dc-4139-9542-4a4e206c4b96", "metadata": {"createdDate": "2022-05-06T13:16:33.091", "updatedDate": "2022-05-06T13:16:33.091+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
c2052c5e-be72-4c91-a759-ae911b19c9e9	{"id": "c2052c5e-be72-4c91-a759-ae911b19c9e9", "userId": "57db810d-d59c-4443-ab43-3542cfdf7905", "metadata": {"createdDate": "2022-05-06T13:16:33.116", "updatedDate": "2022-05-06T13:16:33.116+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
1b5ba911-c529-47a0-9054-8e5080e5028a	{"id": "1b5ba911-c529-47a0-9054-8e5080e5028a", "userId": "28724f2b-89b3-4a05-839c-2e77138e01a3", "metadata": {"createdDate": "2022-05-06T13:16:33.132", "updatedDate": "2022-05-06T13:16:33.132+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
d2c5ec6b-c441-41f5-b633-706d830c0e8d	{"id": "d2c5ec6b-c441-41f5-b633-706d830c0e8d", "userId": "1f2608df-ff79-4576-b578-14627bbe87fa", "metadata": {"createdDate": "2022-05-06T13:16:33.152", "updatedDate": "2022-05-06T13:16:33.152+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
09742c39-c0bb-4163-9b3d-617a72cf4572	{"id": "09742c39-c0bb-4163-9b3d-617a72cf4572", "userId": "30d7e2dd-db23-4832-b4be-30d3f5f83a60", "metadata": {"createdDate": "2022-05-06T13:16:33.171", "updatedDate": "2022-05-06T13:16:33.171+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
198d2cbe-62b9-4e25-8f55-376667a970cf	{"id": "198d2cbe-62b9-4e25-8f55-376667a970cf", "userId": "2a81b279-d459-4022-82f6-69a569d196b9", "metadata": {"createdDate": "2022-05-06T13:16:33.188", "updatedDate": "2022-05-06T13:16:33.188+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
1ad5d37e-83c5-47f1-a62d-59655fba8330	{"id": "1ad5d37e-83c5-47f1-a62d-59655fba8330", "userId": "2120fe62-ba0a-4dce-8701-f360368d5c30", "metadata": {"createdDate": "2022-05-06T13:16:33.205", "updatedDate": "2022-05-06T13:16:33.205+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
3b0e0284-18cf-4bd6-a138-6af79be6ee33	{"id": "3b0e0284-18cf-4bd6-a138-6af79be6ee33", "userId": "c8edf675-9323-4410-9d14-0727e038dad0", "metadata": {"createdDate": "2022-05-06T13:16:33.218", "updatedDate": "2022-05-06T13:16:33.218+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
051623cb-efd8-46b1-a53d-026d463de674	{"id": "051623cb-efd8-46b1-a53d-026d463de674", "userId": "e0b7cb11-7d1f-48d8-b8a5-bc138550313d", "metadata": {"createdDate": "2022-05-06T13:16:33.239", "updatedDate": "2022-05-06T13:16:33.239+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
e103dac3-5188-476b-89ff-6ba74ee8041d	{"id": "e103dac3-5188-476b-89ff-6ba74ee8041d", "userId": "a983c74e-b4f5-4ca9-94f0-b79efa947b27", "metadata": {"createdDate": "2022-05-06T13:16:33.26", "updatedDate": "2022-05-06T13:16:33.260+00:00"}, "permissions": []}	2000-01-01 13:16:33.26	\N
e4cfb4c4-7eab-4b6d-b03b-66793526b7f7	{"id": "e4cfb4c4-7eab-4b6d-b03b-66793526b7f7", "userId": "f5d7aed2-1647-4e83-b85e-74760f770799", "metadata": {"createdDate": "2022-05-06T13:16:33.278", "updatedDate": "2022-05-06T13:16:33.278+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
f62600d3-b2c2-4ea4-b61c-16fc3fe3f2c8	{"id": "f62600d3-b2c2-4ea4-b61c-16fc3fe3f2c8", "userId": "6f36265e-722a-490a-b436-806e63af2ea7", "metadata": {"createdDate": "2022-05-06T13:16:33.301", "updatedDate": "2022-05-06T13:16:33.301+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
a00a97d1-096d-4a21-8663-8aee5997786d	{"id": "a00a97d1-096d-4a21-8663-8aee5997786d", "userId": "23807f0f-6053-417c-b335-f3b0f84ceb8e", "metadata": {"createdDate": "2022-05-06T13:16:33.314", "updatedDate": "2022-05-06T13:16:33.314+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
960442ff-0af1-4ec2-99e6-aa7a38d23e1f	{"id": "960442ff-0af1-4ec2-99e6-aa7a38d23e1f", "userId": "8cde4a35-f58b-492e-bd07-d668f7322253", "metadata": {"createdDate": "2022-05-06T13:16:33.332", "updatedDate": "2022-05-06T13:16:33.332+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
14d6bd55-287a-4cc7-9c05-d255d838bc31	{"id": "14d6bd55-287a-4cc7-9c05-d255d838bc31", "userId": "cc0685f2-6ac2-4840-bb67-1493c14968c5", "metadata": {"createdDate": "2022-05-06T13:16:33.346", "updatedDate": "2022-05-06T13:16:33.346+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
e0f2a2ac-cf1f-4916-8d53-fe56210b6daa	{"id": "e0f2a2ac-cf1f-4916-8d53-fe56210b6daa", "userId": "5a57f974-ea09-4c87-b7f5-e4144dde6128", "metadata": {"createdDate": "2022-05-06T13:16:33.366", "updatedDate": "2022-05-06T13:16:33.366+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
f3e434b5-cafd-43b2-972a-53a148e61848	{"id": "f3e434b5-cafd-43b2-972a-53a148e61848", "userId": "e546d50a-926a-421f-8400-a041a2e9db79", "metadata": {"createdDate": "2022-05-06T13:16:33.385", "updatedDate": "2022-05-06T13:16:33.385+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
0948012b-95ee-473d-862c-c829a8d3b642	{"id": "0948012b-95ee-473d-862c-c829a8d3b642", "userId": "3b464026-79e7-450a-a441-0e1d4f8ebf99", "metadata": {"createdDate": "2022-05-06T13:16:33.396", "updatedDate": "2022-05-06T13:16:33.396+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
299a72da-9d6e-422d-b2a8-8de295696019	{"id": "299a72da-9d6e-422d-b2a8-8de295696019", "userId": "93136048-585b-466e-88f8-a10115e6d7e2", "metadata": {"createdDate": "2022-05-06T13:16:33.41", "updatedDate": "2022-05-06T13:16:33.410+00:00"}, "permissions": []}	2000-01-01 13:16:33.41	\N
e613158d-f8fe-4b63-9067-c9ed79e73a69	{"id": "e613158d-f8fe-4b63-9067-c9ed79e73a69", "userId": "ade18246-c529-497f-bd4c-2c3f85e995ad", "metadata": {"createdDate": "2022-05-06T13:16:33.424", "updatedDate": "2022-05-06T13:16:33.424+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
02c25021-1496-4f71-80e5-2cc17c4f600a	{"id": "02c25021-1496-4f71-80e5-2cc17c4f600a", "userId": "e6dfcfef-e724-4485-870d-d2c4d1dcfdd9", "metadata": {"createdDate": "2022-05-06T13:16:33.442", "updatedDate": "2022-05-06T13:16:33.442+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
e48dee06-cf3d-43e0-84c4-be441f953322	{"id": "e48dee06-cf3d-43e0-84c4-be441f953322", "userId": "9ad09b01-7429-455f-9f64-e3897027db61", "metadata": {"createdDate": "2022-05-06T13:16:33.456", "updatedDate": "2022-05-06T13:16:33.456+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
91e98705-f4dc-406d-bc36-70f1b4e8a3a3	{"id": "91e98705-f4dc-406d-bc36-70f1b4e8a3a3", "userId": "bd566b21-d125-421c-9c78-2b9a8bc4c4f7", "metadata": {"createdDate": "2022-05-06T13:16:33.469", "updatedDate": "2022-05-06T13:16:33.469+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
9beecbca-01fd-42b2-ab45-1d78ce8277c0	{"id": "9beecbca-01fd-42b2-ab45-1d78ce8277c0", "userId": "dd176277-5c2d-4310-bf2f-e45e312b5026", "metadata": {"createdDate": "2022-05-06T13:16:33.482", "updatedDate": "2022-05-06T13:16:33.482+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
35c54b53-922f-4ebd-bf77-81b30739fd5d	{"id": "35c54b53-922f-4ebd-bf77-81b30739fd5d", "userId": "c0d4a2da-7c38-46f4-869c-797bb083ee2d", "metadata": {"createdDate": "2022-05-06T13:16:33.034", "updatedDate": "2022-05-06T13:16:33.034+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
2c15ac29-f63d-4881-ae95-fa2d9b7365f9	{"id": "2c15ac29-f63d-4881-ae95-fa2d9b7365f9", "userId": "066795ce-4938-48f2-9411-f3f922b51e1c", "metadata": {"createdDate": "2022-05-06T13:16:33.055", "updatedDate": "2022-05-06T13:16:33.055+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
bc7c81d6-f3e9-408e-95fb-520a0e416391	{"id": "bc7c81d6-f3e9-408e-95fb-520a0e416391", "userId": "c1277b9b-b165-48ee-ac35-e737ed325f34", "metadata": {"createdDate": "2022-05-06T13:16:33.07", "updatedDate": "2022-05-06T13:16:33.070+00:00"}, "permissions": []}	2000-01-01 13:16:33.07	\N
ea3b9066-a685-4936-8adb-4979b46b3246	{"id": "ea3b9066-a685-4936-8adb-4979b46b3246", "userId": "b617c5f2-78d4-4c7e-bf0c-d21392a8c39f", "metadata": {"createdDate": "2022-05-06T13:16:33.086", "updatedDate": "2022-05-06T13:16:33.086+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
8859a82c-944b-4820-8555-22f685543088	{"id": "8859a82c-944b-4820-8555-22f685543088", "userId": "b4e91548-c387-4b01-aaa1-489afc3f6936", "metadata": {"createdDate": "2022-05-06T13:16:33.099", "updatedDate": "2022-05-06T13:16:33.099+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
7f57d30c-5615-4222-ad32-1d914492bbf5	{"id": "7f57d30c-5615-4222-ad32-1d914492bbf5", "userId": "62b25727-310f-4fa3-b308-666a6cf28c97", "metadata": {"createdDate": "2022-05-06T13:16:33.11", "updatedDate": "2022-05-06T13:16:33.110+00:00"}, "permissions": []}	2000-01-01 13:16:33.11	\N
8c99b0fe-5ff9-48e9-86ab-f8a5e04d4ea7	{"id": "8c99b0fe-5ff9-48e9-86ab-f8a5e04d4ea7", "userId": "e308411d-773e-416e-be58-f16176c0549e", "metadata": {"createdDate": "2022-05-06T13:16:33.122", "updatedDate": "2022-05-06T13:16:33.122+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
4021cac3-1bc9-46a1-b532-9c6d9b906ae6	{"id": "4021cac3-1bc9-46a1-b532-9c6d9b906ae6", "userId": "457a3c37-cada-47ed-ae8d-c8eda723251f", "metadata": {"createdDate": "2022-05-06T13:16:33.135", "updatedDate": "2022-05-06T13:16:33.135+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
1758b54b-a0ed-42b2-9f9e-c5435ff7e347	{"id": "1758b54b-a0ed-42b2-9f9e-c5435ff7e347", "userId": "b09038a4-0386-4782-8ee8-11aa87e09887", "metadata": {"createdDate": "2022-05-06T13:16:33.147", "updatedDate": "2022-05-06T13:16:33.147+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
1799a1f9-1c9c-4bb4-9fc3-2dae56179d63	{"id": "1799a1f9-1c9c-4bb4-9fc3-2dae56179d63", "userId": "51e1e298-db10-465b-8c20-7f3d1e929834", "metadata": {"createdDate": "2022-05-06T13:16:33.158", "updatedDate": "2022-05-06T13:16:33.158+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
2c20a3e1-f1b2-46a6-abd5-758422a1d300	{"id": "2c20a3e1-f1b2-46a6-abd5-758422a1d300", "userId": "9207540e-91e9-4a75-ad1e-65a715784326", "metadata": {"createdDate": "2022-05-06T13:16:33.174", "updatedDate": "2022-05-06T13:16:33.174+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
0357e285-703c-4efd-9f29-250620004840	{"id": "0357e285-703c-4efd-9f29-250620004840", "userId": "dc6e1590-7021-433d-98a3-eda0f8d8fde1", "metadata": {"createdDate": "2022-05-06T13:16:33.183", "updatedDate": "2022-05-06T13:16:33.183+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
f3468a05-85da-429d-a2fd-792c4649ab48	{"id": "f3468a05-85da-429d-a2fd-792c4649ab48", "userId": "be3113c3-0965-4bf4-97c2-40ff54501c2b", "metadata": {"createdDate": "2022-05-06T13:16:33.213", "updatedDate": "2022-05-06T13:16:33.213+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
6ae29c5b-bcc3-46da-aa3e-e55af549def3	{"id": "6ae29c5b-bcc3-46da-aa3e-e55af549def3", "userId": "95a99d37-66b5-4b8d-a598-ab36618f9aac", "metadata": {"createdDate": "2022-05-06T13:16:33.222", "updatedDate": "2022-05-06T13:16:33.222+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
13f0ade7-478e-4e76-9319-2298c84aa630	{"id": "13f0ade7-478e-4e76-9319-2298c84aa630", "userId": "dc5dab8d-f80a-476a-b920-3cf21eeee902", "metadata": {"createdDate": "2022-05-06T13:16:33.234", "updatedDate": "2022-05-06T13:16:33.234+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
f2c1d3a3-601d-4a2e-a3af-5f0fae0f6d1b	{"id": "f2c1d3a3-601d-4a2e-a3af-5f0fae0f6d1b", "userId": "ceecd8ee-9586-4024-b107-d368b58a1025", "metadata": {"createdDate": "2022-05-06T13:16:33.246", "updatedDate": "2022-05-06T13:16:33.246+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
30981c82-2a8a-473e-acf5-adbfd7273be0	{"id": "30981c82-2a8a-473e-acf5-adbfd7273be0", "userId": "046353cf-3963-482c-9792-32ade0a33afa", "metadata": {"createdDate": "2022-05-06T13:16:33.252", "updatedDate": "2022-05-06T13:16:33.252+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
92ed843c-101b-429e-b4d4-2cfe5812cc03	{"id": "92ed843c-101b-429e-b4d4-2cfe5812cc03", "userId": "e1e435da-97e2-4083-8657-832aeb549929", "metadata": {"createdDate": "2022-05-06T13:16:33.265", "updatedDate": "2022-05-06T13:16:33.265+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
aa491157-c6c9-460f-93ac-d8f2aacdd566	{"id": "aa491157-c6c9-460f-93ac-d8f2aacdd566", "userId": "d3409c88-7e3f-497a-b94c-69e85e23b45c", "metadata": {"createdDate": "2022-05-06T13:16:33.277", "updatedDate": "2022-05-06T13:16:33.277+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
d600c5b1-8edc-4c9e-aa7f-e45ce50f000f	{"id": "d600c5b1-8edc-4c9e-aa7f-e45ce50f000f", "userId": "589ee8e5-4fe4-4ab3-8a58-441cebea454a", "metadata": {"createdDate": "2022-05-06T13:16:33.294", "updatedDate": "2022-05-06T13:16:33.294+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
ef393b03-88eb-4878-a40b-426690c8e702	{"id": "ef393b03-88eb-4878-a40b-426690c8e702", "userId": "86c9f455-a685-45d0-9d01-5943a1ba7e5b", "metadata": {"createdDate": "2022-05-06T13:16:33.305", "updatedDate": "2022-05-06T13:16:33.305+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
e2118f97-aa6a-40a3-a2f0-863e9355d42d	{"id": "e2118f97-aa6a-40a3-a2f0-863e9355d42d", "userId": "78a21fb3-0e80-4172-8ccf-8a1d8d5e1553", "metadata": {"createdDate": "2022-05-06T13:16:33.319", "updatedDate": "2022-05-06T13:16:33.319+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
a1af7633-c318-4479-9924-e3080ad1b681	{"id": "a1af7633-c318-4479-9924-e3080ad1b681", "userId": "71f28723-784e-4292-b794-af4ffca9178e", "metadata": {"createdDate": "2022-05-06T13:16:33.333", "updatedDate": "2022-05-06T13:16:33.333+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
88c4bf5e-4178-43cb-b560-f433da9dd34d	{"id": "88c4bf5e-4178-43cb-b560-f433da9dd34d", "userId": "dc13dcc6-2bda-412c-b046-1398d1becb75", "metadata": {"createdDate": "2022-05-06T13:16:33.343", "updatedDate": "2022-05-06T13:16:33.343+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
5163af34-c791-4df7-919e-caad440d98c7	{"id": "5163af34-c791-4df7-919e-caad440d98c7", "userId": "a2468e40-fb7c-453c-a217-8388801e2407", "metadata": {"createdDate": "2022-05-06T13:16:33.353", "updatedDate": "2022-05-06T13:16:33.353+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
aff9b2b9-8081-41ee-9eb7-07716fa96738	{"id": "aff9b2b9-8081-41ee-9eb7-07716fa96738", "userId": "7d7f46e8-5f99-4ac8-aa86-83a23f4bd8de", "metadata": {"createdDate": "2022-05-06T13:16:33.363", "updatedDate": "2022-05-06T13:16:33.363+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
5c48058a-f031-44d0-84fc-34ac09f636cb	{"id": "5c48058a-f031-44d0-84fc-34ac09f636cb", "userId": "e63273e7-48f5-4c43-ab4e-1751ecacaa21", "metadata": {"createdDate": "2022-05-06T13:16:33.371", "updatedDate": "2022-05-06T13:16:33.371+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
9bc75eae-3c0f-49e1-94b8-f22f53136fcc	{"id": "9bc75eae-3c0f-49e1-94b8-f22f53136fcc", "userId": "8931291a-8f92-4044-a17f-49a546b489ce", "metadata": {"createdDate": "2022-05-06T13:16:33.038", "updatedDate": "2022-05-06T13:16:33.038+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
80ab620e-401f-4ac9-8481-8c2aab69d4ab	{"id": "80ab620e-401f-4ac9-8481-8c2aab69d4ab", "userId": "6e74dfe1-2eca-48bd-89ce-9fe1633920a3", "metadata": {"createdDate": "2022-05-06T13:16:33.052", "updatedDate": "2022-05-06T13:16:33.052+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
81f76628-feab-42cf-99aa-b9b7fd615c35	{"id": "81f76628-feab-42cf-99aa-b9b7fd615c35", "userId": "2a424823-588a-45ee-9441-a6384b6614b2", "metadata": {"createdDate": "2022-05-06T13:16:33.06", "updatedDate": "2022-05-06T13:16:33.060+00:00"}, "permissions": []}	2000-01-01 13:16:33.06	\N
0f293723-9a72-4e8f-9f5b-e123b318fa29	{"id": "0f293723-9a72-4e8f-9f5b-e123b318fa29", "userId": "55e09c25-0a7b-4df8-8bde-a8964b57ef40", "metadata": {"createdDate": "2022-05-06T13:16:33.077", "updatedDate": "2022-05-06T13:16:33.077+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
711c3501-7374-4aa8-8442-fd8f000fa3e7	{"id": "711c3501-7374-4aa8-8442-fd8f000fa3e7", "userId": "44e640f4-3e0e-4bb4-92af-6263108893b2", "metadata": {"createdDate": "2022-05-06T13:16:33.098", "updatedDate": "2022-05-06T13:16:33.098+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
fa1b3a96-ab66-4855-bfb7-d4fcc688f351	{"id": "fa1b3a96-ab66-4855-bfb7-d4fcc688f351", "userId": "c926be9c-a8ce-4399-a9b3-11ec0fc8d6c9", "metadata": {"createdDate": "2022-05-06T13:16:33.111", "updatedDate": "2022-05-06T13:16:33.111+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
59310c91-8753-47bd-afe6-6193dc81c7c5	{"id": "59310c91-8753-47bd-afe6-6193dc81c7c5", "userId": "0f1f1a5d-49b6-42f4-8b18-faa2ce0e7be4", "metadata": {"createdDate": "2022-05-06T13:16:33.126", "updatedDate": "2022-05-06T13:16:33.126+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
63a50275-d777-4566-9079-a9cc375a3cdf	{"id": "63a50275-d777-4566-9079-a9cc375a3cdf", "userId": "969e6710-309e-41bd-ba35-2b97faec30b7", "metadata": {"createdDate": "2022-05-06T13:16:33.138", "updatedDate": "2022-05-06T13:16:33.138+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
f476f503-8778-40d2-8239-72015901fffd	{"id": "f476f503-8778-40d2-8239-72015901fffd", "userId": "0414af69-f89c-40f2-bea9-a9b5d0a179d4", "metadata": {"createdDate": "2022-05-06T13:16:33.142", "updatedDate": "2022-05-06T13:16:33.142+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
3b751c8c-e7b2-4cfe-8df7-a80f691258d8	{"id": "3b751c8c-e7b2-4cfe-8df7-a80f691258d8", "userId": "cd5994cf-6ee5-49b4-b58e-7bc70d724626", "metadata": {"createdDate": "2022-05-06T13:16:33.154", "updatedDate": "2022-05-06T13:16:33.154+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
7650f026-3710-4c1a-aa04-73c72d90dee5	{"id": "7650f026-3710-4c1a-aa04-73c72d90dee5", "userId": "f6d2c74c-3181-4c25-9a21-1b1f4c30765f", "metadata": {"createdDate": "2022-05-06T13:16:33.165", "updatedDate": "2022-05-06T13:16:33.165+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
49c4fd85-fe60-4e80-a1a0-744fc80061ad	{"id": "49c4fd85-fe60-4e80-a1a0-744fc80061ad", "userId": "8af5eaca-d164-4a5f-9941-0467c6facffa", "metadata": {"createdDate": "2022-05-06T13:16:33.179", "updatedDate": "2022-05-06T13:16:33.179+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
4384e0dc-df57-441d-83d5-e1950dfed923	{"id": "4384e0dc-df57-441d-83d5-e1950dfed923", "userId": "d0fc4228-2e42-49b2-a5b0-9df48897e8c0", "metadata": {"createdDate": "2022-05-06T13:16:33.199", "updatedDate": "2022-05-06T13:16:33.199+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
6e7120eb-1610-4bf3-8c72-35b6a61a3219	{"id": "6e7120eb-1610-4bf3-8c72-35b6a61a3219", "userId": "10cb9367-c095-4872-9add-8aecdf339dd4", "metadata": {"createdDate": "2022-05-06T13:16:33.216", "updatedDate": "2022-05-06T13:16:33.216+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
79675f8f-b4a1-4c71-bfe6-f4632e87e664	{"id": "79675f8f-b4a1-4c71-bfe6-f4632e87e664", "userId": "50e30476-ee93-4b16-a53c-27ce2c4b49d7", "metadata": {"createdDate": "2022-05-06T13:16:33.229", "updatedDate": "2022-05-06T13:16:33.229+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
e2ebdb5f-82cc-4a9a-babb-ccb16d585017	{"id": "e2ebdb5f-82cc-4a9a-babb-ccb16d585017", "userId": "860b2291-c28a-4943-804a-169af01edef4", "metadata": {"createdDate": "2022-05-06T13:16:33.241", "updatedDate": "2022-05-06T13:16:33.241+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
b8871fdf-f461-4371-90e7-9f6e129e4de7	{"id": "b8871fdf-f461-4371-90e7-9f6e129e4de7", "userId": "4205e8ff-804d-45bb-9d6d-f75f845ce608", "metadata": {"createdDate": "2022-05-06T13:16:33.25", "updatedDate": "2022-05-06T13:16:33.250+00:00"}, "permissions": []}	2000-01-01 13:16:33.25	\N
590a06a8-9769-4ada-bc7c-d0e776add1ed	{"id": "590a06a8-9769-4ada-bc7c-d0e776add1ed", "userId": "67002fdf-b2f6-4e1f-bab8-d750efb0558f", "metadata": {"createdDate": "2022-05-06T13:16:33.256", "updatedDate": "2022-05-06T13:16:33.256+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
256b57ec-eb4a-4518-96ab-a3ca443171cb	{"id": "256b57ec-eb4a-4518-96ab-a3ca443171cb", "userId": "2220260d-12c7-49ab-9ac4-f923323f0cb3", "metadata": {"createdDate": "2022-05-06T13:16:33.274", "updatedDate": "2022-05-06T13:16:33.274+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
e0aebcbe-b8bd-40d9-977f-56d201422443	{"id": "e0aebcbe-b8bd-40d9-977f-56d201422443", "userId": "15fa3eda-f495-496c-b21e-4f281b38a3ef", "metadata": {"createdDate": "2022-05-06T13:16:33.286", "updatedDate": "2022-05-06T13:16:33.286+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
1f38d0ef-b110-4cb1-80c9-294fef882808	{"id": "1f38d0ef-b110-4cb1-80c9-294fef882808", "userId": "8d65692e-fa98-49f2-9896-f9f6b0893358", "metadata": {"createdDate": "2022-05-06T13:16:33.297", "updatedDate": "2022-05-06T13:16:33.297+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
9c96b09c-db60-4f56-ba84-62aff0a93e35	{"id": "9c96b09c-db60-4f56-ba84-62aff0a93e35", "userId": "1db3d6c7-6ac5-4b3c-b860-deb2df449736", "metadata": {"createdDate": "2022-05-06T13:16:33.311", "updatedDate": "2022-05-06T13:16:33.311+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
c1f3612d-5595-4a04-81cc-bf4b683381e8	{"id": "c1f3612d-5595-4a04-81cc-bf4b683381e8", "userId": "be2e9bdb-9884-4fe9-87d0-ba91e8425412", "metadata": {"createdDate": "2022-05-06T13:16:33.323", "updatedDate": "2022-05-06T13:16:33.323+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
1b80a470-4672-4fbc-abda-f20494c48343	{"id": "1b80a470-4672-4fbc-abda-f20494c48343", "userId": "5f9bb63a-66f1-47eb-bc19-f182af2fc3e7", "metadata": {"createdDate": "2022-05-06T13:16:33.33", "updatedDate": "2022-05-06T13:16:33.330+00:00"}, "permissions": []}	2000-01-01 13:16:33.33	\N
5bd3d4a0-060c-46fb-a4ad-f539d82eec0f	{"id": "5bd3d4a0-060c-46fb-a4ad-f539d82eec0f", "userId": "488d4776-d0e2-4618-9ca9-78fa5dcc787c", "metadata": {"createdDate": "2022-05-06T13:16:33.337", "updatedDate": "2022-05-06T13:16:33.337+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
f7f51617-7867-4883-92aa-541aee936b73	{"id": "f7f51617-7867-4883-92aa-541aee936b73", "userId": "1c65f9c5-5970-48bb-aa72-82ef96fc145e", "metadata": {"createdDate": "2022-05-06T13:16:33.349", "updatedDate": "2022-05-06T13:16:33.349+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
e4a53955-825f-4c1a-8de5-3160b8dc8e25	{"id": "e4a53955-825f-4c1a-8de5-3160b8dc8e25", "userId": "a23eac4b-955e-451c-b4ff-6ec2f5e63e23", "metadata": {"createdDate": "2022-05-06T13:16:33.356", "updatedDate": "2022-05-06T13:16:33.356+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
86567988-f139-4888-a667-35b59acc749c	{"id": "86567988-f139-4888-a667-35b59acc749c", "userId": "c8c1eced-7ff5-44e2-89da-12d276c1e2bc", "metadata": {"createdDate": "2022-05-06T13:16:33.361", "updatedDate": "2022-05-06T13:16:33.361+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
5db81254-1a44-4275-a9d3-c5f90a71a3e9	{"id": "5db81254-1a44-4275-a9d3-c5f90a71a3e9", "userId": "2ee07dc7-835f-4a33-a783-db2ae3f1238c", "metadata": {"createdDate": "2022-05-06T13:16:33.374", "updatedDate": "2022-05-06T13:16:33.374+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
8ef30a20-a5ba-4189-a572-f40ea78bb002	{"id": "8ef30a20-a5ba-4189-a572-f40ea78bb002", "userId": "65fcc41e-df15-459a-bf93-2f53cfa8ff7f", "metadata": {"createdDate": "2022-05-06T13:16:33.379", "updatedDate": "2022-05-06T13:16:33.379+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
96d4d78c-9a7f-4e57-bfde-9b674d26568e	{"id": "96d4d78c-9a7f-4e57-bfde-9b674d26568e", "userId": "ea3a1605-a930-4183-b04e-0b2fca3ae094", "metadata": {"createdDate": "2022-05-06T13:16:33.393", "updatedDate": "2022-05-06T13:16:33.393+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
7be1f2bc-502e-48ea-bf51-2aa86ada6175	{"id": "7be1f2bc-502e-48ea-bf51-2aa86ada6175", "userId": "137d4cbd-00ff-4332-97cb-88373fa9b556", "metadata": {"createdDate": "2022-05-06T13:16:33.404", "updatedDate": "2022-05-06T13:16:33.404+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
a5e6dfd8-4d1b-4035-9d50-53165dab9049	{"id": "a5e6dfd8-4d1b-4035-9d50-53165dab9049", "userId": "888a321d-676e-42fc-b588-a677d16a76ec", "metadata": {"createdDate": "2022-05-06T13:16:33.414", "updatedDate": "2022-05-06T13:16:33.414+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
f6f2f391-3bad-4746-bd2c-22665428b12b	{"id": "f6f2f391-3bad-4746-bd2c-22665428b12b", "userId": "15b9deaf-1a59-4396-a8e5-d6c7e5b79b28", "metadata": {"createdDate": "2022-05-06T13:16:33.422", "updatedDate": "2022-05-06T13:16:33.422+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
5af1dffd-c5e6-4601-aa80-cea6804ba577	{"id": "5af1dffd-c5e6-4601-aa80-cea6804ba577", "userId": "6f511507-dab9-42fb-b966-bb8a1330ee7a", "metadata": {"createdDate": "2022-05-06T13:16:33.43", "updatedDate": "2022-05-06T13:16:33.430+00:00"}, "permissions": []}	2000-01-01 13:16:33.43	\N
464627bc-bdc8-4cab-b646-309e6f3f9209	{"id": "464627bc-bdc8-4cab-b646-309e6f3f9209", "userId": "c2610e2c-a6f8-4336-95b6-54d716348b03", "metadata": {"createdDate": "2022-05-06T13:16:33.437", "updatedDate": "2022-05-06T13:16:33.437+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
d6ad0400-2b1b-4c01-86bb-980fc041684f	{"id": "d6ad0400-2b1b-4c01-86bb-980fc041684f", "userId": "0e64adb1-36ba-4cdd-9909-047612b7629e", "metadata": {"createdDate": "2022-05-06T13:16:33.446", "updatedDate": "2022-05-06T13:16:33.446+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
5bceafec-1bfc-44c2-b74a-ac619e85af62	{"id": "5bceafec-1bfc-44c2-b74a-ac619e85af62", "userId": "71f4828b-8ad5-4ae6-bfa6-45ecfe3f6c3c", "metadata": {"createdDate": "2022-05-06T13:16:33.453", "updatedDate": "2022-05-06T13:16:33.453+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
e759ec7e-c468-4a15-80c8-28f6361bf719	{"id": "e759ec7e-c468-4a15-80c8-28f6361bf719", "userId": "b7da16ef-f3d2-4c12-a564-858bc3eee366", "metadata": {"createdDate": "2022-05-06T13:16:33.46", "updatedDate": "2022-05-06T13:16:33.460+00:00"}, "permissions": []}	2000-01-01 13:16:33.46	\N
c31b1725-8619-41af-ab6a-c8b2023c7aeb	{"id": "c31b1725-8619-41af-ab6a-c8b2023c7aeb", "userId": "550a06c3-8d0c-4ae3-a267-b32527272772", "metadata": {"createdDate": "2022-05-06T13:16:33.467", "updatedDate": "2022-05-06T13:16:33.467+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
02ba420a-93e2-44ce-879a-e9bbc75d274c	{"id": "02ba420a-93e2-44ce-879a-e9bbc75d274c", "userId": "5e3d70ff-a89a-44a0-a2e2-4cae67668805", "metadata": {"createdDate": "2022-05-06T13:16:33.479", "updatedDate": "2022-05-06T13:16:33.479+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
b7f6b16f-f6cb-499f-ad81-bdf03004f85a	{"id": "b7f6b16f-f6cb-499f-ad81-bdf03004f85a", "userId": "4adf499e-c954-4bf9-9261-26720608e120", "metadata": {"createdDate": "2022-05-06T13:16:33.489", "updatedDate": "2022-05-06T13:16:33.489+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
4e85ad5b-e051-4b6e-89fe-6d201abc6ab7	{"id": "4e85ad5b-e051-4b6e-89fe-6d201abc6ab7", "userId": "261e1062-a473-47f4-a00f-a197c4a87530", "metadata": {"createdDate": "2022-05-06T13:16:33.497", "updatedDate": "2022-05-06T13:16:33.497+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
5b1cb7bf-9be9-4738-b230-07384cc62d94	{"id": "5b1cb7bf-9be9-4738-b230-07384cc62d94", "userId": "4fd8d3dd-ebc0-4d10-ae81-199e831be32e", "metadata": {"createdDate": "2022-05-06T13:16:33.506", "updatedDate": "2022-05-06T13:16:33.506+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
79845a3f-0b0e-42ca-8eba-cf3341b51212	{"id": "79845a3f-0b0e-42ca-8eba-cf3341b51212", "userId": "78284bd0-cdf1-4fc9-a404-739388b41cc7", "metadata": {"createdDate": "2022-05-06T13:16:33.514", "updatedDate": "2022-05-06T13:16:33.514+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
76a26609-a44a-41fd-b7be-5ac6d546d112	{"id": "76a26609-a44a-41fd-b7be-5ac6d546d112", "userId": "c97f19d3-7a82-4891-aaa1-de087a9a903f", "metadata": {"createdDate": "2022-05-06T13:16:33.518", "updatedDate": "2022-05-06T13:16:33.518+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
8d311184-6223-4036-9f31-cd149403e78d	{"id": "8d311184-6223-4036-9f31-cd149403e78d", "userId": "7b06cbcf-5d6d-431b-8922-20509d40f1ae", "metadata": {"createdDate": "2022-05-06T13:16:33.527", "updatedDate": "2022-05-06T13:16:33.527+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
0a1d7eb5-11e2-476f-aa39-6b673b3fe75e	{"id": "0a1d7eb5-11e2-476f-aa39-6b673b3fe75e", "userId": "1dbf36ab-bba6-4725-8456-bda646796dd1", "metadata": {"createdDate": "2022-05-06T13:16:33.538", "updatedDate": "2022-05-06T13:16:33.538+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
c8d0ac17-06ce-4cab-885b-6c207f996eff	{"id": "c8d0ac17-06ce-4cab-885b-6c207f996eff", "userId": "342971e4-43af-44c3-a8c3-478a97cc94bc", "metadata": {"createdDate": "2022-05-06T13:16:33.548", "updatedDate": "2022-05-06T13:16:33.548+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
b82c3c8a-ecc0-4233-95ce-3ea2b9a9b28f	{"id": "b82c3c8a-ecc0-4233-95ce-3ea2b9a9b28f", "userId": "8853c9a2-cae2-4b5e-84ce-2b39bb809e5b", "metadata": {"createdDate": "2022-05-06T13:16:33.552", "updatedDate": "2022-05-06T13:16:33.552+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
d30c74c5-6dfd-4423-beca-a022e132ec32	{"id": "d30c74c5-6dfd-4423-beca-a022e132ec32", "userId": "08522da4-668a-4450-a769-3abfae5678ad", "metadata": {"createdDate": "2022-05-06T13:16:33.574", "updatedDate": "2022-05-06T13:16:33.574+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
5f4fa72c-2924-4a3e-93fc-7fbfee6eaae8	{"id": "5f4fa72c-2924-4a3e-93fc-7fbfee6eaae8", "userId": "daa9cf25-f333-447b-b577-158d6ce944a5", "metadata": {"createdDate": "2022-05-06T13:16:33.585", "updatedDate": "2022-05-06T13:16:33.585+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
86429896-d4b7-4bf2-b524-7aa3402a085b	{"id": "86429896-d4b7-4bf2-b524-7aa3402a085b", "userId": "69a7d4f8-a32a-46d8-a006-0e5ea69f34bc", "metadata": {"createdDate": "2022-05-06T13:16:33.6", "updatedDate": "2022-05-06T13:16:33.600+00:00"}, "permissions": []}	2000-01-01 13:16:33.6	\N
0874fc61-697f-47ab-ab45-19aa7018ceb7	{"id": "0874fc61-697f-47ab-ab45-19aa7018ceb7", "userId": "21c08ac3-c287-4a21-b966-e263504aa773", "metadata": {"createdDate": "2022-05-06T13:16:33.615", "updatedDate": "2022-05-06T13:16:33.615+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
3dff3ade-a165-4c06-83ce-b1bcf0bd49bd	{"id": "3dff3ade-a165-4c06-83ce-b1bcf0bd49bd", "userId": "bb4d8818-35cc-4cb6-b181-b5ccfb734744", "metadata": {"createdDate": "2022-05-06T13:16:33.381", "updatedDate": "2022-05-06T13:16:33.381+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
e4642560-18f7-4906-90c3-f2f7b2dec7f2	{"id": "e4642560-18f7-4906-90c3-f2f7b2dec7f2", "userId": "4f0e711c-d583-41e0-9555-b62f1725023f", "metadata": {"createdDate": "2022-05-06T13:16:33.389", "updatedDate": "2022-05-06T13:16:33.389+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
b845fbb3-323c-4ffd-93de-7bb2966efe61	{"id": "b845fbb3-323c-4ffd-93de-7bb2966efe61", "userId": "6f644096-0cb6-4d9c-9da4-0831b3625c0d", "metadata": {"createdDate": "2022-05-06T13:16:33.399", "updatedDate": "2022-05-06T13:16:33.399+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
c8bd609d-a6bf-4539-b91f-460c427fac91	{"id": "c8bd609d-a6bf-4539-b91f-460c427fac91", "userId": "2eb8fef6-95c8-491d-a6a3-00176997dca4", "metadata": {"createdDate": "2022-05-06T13:16:33.407", "updatedDate": "2022-05-06T13:16:33.407+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
61037b68-50e1-4861-a380-c77992bfe5f4	{"id": "61037b68-50e1-4861-a380-c77992bfe5f4", "userId": "f0dc6802-450f-459a-9dc6-209086375b7f", "metadata": {"createdDate": "2022-05-06T13:16:33.419", "updatedDate": "2022-05-06T13:16:33.419+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
00278edf-4458-4385-9462-5ce1ce4aec9e	{"id": "00278edf-4458-4385-9462-5ce1ce4aec9e", "userId": "1b5795ad-5ad0-4ba5-9c62-a7b26eb2f6b8", "metadata": {"createdDate": "2022-05-06T13:16:33.432", "updatedDate": "2022-05-06T13:16:33.432+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
b61c0204-0ff6-4ec3-a991-68de9a694afc	{"id": "b61c0204-0ff6-4ec3-a991-68de9a694afc", "userId": "7aa8082c-d1ed-4e33-bf0e-02d3efe5624b", "metadata": {"createdDate": "2022-05-06T13:16:33.451", "updatedDate": "2022-05-06T13:16:33.451+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
ce80c7f2-0877-401b-b5eb-aabb155789da	{"id": "ce80c7f2-0877-401b-b5eb-aabb155789da", "userId": "d80b45eb-5dc0-4635-b539-dac722cc3a50", "metadata": {"createdDate": "2022-05-06T13:16:33.458", "updatedDate": "2022-05-06T13:16:33.458+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
571b6e29-c55a-4184-9243-bc53fa863b94	{"id": "571b6e29-c55a-4184-9243-bc53fa863b94", "userId": "c7d6c761-905e-4e7b-a616-a624175efe11", "metadata": {"createdDate": "2022-05-06T13:16:33.465", "updatedDate": "2022-05-06T13:16:33.465+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
1f39871d-ba79-4cf8-acb3-3bfdf8ca24c7	{"id": "1f39871d-ba79-4cf8-acb3-3bfdf8ca24c7", "userId": "785c6f6e-36a5-4434-8aa7-210bb55cea34", "metadata": {"createdDate": "2022-05-06T13:16:33.475", "updatedDate": "2022-05-06T13:16:33.475+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
07401202-8ec5-44b7-b828-fce7083e6a48	{"id": "07401202-8ec5-44b7-b828-fce7083e6a48", "userId": "1b648069-8563-41c8-afc0-d8359f11503c", "metadata": {"createdDate": "2022-05-06T13:16:33.48", "updatedDate": "2022-05-06T13:16:33.480+00:00"}, "permissions": []}	2000-01-01 13:16:33.48	\N
0e6f2647-5f9a-4ae7-a8d8-91c7d8caf4f2	{"id": "0e6f2647-5f9a-4ae7-a8d8-91c7d8caf4f2", "userId": "292451b5-0026-4463-a762-d43fc6cc9122", "metadata": {"createdDate": "2022-05-06T13:16:33.493", "updatedDate": "2022-05-06T13:16:33.493+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
4eeddb87-74a8-43fe-bc76-bc551d03b56d	{"id": "4eeddb87-74a8-43fe-bc76-bc551d03b56d", "userId": "e1bcf784-484c-42ca-b502-cf2f2e57eca3", "metadata": {"createdDate": "2022-05-06T13:16:33.502", "updatedDate": "2022-05-06T13:16:33.502+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
7632ae2d-e6f3-41f2-85c2-521af8f70bd3	{"id": "7632ae2d-e6f3-41f2-85c2-521af8f70bd3", "userId": "ffce08d4-c08d-4ff8-8ff8-060a5015aa2a", "metadata": {"createdDate": "2022-05-06T13:16:33.51", "updatedDate": "2022-05-06T13:16:33.510+00:00"}, "permissions": []}	2000-01-01 13:16:33.51	\N
8d8d4250-34b7-49f8-bcca-709a26d71e33	{"id": "8d8d4250-34b7-49f8-bcca-709a26d71e33", "userId": "2084e201-b0da-4ac3-b3ae-873c48596093", "metadata": {"createdDate": "2022-05-06T13:16:33.515", "updatedDate": "2022-05-06T13:16:33.515+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
83549201-5a33-46b8-aa17-cee5f8ccd4dd	{"id": "83549201-5a33-46b8-aa17-cee5f8ccd4dd", "userId": "f308aadb-9403-44de-9b5a-06792b78bb3a", "metadata": {"createdDate": "2022-05-06T13:16:33.532", "updatedDate": "2022-05-06T13:16:33.532+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
25b2af97-3e0f-40bd-b8ef-ca67199bf1fe	{"id": "25b2af97-3e0f-40bd-b8ef-ca67199bf1fe", "userId": "975256dc-abdc-45d1-b51a-f9f9ca15a491", "metadata": {"createdDate": "2022-05-06T13:16:33.545", "updatedDate": "2022-05-06T13:16:33.545+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
5a8e4d5a-0e7c-4eb3-a211-5c392431f635	{"id": "5a8e4d5a-0e7c-4eb3-a211-5c392431f635", "userId": "e25fecaf-dfbf-4e59-bd3d-0493c1b519f5", "metadata": {"createdDate": "2022-05-06T13:16:33.558", "updatedDate": "2022-05-06T13:16:33.558+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
1fc7305e-17eb-4bf0-a13a-5bc585798b04	{"id": "1fc7305e-17eb-4bf0-a13a-5bc585798b04", "userId": "368b8b4a-c3c2-436c-9cba-95dcac52ebf9", "metadata": {"createdDate": "2022-05-06T13:16:33.572", "updatedDate": "2022-05-06T13:16:33.572+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
341a5f3b-7148-469f-aa83-4dfce1470dcf	{"id": "341a5f3b-7148-469f-aa83-4dfce1470dcf", "userId": "7ff26639-c033-442a-8bf4-2e896b17fcf9", "metadata": {"createdDate": "2022-05-06T13:16:33.59", "updatedDate": "2022-05-06T13:16:33.590+00:00"}, "permissions": []}	2000-01-01 13:16:33.59	\N
0f7f64d5-56cc-47b2-8f83-f832c35de5d7	{"id": "0f7f64d5-56cc-47b2-8f83-f832c35de5d7", "userId": "169aeb77-32f6-4f60-a2c0-db791b96e411", "metadata": {"createdDate": "2022-05-06T13:16:33.606", "updatedDate": "2022-05-06T13:16:33.606+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
9f05ee38-10aa-41ca-80ac-f4667749285c	{"id": "9f05ee38-10aa-41ca-80ac-f4667749285c", "userId": "430f49de-6848-4cba-886a-4902cb9b887d", "metadata": {"createdDate": "2022-05-06T13:16:33.62", "updatedDate": "2022-05-06T13:16:33.620+00:00"}, "permissions": []}	2000-01-01 13:16:33.62	\N
e5fcb19f-7339-4bbb-9bf5-7420c5ab4b98	{"id": "e5fcb19f-7339-4bbb-9bf5-7420c5ab4b98", "userId": "6f4111a4-8b6f-4008-9b95-ecd31db69234", "metadata": {"createdDate": "2022-05-06T13:16:33.633", "updatedDate": "2022-05-06T13:16:33.633+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
6b698db1-85ba-4923-9ba7-a2a3e6e0a80c	{"id": "6b698db1-85ba-4923-9ba7-a2a3e6e0a80c", "userId": "a208cf17-a7f0-452d-ae0e-64011232c86d", "metadata": {"createdDate": "2022-05-06T13:16:33.644", "updatedDate": "2022-05-06T13:16:33.644+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
61707238-32d4-43f5-a0a6-a83463a8adac	{"id": "61707238-32d4-43f5-a0a6-a83463a8adac", "userId": "201be44f-2f29-47af-85da-2cbfc72ac29e", "metadata": {"createdDate": "2022-05-06T13:16:33.657", "updatedDate": "2022-05-06T13:16:33.657+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
66b07da6-692a-4b94-bab2-06211264a87c	{"id": "66b07da6-692a-4b94-bab2-06211264a87c", "userId": "f3055954-ebc3-4da8-8a60-0b8f52480125", "metadata": {"createdDate": "2022-05-06T13:16:33.668", "updatedDate": "2022-05-06T13:16:33.668+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
a20fd337-a840-4d78-8078-7efe0792c985	{"id": "a20fd337-a840-4d78-8078-7efe0792c985", "userId": "d1f69036-a316-41e4-89c1-77f77a3c7f1d", "metadata": {"createdDate": "2022-05-06T13:16:33.677", "updatedDate": "2022-05-06T13:16:33.677+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
2d6d51cb-85f7-4c55-95c3-e454eef059a3	{"id": "2d6d51cb-85f7-4c55-95c3-e454eef059a3", "userId": "ec97250b-1ded-46a7-a8f6-a474f8fe622d", "metadata": {"createdDate": "2022-05-06T13:16:33.411", "updatedDate": "2022-05-06T13:16:33.411+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
59565b13-5005-4f8b-9b9b-f22c84c7ce3f	{"id": "59565b13-5005-4f8b-9b9b-f22c84c7ce3f", "userId": "2c9e8cdd-d2fe-485b-b663-34225637fe93", "metadata": {"createdDate": "2022-05-06T13:16:33.425", "updatedDate": "2022-05-06T13:16:33.425+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
5894f56c-72d9-44db-ae69-c24ff9604d1f	{"id": "5894f56c-72d9-44db-ae69-c24ff9604d1f", "userId": "f6cd72ab-3e89-44c0-99aa-54ab4844f167", "metadata": {"createdDate": "2022-05-06T13:16:33.441", "updatedDate": "2022-05-06T13:16:33.441+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
29149095-9896-447d-8485-295c2a722dc5	{"id": "29149095-9896-447d-8485-295c2a722dc5", "userId": "5dfffe75-267d-4133-b7bf-6d6daf26d5a4", "metadata": {"createdDate": "2022-05-06T13:16:33.471", "updatedDate": "2022-05-06T13:16:33.471+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
cc13513a-9550-4751-b4b0-5c1660a2b906	{"id": "cc13513a-9550-4751-b4b0-5c1660a2b906", "userId": "c6d99127-e834-4f60-9a77-f74646cd1618", "metadata": {"createdDate": "2022-05-06T13:16:33.485", "updatedDate": "2022-05-06T13:16:33.485+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
cfe1980c-be5c-49a3-af44-d660042d9bb9	{"id": "cfe1980c-be5c-49a3-af44-d660042d9bb9", "userId": "872695bb-4157-4c6f-84c7-eb5b50b9ce17", "metadata": {"createdDate": "2022-05-06T13:16:33.492", "updatedDate": "2022-05-06T13:16:33.492+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
f0992d2a-4b04-4c29-8757-74258c73c4c7	{"id": "f0992d2a-4b04-4c29-8757-74258c73c4c7", "userId": "6c6ab6f6-394a-44a5-8d5c-66f88f9ec01d", "metadata": {"createdDate": "2022-05-06T13:16:33.505", "updatedDate": "2022-05-06T13:16:33.505+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
3d0e2d5f-c41e-401c-8781-6c6179008106	{"id": "3d0e2d5f-c41e-401c-8781-6c6179008106", "userId": "745bdee1-458c-4076-bad1-be5a470c49fb", "metadata": {"createdDate": "2022-05-06T13:16:33.522", "updatedDate": "2022-05-06T13:16:33.522+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
ce79d9aa-f815-4ce5-b606-4088603b3776	{"id": "ce79d9aa-f815-4ce5-b606-4088603b3776", "userId": "1bcfd501-232e-47da-a511-fdd29ae3d692", "metadata": {"createdDate": "2022-05-06T13:16:33.534", "updatedDate": "2022-05-06T13:16:33.534+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
aa61d65f-4a21-4177-b117-d0103225f09c	{"id": "aa61d65f-4a21-4177-b117-d0103225f09c", "userId": "6ff36aa8-c68d-42c9-b68b-ece603ea59d7", "metadata": {"createdDate": "2022-05-06T13:16:33.554", "updatedDate": "2022-05-06T13:16:33.554+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
9bc03b5c-4b85-4f0f-b6fa-41abd7c71b85	{"id": "9bc03b5c-4b85-4f0f-b6fa-41abd7c71b85", "userId": "b9a05706-9d87-499d-8e5e-47dc512a21c3", "metadata": {"createdDate": "2022-05-06T13:16:33.569", "updatedDate": "2022-05-06T13:16:33.569+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
163a2aa6-f3f8-4dff-b69a-7fc965eb8f62	{"id": "163a2aa6-f3f8-4dff-b69a-7fc965eb8f62", "userId": "f046c9bd-45aa-4ab1-ad3f-461ead3dfdc1", "metadata": {"createdDate": "2022-05-06T13:16:33.58", "updatedDate": "2022-05-06T13:16:33.580+00:00"}, "permissions": []}	2000-01-01 13:16:33.58	\N
45afa7b5-9af7-478d-9ecf-d115ca841304	{"id": "45afa7b5-9af7-478d-9ecf-d115ca841304", "userId": "ef39251f-db4c-4253-9951-645735f84904", "metadata": {"createdDate": "2022-05-06T13:16:33.595", "updatedDate": "2022-05-06T13:16:33.595+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
be7ff008-4d00-4ecd-a3a5-1e0eebfc1bdf	{"id": "be7ff008-4d00-4ecd-a3a5-1e0eebfc1bdf", "userId": "b3dae815-3d30-49f9-ac26-363e661382a0", "metadata": {"createdDate": "2022-05-06T13:16:33.61", "updatedDate": "2022-05-06T13:16:33.610+00:00"}, "permissions": []}	2000-01-01 13:16:33.61	\N
1ff6cda3-5b5f-4a89-a58c-28a74257bc7b	{"id": "1ff6cda3-5b5f-4a89-a58c-28a74257bc7b", "userId": "94fc2d88-359e-45e1-8360-ff6fb132cac4", "metadata": {"createdDate": "2022-05-06T13:16:33.626", "updatedDate": "2022-05-06T13:16:33.626+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
3e0034fa-6733-4215-b3aa-af91fea06549	{"id": "3e0034fa-6733-4215-b3aa-af91fea06549", "userId": "b56b28b9-7f22-426e-a099-4f753be686fa", "metadata": {"createdDate": "2022-05-06T13:16:33.636", "updatedDate": "2022-05-06T13:16:33.636+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
a82f37d8-92b1-4a94-9738-d5c8a74515df	{"id": "a82f37d8-92b1-4a94-9738-d5c8a74515df", "userId": "e13b3d8d-71ff-49bd-9ea1-4ad7da8b1b8e", "metadata": {"createdDate": "2022-05-06T13:16:33.652", "updatedDate": "2022-05-06T13:16:33.652+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
8223807c-f7ec-4682-a5c9-9b40a40f294e	{"id": "8223807c-f7ec-4682-a5c9-9b40a40f294e", "userId": "34a22ca8-9aff-4b1c-96c2-f908ddb068ae", "metadata": {"createdDate": "2022-05-06T13:16:33.669", "updatedDate": "2022-05-06T13:16:33.669+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
6af79d7b-8d6d-4ae2-a381-1753f35e81e4	{"id": "6af79d7b-8d6d-4ae2-a381-1753f35e81e4", "userId": "48a3115d-d476-4582-b6a8-55c09eed7ec7", "metadata": {"createdDate": "2022-05-06T13:16:33.687", "updatedDate": "2022-05-06T13:16:33.687+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
cbe510a5-980e-4b3c-afed-5f5ecf6a7959	{"id": "cbe510a5-980e-4b3c-afed-5f5ecf6a7959", "userId": "ff5ec271-7138-46d0-8ca1-f5f57790e5bd", "metadata": {"createdDate": "2022-05-06T13:16:33.699", "updatedDate": "2022-05-06T13:16:33.699+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
1a940803-f2af-447d-ae77-9cac4d0cdafc	{"id": "1a940803-f2af-447d-ae77-9cac4d0cdafc", "userId": "22b0e29a-cc5d-456b-b272-b521ad5d2a39", "metadata": {"createdDate": "2022-05-06T13:16:33.714", "updatedDate": "2022-05-06T13:16:33.714+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
9a354356-e074-449d-a5d0-ea5cf1ff9e6f	{"id": "9a354356-e074-449d-a5d0-ea5cf1ff9e6f", "userId": "fad510e6-5b8d-4b10-b846-ce6ff7457629", "metadata": {"createdDate": "2022-05-06T13:16:33.725", "updatedDate": "2022-05-06T13:16:33.725+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
b563e144-a6f7-4ad7-a310-e9f474dd26ea	{"id": "b563e144-a6f7-4ad7-a310-e9f474dd26ea", "userId": "a7fb2289-b4dc-4deb-8fd3-86cf8e2b7db6", "metadata": {"createdDate": "2022-05-06T13:16:33.735", "updatedDate": "2022-05-06T13:16:33.735+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
637cdab2-ca4b-4c73-bb5c-a47043120fa1	{"id": "637cdab2-ca4b-4c73-bb5c-a47043120fa1", "userId": "54f65a75-f35b-4f56-86a6-fa4a3d957e57", "metadata": {"createdDate": "2022-05-06T13:16:33.749", "updatedDate": "2022-05-06T13:16:33.749+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
0bb07efe-e75c-46b4-9c37-cd925f09e7cd	{"id": "0bb07efe-e75c-46b4-9c37-cd925f09e7cd", "userId": "42e9d211-4bfb-45fe-a088-f19d0a514f98", "metadata": {"createdDate": "2022-05-06T13:16:33.777", "updatedDate": "2022-05-06T13:16:33.777+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
068be1c2-a3e5-4eae-875e-21108a17f3d8	{"id": "068be1c2-a3e5-4eae-875e-21108a17f3d8", "userId": "b4fa5d79-4af6-4623-9331-fabdfee79e0c", "metadata": {"createdDate": "2022-05-06T13:16:33.803", "updatedDate": "2022-05-06T13:16:33.803+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
4c08ee8c-5d8d-40bd-b9a5-6500167f6dc7	{"id": "4c08ee8c-5d8d-40bd-b9a5-6500167f6dc7", "userId": "4cb9a24c-76e1-4755-9f54-f51115e00b53", "metadata": {"createdDate": "2022-05-06T13:16:33.826", "updatedDate": "2022-05-06T13:16:33.826+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
2bddfd7f-cc2a-480a-abc0-9a5ea3c104b4	{"id": "2bddfd7f-cc2a-480a-abc0-9a5ea3c104b4", "userId": "d6c40971-39a6-4977-9d27-e83e731f51de", "metadata": {"createdDate": "2022-05-06T13:16:33.498", "updatedDate": "2022-05-06T13:16:33.498+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
184d9000-438a-4a7e-a134-08ffbf465b09	{"id": "184d9000-438a-4a7e-a134-08ffbf465b09", "userId": "960ab857-c8c7-4445-8d24-1e4c33de3e86", "metadata": {"createdDate": "2022-05-06T13:16:33.52", "updatedDate": "2022-05-06T13:16:33.520+00:00"}, "permissions": []}	2000-01-01 13:16:33.52	\N
c7b2be8f-cf4d-46b1-9f7d-c08d93449bb2	{"id": "c7b2be8f-cf4d-46b1-9f7d-c08d93449bb2", "userId": "4f012e5c-840b-4f7a-b7e0-c2e3b1d41309", "metadata": {"createdDate": "2022-05-06T13:16:33.533", "updatedDate": "2022-05-06T13:16:33.533+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
ff8afdd5-7c48-4f85-8a2a-3a3e913ab195	{"id": "ff8afdd5-7c48-4f85-8a2a-3a3e913ab195", "userId": "21ce4a36-4d3a-4a9d-98be-d40852799d9b", "metadata": {"createdDate": "2022-05-06T13:16:33.542", "updatedDate": "2022-05-06T13:16:33.542+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
97567a3d-6630-41a0-982f-2ebbbdeb5f05	{"id": "97567a3d-6630-41a0-982f-2ebbbdeb5f05", "userId": "8e9d1a69-9745-4ad4-a8e8-6841a9441b40", "metadata": {"createdDate": "2022-05-06T13:16:33.556", "updatedDate": "2022-05-06T13:16:33.556+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
a73aa763-ce46-445e-a2da-e90c410e334e	{"id": "a73aa763-ce46-445e-a2da-e90c410e334e", "userId": "3387893c-7bde-4d2f-9ad2-d4974b3e959e", "metadata": {"createdDate": "2022-05-06T13:16:33.562", "updatedDate": "2022-05-06T13:16:33.562+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
6ca8040d-ed68-4581-a540-85efd38453eb	{"id": "6ca8040d-ed68-4581-a540-85efd38453eb", "userId": "197f8e91-5110-4487-ad36-b3d21a66059d", "metadata": {"createdDate": "2022-05-06T13:16:33.573", "updatedDate": "2022-05-06T13:16:33.573+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
4d156fe6-714d-44e1-87ec-55621a5f8b5a	{"id": "4d156fe6-714d-44e1-87ec-55621a5f8b5a", "userId": "c6a1f097-1292-441c-a760-682279a7f94c", "metadata": {"createdDate": "2022-05-06T13:16:33.593", "updatedDate": "2022-05-06T13:16:33.593+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
1411f547-964a-4e0b-b830-4ca95d0533ec	{"id": "1411f547-964a-4e0b-b830-4ca95d0533ec", "userId": "2338689d-f27e-49fd-8bce-9f9bf7be6ea0", "metadata": {"createdDate": "2022-05-06T13:16:33.612", "updatedDate": "2022-05-06T13:16:33.612+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
f919004e-715c-47fb-b823-1c3ae69c3fe7	{"id": "f919004e-715c-47fb-b823-1c3ae69c3fe7", "userId": "9e87bfea-2d31-4cc3-9cef-9e1e67553243", "metadata": {"createdDate": "2022-05-06T13:16:33.627", "updatedDate": "2022-05-06T13:16:33.627+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
3d22fc5d-bf76-410b-8bb6-b2f6c2d421aa	{"id": "3d22fc5d-bf76-410b-8bb6-b2f6c2d421aa", "userId": "223907c9-517b-4b10-a6bc-8f7fcb0a05c3", "metadata": {"createdDate": "2022-05-06T13:16:33.65", "updatedDate": "2022-05-06T13:16:33.650+00:00"}, "permissions": []}	2000-01-01 13:16:33.65	\N
672bda7b-ce9b-469c-9b94-7157da212f6a	{"id": "672bda7b-ce9b-469c-9b94-7157da212f6a", "userId": "a98abce3-be00-4d9a-a66a-0593d27b41e0", "metadata": {"createdDate": "2022-05-06T13:16:33.667", "updatedDate": "2022-05-06T13:16:33.667+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
f0e17c50-a88e-41b2-a7fe-c54c1b34ee27	{"id": "f0e17c50-a88e-41b2-a7fe-c54c1b34ee27", "userId": "e59bc21a-884e-42a2-8792-163efc3662e7", "metadata": {"createdDate": "2022-05-06T13:16:33.68", "updatedDate": "2022-05-06T13:16:33.680+00:00"}, "permissions": []}	2000-01-01 13:16:33.68	\N
abf36bac-4071-4450-802d-cb63e932a88e	{"id": "abf36bac-4071-4450-802d-cb63e932a88e", "userId": "78c51a90-e64f-49ce-8d28-e246a49c7f63", "metadata": {"createdDate": "2022-05-06T13:16:33.689", "updatedDate": "2022-05-06T13:16:33.689+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
0a928f15-db46-42eb-9596-371e21a7ea54	{"id": "0a928f15-db46-42eb-9596-371e21a7ea54", "userId": "a5fb6646-3e42-46ed-b686-e009e1490d2c", "metadata": {"createdDate": "2022-05-06T13:16:33.693", "updatedDate": "2022-05-06T13:16:33.693+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
c5378a71-78df-405e-b763-2524cb038a69	{"id": "c5378a71-78df-405e-b763-2524cb038a69", "userId": "fb5de3c8-5293-440e-8448-a688c3a7367c", "metadata": {"createdDate": "2022-05-06T13:16:33.701", "updatedDate": "2022-05-06T13:16:33.701+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
73c2879f-82e9-403a-bdff-28765ce80af2	{"id": "73c2879f-82e9-403a-bdff-28765ce80af2", "userId": "1200edd1-4b53-43e7-a9b7-fc590ab1c8d9", "metadata": {"createdDate": "2022-05-06T13:16:33.707", "updatedDate": "2022-05-06T13:16:33.707+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
f198beb9-56bb-4b0a-aaf0-f0332077a49a	{"id": "f198beb9-56bb-4b0a-aaf0-f0332077a49a", "userId": "2c2e383d-7369-4aff-afb7-eb3db4cb71a0", "metadata": {"createdDate": "2022-05-06T13:16:33.723", "updatedDate": "2022-05-06T13:16:33.723+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
67e44366-e45c-4210-bed0-99ac4821ca58	{"id": "67e44366-e45c-4210-bed0-99ac4821ca58", "userId": "b3f61b07-a1b3-44ac-bb7f-622b90ac17c3", "metadata": {"createdDate": "2022-05-06T13:16:33.733", "updatedDate": "2022-05-06T13:16:33.733+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
2f19f693-127e-44f4-a360-730eabc7c17b	{"id": "2f19f693-127e-44f4-a360-730eabc7c17b", "userId": "16757efe-86ac-40bb-bdd6-fafee02463c7", "metadata": {"createdDate": "2022-05-06T13:16:33.747", "updatedDate": "2022-05-06T13:16:33.747+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
00739098-a9ac-4b40-a52f-1e2a9bbc80d5	{"id": "00739098-a9ac-4b40-a52f-1e2a9bbc80d5", "userId": "a5a80ce1-c00d-4ede-ba44-912c1e093948", "metadata": {"createdDate": "2022-05-06T13:16:33.778", "updatedDate": "2022-05-06T13:16:33.778+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
2b5730d7-83be-43cd-9a45-a9c7e9271b4b	{"id": "2b5730d7-83be-43cd-9a45-a9c7e9271b4b", "userId": "67e40b72-66ca-4113-bed9-17a40bc448e0", "metadata": {"createdDate": "2022-05-06T13:16:33.805", "updatedDate": "2022-05-06T13:16:33.805+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
e2ab69bf-84c9-47c4-832c-0414da627f86	{"id": "e2ab69bf-84c9-47c4-832c-0414da627f86", "userId": "ab579dc3-219b-4f5b-8068-ab1c7a55c402", "metadata": {"createdDate": "2022-05-06T13:16:33.832", "updatedDate": "2022-05-06T13:16:33.832+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
25376595-e141-49d8-b7f4-fdc1bad27705	{"id": "25376595-e141-49d8-b7f4-fdc1bad27705", "userId": "e14b1bc1-8784-4b55-bfbe-e5ec8ce0b07a", "metadata": {"createdDate": "2022-05-06T13:16:33.852", "updatedDate": "2022-05-06T13:16:33.852+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
78695055-dbf4-437c-aecb-f69a370331c5	{"id": "78695055-dbf4-437c-aecb-f69a370331c5", "userId": "56708cfe-750e-49ad-b72a-003ce7ad78a4", "metadata": {"createdDate": "2022-05-06T13:16:33.866", "updatedDate": "2022-05-06T13:16:33.866+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
130a6d56-e319-4252-892c-ee322d129fca	{"id": "130a6d56-e319-4252-892c-ee322d129fca", "userId": "11dd4634-e4a9-45f0-9683-fa4d7a8f9728", "metadata": {"createdDate": "2022-05-06T13:16:33.889", "updatedDate": "2022-05-06T13:16:33.889+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
489e9fe8-921e-4b6f-a2d6-ae68f02d2797	{"id": "489e9fe8-921e-4b6f-a2d6-ae68f02d2797", "userId": "2cb8a9f5-5a04-4b26-89de-c5a522638de2", "metadata": {"createdDate": "2022-05-06T13:16:33.899", "updatedDate": "2022-05-06T13:16:33.899+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
74633b09-30ac-456c-9266-9a299429e4a6	{"id": "74633b09-30ac-456c-9266-9a299429e4a6", "userId": "c573e5f8-a570-475d-a75e-88a4d2b757d2", "metadata": {"createdDate": "2022-05-06T13:16:33.631", "updatedDate": "2022-05-06T13:16:33.631+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
59a7efbd-32cc-4fea-b9ab-6f4cf75fda48	{"id": "59a7efbd-32cc-4fea-b9ab-6f4cf75fda48", "userId": "b549fc60-9779-4bac-a4de-df8304ff69c4", "metadata": {"createdDate": "2022-05-06T13:16:33.651", "updatedDate": "2022-05-06T13:16:33.651+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
72b965ac-88b0-4871-a8a8-466d64db696e	{"id": "72b965ac-88b0-4871-a8a8-466d64db696e", "userId": "66fe5bd9-1129-4b40-b54d-05b4c358463c", "metadata": {"createdDate": "2022-05-06T13:16:33.664", "updatedDate": "2022-05-06T13:16:33.664+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
7c09f483-d6b9-4ae7-9a19-dae586f1d900	{"id": "7c09f483-d6b9-4ae7-9a19-dae586f1d900", "userId": "0a985a0a-b515-42a0-8ec2-1c2b7e8a1d8c", "metadata": {"createdDate": "2022-05-06T13:16:33.674", "updatedDate": "2022-05-06T13:16:33.674+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
eac0c23a-8789-4869-aa22-fb52aee72920	{"id": "eac0c23a-8789-4869-aa22-fb52aee72920", "userId": "ea2ef01f-d732-4119-90ab-ee6df447548f", "metadata": {"createdDate": "2022-05-06T13:16:33.681", "updatedDate": "2022-05-06T13:16:33.681+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
d6368023-7516-4f38-807b-8492e0849cf1	{"id": "d6368023-7516-4f38-807b-8492e0849cf1", "userId": "ac521a2a-d933-42f9-b3a4-2d7399880057", "metadata": {"createdDate": "2022-05-06T13:16:33.691", "updatedDate": "2022-05-06T13:16:33.691+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
40221c72-a390-4450-9d94-8dd33eba292b	{"id": "40221c72-a390-4450-9d94-8dd33eba292b", "userId": "bc048122-1914-4971-ab0f-62303fef71aa", "metadata": {"createdDate": "2022-05-06T13:16:33.703", "updatedDate": "2022-05-06T13:16:33.703+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
a6e238a1-82a0-4bcc-8080-6be4e137ea93	{"id": "a6e238a1-82a0-4bcc-8080-6be4e137ea93", "userId": "a56edfbe-087e-4f79-bd7e-d855fbe746e4", "metadata": {"createdDate": "2022-05-06T13:16:33.71", "updatedDate": "2022-05-06T13:16:33.710+00:00"}, "permissions": []}	2000-01-01 13:16:33.71	\N
7bd3dbb8-4a10-4d48-a73c-ca5cca72db54	{"id": "7bd3dbb8-4a10-4d48-a73c-ca5cca72db54", "userId": "c9255397-a8cb-4208-9558-1aae0e6f2c68", "metadata": {"createdDate": "2022-05-06T13:16:33.717", "updatedDate": "2022-05-06T13:16:33.717+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
4dcb82fe-77ca-42d7-939f-ab22bacaa876	{"id": "4dcb82fe-77ca-42d7-939f-ab22bacaa876", "userId": "acf8aab2-91ee-4210-bb7c-b688d66a9de4", "metadata": {"createdDate": "2022-05-06T13:16:33.73", "updatedDate": "2022-05-06T13:16:33.730+00:00"}, "permissions": []}	2000-01-01 13:16:33.73	\N
600c15a2-52e4-4e9d-9325-b7c3f2ee9444	{"id": "600c15a2-52e4-4e9d-9325-b7c3f2ee9444", "userId": "eaeffd06-57d3-488c-bd1b-c39d5c62e97d", "metadata": {"createdDate": "2022-05-06T13:16:33.74", "updatedDate": "2022-05-06T13:16:33.740+00:00"}, "permissions": []}	2000-01-01 13:16:33.74	\N
ebb16174-1c95-48e2-ae8f-943c0eee01c8	{"id": "ebb16174-1c95-48e2-ae8f-943c0eee01c8", "userId": "1dbc9318-9718-4e9d-b32a-6684cf258910", "metadata": {"createdDate": "2022-05-06T13:16:33.756", "updatedDate": "2022-05-06T13:16:33.756+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
f9368298-ee65-483c-a646-cdda416f352b	{"id": "f9368298-ee65-483c-a646-cdda416f352b", "userId": "f643e743-3496-4ecd-94d7-1ca2fdf56c82", "metadata": {"createdDate": "2022-05-06T13:16:33.779", "updatedDate": "2022-05-06T13:16:33.779+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
6803218d-3ee8-475a-9363-6acbc0773784	{"id": "6803218d-3ee8-475a-9363-6acbc0773784", "userId": "04e1cda1-a049-463b-97af-98c59a8fd806", "metadata": {"createdDate": "2022-05-06T13:16:33.791", "updatedDate": "2022-05-06T13:16:33.791+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
92954fbc-597b-403d-9ac2-628ed9f3b5b3	{"id": "92954fbc-597b-403d-9ac2-628ed9f3b5b3", "userId": "eb7696da-a2c3-4166-8aba-757c42556d1e", "metadata": {"createdDate": "2022-05-06T13:16:33.808", "updatedDate": "2022-05-06T13:16:33.808+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
627ac874-f701-4c4a-b532-e60a0d41db02	{"id": "627ac874-f701-4c4a-b532-e60a0d41db02", "userId": "e923bd61-bf27-42a9-8293-ed7738c24bca", "metadata": {"createdDate": "2022-05-06T13:16:33.814", "updatedDate": "2022-05-06T13:16:33.814+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
104ad10c-200a-47d1-8086-765a47842a1e	{"id": "104ad10c-200a-47d1-8086-765a47842a1e", "userId": "f62dc160-eacc-4922-a0cb-e1ed68a44601", "metadata": {"createdDate": "2022-05-06T13:16:33.827", "updatedDate": "2022-05-06T13:16:33.827+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
b8300253-0107-4bbe-a69d-d92196ae7b03	{"id": "b8300253-0107-4bbe-a69d-d92196ae7b03", "userId": "a601f1f2-88a4-465a-850e-8f50c28ce7d9", "metadata": {"createdDate": "2022-05-06T13:16:33.848", "updatedDate": "2022-05-06T13:16:33.848+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
d23646ec-38f4-4e14-83ef-53a12d064a9d	{"id": "d23646ec-38f4-4e14-83ef-53a12d064a9d", "userId": "9a04ae0d-e39f-44c3-b520-43144f6d93e4", "metadata": {"createdDate": "2022-05-06T13:16:33.859", "updatedDate": "2022-05-06T13:16:33.859+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
b6d02534-7fbe-44bc-8a8f-69a7f7d6ee68	{"id": "b6d02534-7fbe-44bc-8a8f-69a7f7d6ee68", "userId": "5cf0c0d9-17cc-42f1-87c1-10ec6476fc3a", "metadata": {"createdDate": "2022-05-06T13:16:33.869", "updatedDate": "2022-05-06T13:16:33.869+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
0dcbd035-efa0-473e-95d4-4aa2d0962c75	{"id": "0dcbd035-efa0-473e-95d4-4aa2d0962c75", "userId": "5c4910af-508f-49f5-b2c2-f856ffd7f2aa", "metadata": {"createdDate": "2022-05-06T13:16:33.878", "updatedDate": "2022-05-06T13:16:33.878+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
866e14b1-936b-4520-8535-dd000bdb876f	{"id": "866e14b1-936b-4520-8535-dd000bdb876f", "userId": "6c76eeec-183d-4635-9019-11ce8623d50c", "metadata": {"createdDate": "2022-05-06T13:16:33.888", "updatedDate": "2022-05-06T13:16:33.888+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
0fee77f9-feec-40d1-87c7-359abacee349	{"id": "0fee77f9-feec-40d1-87c7-359abacee349", "userId": "df84acd4-4425-47e9-9a25-db8eb2973950", "metadata": {"createdDate": "2022-05-06T13:16:33.897", "updatedDate": "2022-05-06T13:16:33.897+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
2b517f24-412d-4ac8-8d49-735b9643be83	{"id": "2b517f24-412d-4ac8-8d49-735b9643be83", "userId": "259d55dc-015d-420a-b13d-8706018305b1", "metadata": {"createdDate": "2022-05-06T13:16:33.905", "updatedDate": "2022-05-06T13:16:33.905+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
f59b1a78-b11f-4a69-8563-1c048ab069f7	{"id": "f59b1a78-b11f-4a69-8563-1c048ab069f7", "userId": "2fabd929-3ed9-40ae-aaf2-6c39c4bebf13", "metadata": {"createdDate": "2022-05-06T13:16:33.915", "updatedDate": "2022-05-06T13:16:33.915+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
425c9c69-07b2-4ea0-a48d-dc14388af289	{"id": "425c9c69-07b2-4ea0-a48d-dc14388af289", "userId": "4c564a60-8ac6-41f9-a8b6-088901a5f8ca", "metadata": {"createdDate": "2022-05-06T13:16:33.92", "updatedDate": "2022-05-06T13:16:33.920+00:00"}, "permissions": []}	2000-01-01 13:16:33.92	\N
13563468-dff3-4f24-a5c3-aa4072871349	{"id": "13563468-dff3-4f24-a5c3-aa4072871349", "userId": "734f2e97-2c41-4e70-9b98-44cead2607e4", "metadata": {"createdDate": "2022-05-06T13:16:33.924", "updatedDate": "2022-05-06T13:16:33.924+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
0758a809-18aa-4446-9b16-d31404142db0	{"id": "0758a809-18aa-4446-9b16-d31404142db0", "userId": "00bc2807-4d5b-4a27-a2b5-b7b1ba431cc4", "metadata": {"createdDate": "2022-05-06T13:16:33.684", "updatedDate": "2022-05-06T13:16:33.684+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
629e8f50-50a6-485b-bf66-7763175c8f0c	{"id": "629e8f50-50a6-485b-bf66-7763175c8f0c", "userId": "07066a1f-1fb7-4793-bbca-7cd8d1ea90ab", "metadata": {"createdDate": "2022-05-06T13:16:33.695", "updatedDate": "2022-05-06T13:16:33.695+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
3395613f-80b5-4790-97f8-96c93f633f90	{"id": "3395613f-80b5-4790-97f8-96c93f633f90", "userId": "f57c13f1-6114-41b2-aa6f-33045068d6be", "metadata": {"createdDate": "2022-05-06T13:16:33.707", "updatedDate": "2022-05-06T13:16:33.707+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
abb3e798-8111-4986-9a53-f992913d49b3	{"id": "abb3e798-8111-4986-9a53-f992913d49b3", "userId": "aace299f-7a74-4118-9cf3-599110dce278", "metadata": {"createdDate": "2022-05-06T13:16:33.715", "updatedDate": "2022-05-06T13:16:33.715+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
e5d7d60e-5b8d-437c-9a4f-bdb34d39bcc6	{"id": "e5d7d60e-5b8d-437c-9a4f-bdb34d39bcc6", "userId": "f45d047f-248d-424a-9571-8b1249279c02", "metadata": {"createdDate": "2022-05-06T13:16:33.72", "updatedDate": "2022-05-06T13:16:33.720+00:00"}, "permissions": []}	2000-01-01 13:16:33.72	\N
bcfc14b6-d84e-4324-9850-95149cbaf99f	{"id": "bcfc14b6-d84e-4324-9850-95149cbaf99f", "userId": "4acbd1f5-dbfe-4928-8325-2955e50faa4b", "metadata": {"createdDate": "2022-05-06T13:16:33.728", "updatedDate": "2022-05-06T13:16:33.728+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
b9eaf900-4c3f-4ba9-a6e7-ef9da68fe477	{"id": "b9eaf900-4c3f-4ba9-a6e7-ef9da68fe477", "userId": "46ed7160-426b-460e-91b3-ab22a7d6fc26", "metadata": {"createdDate": "2022-05-06T13:16:33.746", "updatedDate": "2022-05-06T13:16:33.746+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
569eb8ac-83a7-48e9-9fd8-f7eb2bd90b18	{"id": "569eb8ac-83a7-48e9-9fd8-f7eb2bd90b18", "userId": "011dc219-6b7f-4d93-ae7f-f512ed651493", "metadata": {"createdDate": "2022-05-06T13:16:33.763", "updatedDate": "2022-05-06T13:16:33.763+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
e52e1552-8ff3-4690-8bb6-89c0ed3ed8c8	{"id": "e52e1552-8ff3-4690-8bb6-89c0ed3ed8c8", "userId": "eb81f274-3676-45a9-8e8d-8a151af2506b", "metadata": {"createdDate": "2022-05-06T13:16:33.8", "updatedDate": "2022-05-06T13:16:33.800+00:00"}, "permissions": []}	2000-01-01 13:16:33.8	\N
5f527c8e-43de-479d-b3b6-10e6fbfe5e54	{"id": "5f527c8e-43de-479d-b3b6-10e6fbfe5e54", "userId": "0db6912a-40c0-41db-8d15-be05ff851f96", "metadata": {"createdDate": "2022-05-06T13:16:33.816", "updatedDate": "2022-05-06T13:16:33.816+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
8812b12a-19a7-4244-940e-32de92c4497f	{"id": "8812b12a-19a7-4244-940e-32de92c4497f", "userId": "01b9d72b-9aab-4efd-97a4-d03c1667bf0d", "metadata": {"createdDate": "2022-05-06T13:16:33.835", "updatedDate": "2022-05-06T13:16:33.835+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
9a7b647c-f09c-4dc1-a700-9ca9a519f814	{"id": "9a7b647c-f09c-4dc1-a700-9ca9a519f814", "userId": "0b6d1482-de21-4643-ae5b-90b4c7164c4a", "metadata": {"createdDate": "2022-05-06T13:16:33.85", "updatedDate": "2022-05-06T13:16:33.850+00:00"}, "permissions": []}	2000-01-01 13:16:33.85	\N
61465f5e-b00b-46f9-a426-f6fc502820c9	{"id": "61465f5e-b00b-46f9-a426-f6fc502820c9", "userId": "75da2654-00a8-4ca5-9c73-2bf9e1e5c883", "metadata": {"createdDate": "2022-05-06T13:16:33.864", "updatedDate": "2022-05-06T13:16:33.864+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
759671d6-1906-4d58-9223-b8a4254567a2	{"id": "759671d6-1906-4d58-9223-b8a4254567a2", "userId": "0aa0c321-9974-4a67-92dc-bca029d093e2", "metadata": {"createdDate": "2022-05-06T13:16:33.886", "updatedDate": "2022-05-06T13:16:33.886+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
19d0011a-56e8-433b-b31c-da8a645842b6	{"id": "19d0011a-56e8-433b-b31c-da8a645842b6", "userId": "1be7f410-6ec3-4e88-ac25-0f8d8c63274d", "metadata": {"createdDate": "2022-05-06T13:16:33.894", "updatedDate": "2022-05-06T13:16:33.894+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
fe6755f4-9221-4a04-b23c-b1a10f9e6ce0	{"id": "fe6755f4-9221-4a04-b23c-b1a10f9e6ce0", "userId": "423d5beb-3196-449e-aacb-9595d6321950", "metadata": {"createdDate": "2022-05-06T13:16:33.908", "updatedDate": "2022-05-06T13:16:33.908+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
1e92492d-f0b1-4b26-a00e-b356b26d7301	{"id": "1e92492d-f0b1-4b26-a00e-b356b26d7301", "userId": "eaed771f-1472-4f5a-a31f-bbb5922ba5fe", "metadata": {"createdDate": "2022-05-06T13:16:33.916", "updatedDate": "2022-05-06T13:16:33.916+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
fe1da53f-a8b0-434c-905f-06706add9168	{"id": "fe1da53f-a8b0-434c-905f-06706add9168", "userId": "f4e0bd3e-1592-4a70-9f4a-41ccb6ca6b43", "metadata": {"createdDate": "2022-05-06T13:16:33.927", "updatedDate": "2022-05-06T13:16:33.927+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
4629523a-adeb-409f-b8df-0d2570786049	{"id": "4629523a-adeb-409f-b8df-0d2570786049", "userId": "c51cf0e7-ea33-4638-a54c-afffc75a680b", "metadata": {"createdDate": "2022-05-06T13:16:33.932", "updatedDate": "2022-05-06T13:16:33.932+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
618f08ad-9b64-482c-a16c-9be339008b28	{"id": "618f08ad-9b64-482c-a16c-9be339008b28", "userId": "c78aa9ec-b7d3-4d53-9e43-20296f39b496", "metadata": {"createdDate": "2022-05-06T13:16:33.842", "updatedDate": "2022-05-06T13:16:33.842+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
8cc1fc15-5cc0-44bb-8aca-651f69eaabb1	{"id": "8cc1fc15-5cc0-44bb-8aca-651f69eaabb1", "userId": "97f100da-9218-4351-bc14-ef0558f01625", "metadata": {"createdDate": "2022-05-06T13:16:33.854", "updatedDate": "2022-05-06T13:16:33.854+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
dbf3b6e0-3174-4a16-be34-ec06a2b826f4	{"id": "dbf3b6e0-3174-4a16-be34-ec06a2b826f4", "userId": "3793853e-6297-424d-abea-24525079f658", "metadata": {"createdDate": "2022-05-06T13:16:33.865", "updatedDate": "2022-05-06T13:16:33.865+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
674979da-d915-4f29-8967-87bbe1619108	{"id": "674979da-d915-4f29-8967-87bbe1619108", "userId": "2a823816-c059-4703-becf-0cc68a734189", "metadata": {"createdDate": "2022-05-06T13:16:33.886", "updatedDate": "2022-05-06T13:16:33.886+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
a844bd92-67ef-4674-bb5a-18075c17fcbd	{"id": "a844bd92-67ef-4674-bb5a-18075c17fcbd", "userId": "0cce8c30-0a0d-4ebb-a107-cf47ad35eafb", "metadata": {"createdDate": "2022-05-06T13:16:33.896", "updatedDate": "2022-05-06T13:16:33.896+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
a167a06e-43b1-40fb-9873-42fa4b36d531	{"id": "a167a06e-43b1-40fb-9873-42fa4b36d531", "userId": "d20d2f4e-6356-4dc3-b7b1-8b9fb5564e02", "metadata": {"createdDate": "2022-05-06T13:16:33.9", "updatedDate": "2022-05-06T13:16:33.900+00:00"}, "permissions": []}	2000-01-01 13:16:33.9	\N
907b7b38-8ca1-4905-a33e-372cd3f52999	{"id": "907b7b38-8ca1-4905-a33e-372cd3f52999", "userId": "5e84b6a4-fde4-4099-ab54-c82c9041f685", "metadata": {"createdDate": "2022-05-06T13:16:33.912", "updatedDate": "2022-05-06T13:16:33.912+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
46f78d7e-1fb1-43e4-8a97-c1fa41ed9c11	{"id": "46f78d7e-1fb1-43e4-8a97-c1fa41ed9c11", "userId": "b3e39715-0659-4776-9d40-abe655408d84", "metadata": {"createdDate": "2022-05-06T13:16:33.923", "updatedDate": "2022-05-06T13:16:33.923+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
d58b183a-dfb9-4273-8dd2-aac7d7aade3a	{"id": "d58b183a-dfb9-4273-8dd2-aac7d7aade3a", "userId": "384272bb-efab-4e94-b3b8-f67f20796f20", "metadata": {"createdDate": "2022-05-06T13:16:33.908", "updatedDate": "2022-05-06T13:16:33.908+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
3cfbe041-44de-4429-87d6-3430329d8141	{"id": "3cfbe041-44de-4429-87d6-3430329d8141", "userId": "1ee0a888-ade1-4bc7-a9b6-15a2d46a6b18", "metadata": {"createdDate": "2022-05-06T13:16:33.925", "updatedDate": "2022-05-06T13:16:33.925+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
84b35d9c-6865-417d-ad92-fc1664729114	{"id": "84b35d9c-6865-417d-ad92-fc1664729114", "userId": "420d485a-033f-4999-ab2c-12c00cd5ec07", "metadata": {"createdDate": "2022-05-06T13:16:33.93", "updatedDate": "2022-05-06T13:16:33.930+00:00"}, "permissions": []}	2000-01-01 13:16:33.93	\N
\.


--
-- Data for Name: rmb_internal; Type: TABLE DATA; Schema: kiwi_mod_permissions; Owner: postgres
--

COPY kiwi_mod_permissions.rmb_internal (id, jsonb) FROM stdin;
1	{"rmbVersion": "33.2.3", "schemaJson": "{\\n  \\"scripts\\" : [],\\n  \\"tables\\" : [\\n    {\\n      \\"tableName\\" : \\"permissions\\",\\n      \\"fromModuleVersion\\" : \\"5.0\\",\\n      \\"withMetadata\\" : true,\\n      \\"uniqueIndex\\" : [\\n        {\\n          \\"fieldName\\" : \\"permissionName\\",\\n          \\"tOps\\" : \\"ADD\\",\\n          \\"caseSensitive\\": true,\\n          \\"removeAccents\\": false\\n        }\\n      ],\\n      \\"ginIndex\\": [\\n        {\\n          \\"fieldName\\": \\"permissionName\\",\\n          \\"tOps\\": \\"ADD\\",\\n          \\"caseSensitive\\": false,\\n          \\"removeAccents\\": true\\n        }\\n      ]\\n    },\\n    {\\n      \\"tableName\\" : \\"permissions_users\\",\\n      \\"fromModuleVersion\\" : \\"5.0\\",\\n      \\"withMetadata\\" : true,\\n      \\"index\\" : [\\n        {\\n          \\"fieldName\\" : \\"id\\",\\n          \\"tOps\\" : \\"ADD\\",\\n          \\"caseSensitive\\": true,\\n          \\"removeAccents\\": false\\n        },\\n        {\\n          \\"fieldName\\" : \\"userId\\",\\n          \\"tOps\\" : \\"ADD\\",\\n          \\"caseSensitive\\": true,\\n          \\"removeAccents\\": false\\n        }\\n      ],\\n      \\"ginIndex\\": [\\n        {\\n          \\"fieldName\\": \\"userId\\",\\n          \\"tOps\\": \\"ADD\\",\\n          \\"caseSensitive\\": false,\\n          \\"removeAccents\\": true\\n        }\\n      ]\\n    }\\n  ],\\n  \\"views\\" : []\\n\\n}\\n", "moduleVersion": "mod-inventory-storage-999.0.0"}
\.


--
-- Data for Name: rmb_internal_analyze; Type: TABLE DATA; Schema: kiwi_mod_permissions; Owner: postgres
--

COPY kiwi_mod_permissions.rmb_internal_analyze (tablename) FROM stdin;
\.


--
-- Data for Name: rmb_internal_index; Type: TABLE DATA; Schema: kiwi_mod_permissions; Owner: postgres
--

COPY kiwi_mod_permissions.rmb_internal_index (name, def, remove) FROM stdin;
permissions_permissionName_idx_unique	CREATE UNIQUE INDEX IF NOT EXISTS permissions_permissionName_idx_unique ON kiwi_mod_permissions.permissions ((jsonb->>'permissionName'))	f
permissions_permissionName_idx_gin	CREATE INDEX IF NOT EXISTS permissions_permissionName_idx_gin ON kiwi_mod_permissions.permissions USING GIN ((lower(f_unaccent(jsonb->>'permissionName'))) public.gin_trgm_ops)	f
permissions_users_id_idx	CREATE INDEX IF NOT EXISTS permissions_users_id_idx ON kiwi_mod_permissions.permissions_users (left((jsonb->>'id'),600))	f
permissions_users_userId_idx	CREATE INDEX IF NOT EXISTS permissions_users_userId_idx ON kiwi_mod_permissions.permissions_users (left((jsonb->>'userId'),600))	f
permissions_users_userId_idx_gin	CREATE INDEX IF NOT EXISTS permissions_users_userId_idx_gin ON kiwi_mod_permissions.permissions_users USING GIN ((lower(f_unaccent(jsonb->>'userId'))) public.gin_trgm_ops)	f
\.


--
-- Data for Name: rmb_job; Type: TABLE DATA; Schema: kiwi_mod_permissions; Owner: postgres
--

COPY kiwi_mod_permissions.rmb_job (id, jsonb) FROM stdin;
7a540785-c6be-4ea9-b0f7-2a7619de5a57	{"id": "7a540785-c6be-4ea9-b0f7-2a7619de5a57", "tenant": "kiwi", "complete": true, "messages": [], "tenantAttributes": {"module_to": "mod-inventory-storage-999.0.0", "parameters": [{"key": "loadReference", "value": "true"}, {"key": "loadSample", "value": "true"}]}}
\.


--
-- Name: rmb_internal_id_seq; Type: SEQUENCE SET; Schema: kiwi_mod_permissions; Owner: postgres
--

SELECT pg_catalog.setval('kiwi_mod_permissions.rmb_internal_id_seq', 1, true);


--
-- Name: permissions permissions_pkey; Type: CONSTRAINT; Schema: kiwi_mod_permissions; Owner: postgres
--

ALTER TABLE ONLY kiwi_mod_permissions.permissions
    ADD CONSTRAINT permissions_pkey PRIMARY KEY (id);


--
-- Name: permissions_users permissions_users_pkey; Type: CONSTRAINT; Schema: kiwi_mod_permissions; Owner: postgres
--

ALTER TABLE ONLY kiwi_mod_permissions.permissions_users
    ADD CONSTRAINT permissions_users_pkey PRIMARY KEY (id);


--
-- Name: rmb_internal_index rmb_internal_index_pkey; Type: CONSTRAINT; Schema: kiwi_mod_permissions; Owner: postgres
--

ALTER TABLE ONLY kiwi_mod_permissions.rmb_internal_index
    ADD CONSTRAINT rmb_internal_index_pkey PRIMARY KEY (name);


--
-- Name: rmb_internal rmb_internal_pkey; Type: CONSTRAINT; Schema: kiwi_mod_permissions; Owner: postgres
--

ALTER TABLE ONLY kiwi_mod_permissions.rmb_internal
    ADD CONSTRAINT rmb_internal_pkey PRIMARY KEY (id);


--
-- Name: rmb_job rmb_job_pkey; Type: CONSTRAINT; Schema: kiwi_mod_permissions; Owner: postgres
--

ALTER TABLE ONLY kiwi_mod_permissions.rmb_job
    ADD CONSTRAINT rmb_job_pkey PRIMARY KEY (id);


--
-- Name: permissions_permissionname_idx_gin; Type: INDEX; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE INDEX permissions_permissionname_idx_gin ON kiwi_mod_permissions.permissions USING gin (lower(kiwi_mod_permissions.f_unaccent((jsonb ->> 'permissionName'::text))) public.gin_trgm_ops);


--
-- Name: permissions_permissionname_idx_unique; Type: INDEX; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE UNIQUE INDEX permissions_permissionname_idx_unique ON kiwi_mod_permissions.permissions USING btree (((jsonb ->> 'permissionName'::text)));


--
-- Name: permissions_users_id_idx; Type: INDEX; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE INDEX permissions_users_id_idx ON kiwi_mod_permissions.permissions_users USING btree ("left"((jsonb ->> 'id'::text), 600));


--
-- Name: permissions_users_userid_idx; Type: INDEX; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE INDEX permissions_users_userid_idx ON kiwi_mod_permissions.permissions_users USING btree ("left"((jsonb ->> 'userId'::text), 600));


--
-- Name: permissions_users_userid_idx_gin; Type: INDEX; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE INDEX permissions_users_userid_idx_gin ON kiwi_mod_permissions.permissions_users USING gin (lower(kiwi_mod_permissions.f_unaccent((jsonb ->> 'userId'::text))) public.gin_trgm_ops);


--
-- Name: permissions set_id_in_jsonb; Type: TRIGGER; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE TRIGGER set_id_in_jsonb BEFORE INSERT OR UPDATE ON kiwi_mod_permissions.permissions FOR EACH ROW EXECUTE FUNCTION kiwi_mod_permissions.set_id_in_jsonb();


--
-- Name: permissions_users set_id_in_jsonb; Type: TRIGGER; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE TRIGGER set_id_in_jsonb BEFORE INSERT OR UPDATE ON kiwi_mod_permissions.permissions_users FOR EACH ROW EXECUTE FUNCTION kiwi_mod_permissions.set_id_in_jsonb();


--
-- Name: permissions set_permissions_md_json_trigger; Type: TRIGGER; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE TRIGGER set_permissions_md_json_trigger BEFORE UPDATE ON kiwi_mod_permissions.permissions FOR EACH ROW EXECUTE FUNCTION kiwi_mod_permissions.set_permissions_md_json();


--
-- Name: permissions set_permissions_md_trigger; Type: TRIGGER; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE TRIGGER set_permissions_md_trigger BEFORE INSERT ON kiwi_mod_permissions.permissions FOR EACH ROW EXECUTE FUNCTION kiwi_mod_permissions.permissions_set_md();


--
-- Name: permissions_users set_permissions_users_md_json_trigger; Type: TRIGGER; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE TRIGGER set_permissions_users_md_json_trigger BEFORE UPDATE ON kiwi_mod_permissions.permissions_users FOR EACH ROW EXECUTE FUNCTION kiwi_mod_permissions.set_permissions_users_md_json();


--
-- Name: permissions_users set_permissions_users_md_trigger; Type: TRIGGER; Schema: kiwi_mod_permissions; Owner: postgres
--

CREATE TRIGGER set_permissions_users_md_trigger BEFORE INSERT ON kiwi_mod_permissions.permissions_users FOR EACH ROW EXECUTE FUNCTION kiwi_mod_permissions.permissions_users_set_md();


--
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
GRANT USAGE ON SCHEMA public TO PUBLIC;


--
-- Name: TABLE permissions; Type: ACL; Schema: kiwi_mod_permissions; Owner: postgres
--

GRANT ALL ON TABLE kiwi_mod_permissions.permissions TO kiwi_mod_permissions;


--
-- Name: TABLE permissions_users; Type: ACL; Schema: kiwi_mod_permissions; Owner: postgres
--

GRANT ALL ON TABLE kiwi_mod_permissions.permissions_users TO kiwi_mod_permissions;


--
-- Name: TABLE rmb_internal; Type: ACL; Schema: kiwi_mod_permissions; Owner: postgres
--

GRANT ALL ON TABLE kiwi_mod_permissions.rmb_internal TO kiwi_mod_permissions;


--
-- Name: TABLE rmb_internal_analyze; Type: ACL; Schema: kiwi_mod_permissions; Owner: postgres
--

GRANT ALL ON TABLE kiwi_mod_permissions.rmb_internal_analyze TO kiwi_mod_permissions;


--
-- Name: TABLE rmb_internal_index; Type: ACL; Schema: kiwi_mod_permissions; Owner: postgres
--

GRANT ALL ON TABLE kiwi_mod_permissions.rmb_internal_index TO kiwi_mod_permissions;


--
-- Name: TABLE rmb_job; Type: ACL; Schema: kiwi_mod_permissions; Owner: postgres
--

GRANT ALL ON TABLE kiwi_mod_permissions.rmb_job TO kiwi_mod_permissions;


--
-- PostgreSQL database dump complete
--

--
-- PostgreSQL database cluster dump complete
--


-- record without userId, migration must delete this record: https://issues.folio.org/browse/MODPERMS-177

COPY kiwi_mod_permissions.permissions_users (id, jsonb, creation_date, created_by) FROM stdin;
eb0f05ab-88f6-4d95-9a54-b1812fe36e90	{"id": "eb0f05ab-88f6-4d95-9a54-b1812fe36e90", "metadata": {"createdDate": "2000-01-01T00:00:00.000", "updatedDate": "2000-01-01T00:00:00.000+00:00"}, "permissions": []}	2000-01-01 00:00:00.000	\N
\.

