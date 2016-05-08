create database ghc_perf;
\c ghc_perf

CREATE TABLE test_envs
    ( test_env_id serial PRIMARY KEY
    , test_env_name text UNIQUE
    );

CREATE TABLE commits
    ( commit_id serial PRIMARY KEY
    , commit_sha text UNIQUE
    , commit_date timestamp with time zone
    );

CREATE TABLE tests
    ( test_id serial PRIMARY KEY
    , test_name text UNIQUE
    );

CREATE TABLE results
    ( result_id serial PRIMARY KEY
    , commit_id integer REFERENCES commits (commit_id)
    , test_env_id integer REFERENCES test_envs (test_env_id)
    , test_id integer REFERENCES tests (test_id)
    , result_value float
    );

INSERT INTO test_envs (test_env_name) VALUES ('nomeata');
