CREATE DATABASE ghc_perf;
\c ghc_perf

CREATE TABLE test_envs
    ( test_env_id serial PRIMARY KEY
    , test_env_name text UNIQUE
    );

CREATE TABLE commits
    ( commit_id serial PRIMARY KEY
    , commit_sha text UNIQUE
    , commit_date timestamp with time zone
    , commit_title text
    );

CREATE INDEX ON commits (commit_date);
CREATE INDEX ON commits (commit_sha);

CREATE TABLE tests
    ( test_id serial PRIMARY KEY
    , test_name text UNIQUE
    );

CREATE INDEX ON tests (test_name);

CREATE TABLE results
    ( result_id serial PRIMARY KEY
    , commit_id integer REFERENCES commits (commit_id)
    , test_env_id integer REFERENCES test_envs (test_env_id)
    , test_id integer REFERENCES tests (test_id)
    , result_value float
    );

CREATE INDEX ON results (test_env_id, commit_id);
CREATE INDEX ON results (test_env_id, test_id);

CREATE TABLE branches
    ( branch_id serial PRIMARY KEY
    , branch_name text UNIQUE
    );
CREATE INDEX ON branches (branch_name);

CREATE TABLE branch_commits
    ( branch_id integer REFERENCES branches (branch_id)
    , commit_id integer REFERENCES commits (commit_id)
    , PRIMARY KEY (branch_id, commit_id)
    );
CREATE INDEX ON branch_commits (branch_id);


CREATE VIEW results_view AS
    SELECT   commits.commit_sha AS commit_sha
           , commits.commit_date AS commit_date
           , commits.commit_title AS commit_title
           , branches.branch_name AS branch_name
           , test_envs.test_env_name AS test_env
           , tests.test_name AS test_name
           , results.result_value as result_value
    FROM results
    JOIN tests USING (test_id)
    JOIN test_envs USING (test_env_id)
    JOIN commits USING (commit_id)
    JOIN branch_commits USING (commit_id)
    JOIN branches USING (branch_id);

INSERT INTO test_envs (test_env_name) VALUES ('nomeata');
