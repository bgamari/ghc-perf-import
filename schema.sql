CREATE DATABASE ghc_perf;
\c ghc_perf

-- This improves query performance on results_view significantly
ALTER DATABSE ghc_perf SET cpu_tuple_cost = 0.2;

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
    ( commit_id integer REFERENCES commits (commit_id)
    , test_env_id integer REFERENCES test_envs (test_env_id)
    , test_id integer REFERENCES tests (test_id)
    , result_value float
    , PRIMARY KEY (commit_id, test_env_id, test_id)
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
      -- negative with 0 being head of branch
    , sequence_n integer
    , PRIMARY KEY (branch_id, commit_id)
    , UNIQUE (branch_id, sequence_n)
    );
CREATE INDEX ON branch_commits (branch_id);


CREATE VIEW results_view AS
    SELECT   commits.commit_sha AS commit_sha
           , commits.commit_date AS commit_date
           , commits.commit_title AS commit_title
           , commits.commit_id AS commit_id
           , branches.branch_name AS branch_name
           , branches.branch_id AS branch_id
           , branch_commits.sequence_n AS sequence_n
           , test_envs.test_env_name AS test_env
           , tests.test_name AS test_name
           , results.test_env_id AS test_env_id
           , results.test_id AS test_id
           , results.result_value as result_value
    FROM results
    JOIN tests USING (test_id)
    JOIN test_envs USING (test_env_id)
    JOIN commits USING (commit_id)
    JOIN branch_commits USING (commit_id)
    JOIN branches USING (branch_id);

CREATE USER ghc_perf_web;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO ghc_perf_web;

INSERT INTO test_envs (test_env_name) VALUES ('nomeata');
INSERT INTO test_envs (test_env_name) VALUES ('bgamari');

CREATE VIEW deltas AS
    SELECT   commits.commit_sha
           , commit_date
           , commit_title
           , tests.test_name
           , rx.result_value AS result_value_1
           , ry.result_value AS result_value_2
           , (ry.result_value - rx.result_value) AS delta
           , (ry.result_value - rx.result_value) / NULLIF(rx.result_value, 0) AS rel_delta
    FROM   results AS rx
         , results AS ry
         , test_envs
         , tests
         , branch_commits AS brx
         , branch_commits AS bry
         , commits
    WHERE rx.commit_id = brx.commit_id
      AND ry.commit_id = bry.commit_id
      AND brx.sequence_n = (bry.sequence_n - 1)
      AND rx.test_env_id = ry.test_env_id
      AND rx.test_id = ry.test_id
      AND tests.test_id = rx.test_id
      AND test_envs.test_env_id = rx.test_env_id
      AND commits.commit_id = bry.commit_id;

CREATE VIEW branches_view AS
    SELECT branches.*, commits.*
    FROM branches
    JOIN branch_commits USING (branch_id)
    JOIN commits USING (commit_id)
    WHERE sequence_n = 0;

CREATE VIEW deltas2 AS
    SELECT   cx.commit_sha as commit_1
           , cy.commit_sha as commit_2
           , tests.test_name
           , rx.result_value AS result_value_1
           , ry.result_value AS result_value_2
           , (ry.result_value - rx.result_value) AS delta
           , (ry.result_value - rx.result_value) / NULLIF(rx.result_value, 0) AS rel_delta
    FROM   results AS rx
         , results AS ry
         , test_envs
         , tests
         , commits AS cx
         , commits AS cy
    WHERE rx.commit_id = cx.commit_id
      AND ry.commit_id = cy.commit_id
      AND rx.test_env_id = ry.test_env_id
      AND rx.test_id = ry.test_id
      AND tests.test_id = rx.test_id
      AND test_envs.test_env_id = rx.test_env_id;


-- Geometric mean
CREATE TYPE geom_mean_accum AS (prod real, n integer);
CREATE FUNCTION geom_mean_acc(s geom_mean_accum, v real)
RETURNS geom_mean_accum
AS $$ BEGIN
    RETURN ((s).prod * v, (s).n + 1);
END $$ LANGUAGE plpgsql;

CREATE FUNCTION geom_mean_final(s geom_mean_accum)
RETURNS real
AS $$ BEGIN
    RETURN (s).prod^(1.0 / (s).n);
END $$ LANGUAGE plpgsql;

CREATE AGGREGATE geom_mean(real)
(
    sfunc = geom_mean_acc,
    stype = geom_mean_accum,
    initcond = '(1,0)',
    finalfunc = geom_mean_final
);
