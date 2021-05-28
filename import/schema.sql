CREATE DATABASE ghc_perf;
\c ghc_perf

CREATE TABLE test_envs
    ( test_env_id serial PRIMARY KEY
    , test_env_name text UNIQUE NOT NULL
    );

CREATE TABLE commits
    ( commit_id serial PRIMARY KEY
    , commit_sha text UNIQUE NOT NULL
    , commit_date timestamp with time zone
    , commit_title TEXT
    );

CREATE INDEX ON commits (commit_date);
CREATE INDEX ON commits (commit_sha);

CREATE TABLE tests
    ( test_id serial PRIMARY KEY
    , test_name text UNIQUE NOT NULL
    );

CREATE INDEX ON tests (test_name);

CREATE TABLE result_srcs
    ( result_src_id serial PRIMARY KEY
    , description text UNIQUE NOT NULL
    , result_date timestamptz
    );

CREATE TABLE results
    ( result_id serial PRIMARY KEY
    , commit_id integer REFERENCES commits (commit_id)
    , test_env_id integer REFERENCES test_envs (test_env_id)
    , test_id integer REFERENCES tests (test_id)
    , result_src_id integer REFERENCES result_srcs(result_src_id)
    , result_value float NOT NULL
    );

CREATE INDEX ON results (test_env_id, commit_id);
CREATE INDEX ON results (test_env_id, test_id);

CREATE TABLE branches
    ( branch_id serial PRIMARY KEY
    , branch_name text UNIQUE NOT NULL
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


-------------------------------------
-- Convenient Views
-------------------------------------

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
           , results.result_id AS result_id
           , results.test_id AS test_id
           , results.result_value as result_value
    FROM results
    INNER JOIN tests USING (test_id)
    INNER JOIN test_envs USING (test_env_id)
    INNER JOIN commits USING (commit_id)
    INNER JOIN branch_commits USING (commit_id)
    INNER JOIN branches USING (branch_id);

CREATE USER ghc_perf_web WITH PASSWORD 'ghc';
GRANT SELECT ON ALL TABLES IN SCHEMA public TO ghc_perf_web;

INSERT INTO test_envs (test_env_name) VALUES ('nomeata');
INSERT INTO test_envs (test_env_name) VALUES ('bgamari');

CREATE VIEW deltas AS
    SELECT   commits.commit_sha
           , commit_date
           , commit_title
           , branches.branch_name AS branch
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
         , branches
         , commits
    WHERE rx.commit_id = brx.commit_id
      AND ry.commit_id = bry.commit_id
      AND brx.sequence_n = (bry.sequence_n - 1)
      AND brx.branch_id = bry.branch_id
      AND brx.branch_id = branches.branch_id
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
           , test_envs.test_env_name
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

CREATE VIEW test_deltas AS
    WITH x AS (
        SELECT   r_ref.test_env_id as test_env_id
               , r_ref.commit_id as ref_commit_id
               , ry.commit_id as commit_id
               , geom_mean((avg(ry.result_value :: real) / avg(r_ref.result_value :: real)) :: real)
                 OVER (PARTITION BY r_ref.test_env_id, r_ref.commit_id, ry.commit_id) AS geom_mean
        FROM   results AS r_ref
        INNER JOIN results ry
           ON ry.test_id = r_ref.test_id 
          AND ry.test_env_id = r_ref.test_env_id 
        GROUP BY r_ref.test_env_id, r_ref.commit_id, ry.commit_id
    )

    SELECT   test_envs.test_env_id
           , test_envs.test_env_name
	   , ref_commit.commit_sha AS ref_commit_sha
	   , x_commit.commit_sha AS commit_sha
	   , x.geom_mean AS geom_mean
    FROM x
    INNER JOIN test_envs ON test_envs.test_env_id = x.test_env_id
    INNER JOIN commits ref_commit ON ref_commit.commit_id = x.ref_commit_id
    INNER JOIN commits x_commit ON x_commit.commit_id = x.commit_id
    ORDER BY x.test_env_id,
             ref_commit.commit_date;

CREATE VIEW commit_metric_counts AS
    SELECT   results.test_env_id
           , test_envs.test_env_name
           , commits.commit_sha
           , commits.commit_date
           , commits.commit_title
           , count(results.test_id) as result_count
    FROM   results
         , test_envs
         , tests
         , commits
    WHERE results.commit_id = commits.commit_id
      AND tests.test_id = results.test_id
      AND test_envs.test_env_id = results.test_env_id
    GROUP BY   results.test_env_id
             , test_envs.test_env_name
             , commits.commit_sha
             , commits.commit_date
             , commits.commit_title;

-------------------------------------
-- Geometric mean aggregation
-------------------------------------

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

