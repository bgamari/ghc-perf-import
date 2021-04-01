from typing import Optional, NewType, Dict
from datetime import datetime
import asyncpg
import ast
from pathlib import Path

CommitSha = NewType('CommitSha', str)
CommitId = NewType('CommitId', int)
TestName = NewType('TestName', str)
TestId = NewType('TestId', int)
TestEnvName = NewType('TestEnvName', str)
TestEnvId = NewType('TestEnvId', int)
ResultSrcId = NewType('ResultSrcId', int)

async def insert_result_src(
        conn: asyncpg.Connection,
        description: str,
        result_date: datetime) -> Optional[ResultSrcId]:
    """
    Inserts a new result source or returns None if the
    it already exists.
    """

    result = await conn.execute('''
        INSERT INTO result_srcs (description)
        VALUES ($1, $2)
        ON CONFLICT DO NOTHING
        RETURNS result_src_id
    ''', description, result_date)

    return ResultSrcId(result['result_src_id']) if result is not None else None

async def lookup_commit_id(
        conn: asyncpg.Connection,
        commit_sha: CommitSha) -> Optional[CommitId]:
    result = await conn.fetchrow('''
        SELECT commit_id
        FROM commits
        WHERE commit_sha = $1
        ''', commit_sha)
    if result is None:
        return None
    else:
        return CommitId(result['commit_id'])

async def lookup_or_insert_commit_id(
        conn: asyncpg.Connection,
        commit_sha: CommitSha,
        commit_title: str,
        commit_date: datetime) -> Optional[CommitId]:
    await conn.execute('''
        INSERT INTO commits (commit_sha, commit_title, commit_date)
        VALUES ($1, $2, $3)
        ON CONFLICT DO NOTHING
        ''', commit_sha, commit_title, commit_date)
    return await lookup_commit_id(commit_sha)

async def lookup_test(
        conn: asyncpg.Connection,
        test_name: TestName) -> TestId:
    await conn.execute('''
        INSERT INTO tests (test_name)
        VALUES ($1)
        ON CONFLICT DO NOTHING
    ''', test_name)

    result = await conn.fetchrow('''
        SELECT test_id
        FROM tests
        WHERE test_name = $1
    ''', test_name)
    return TestId(result['test_id'])

async def lookup_test_env(
        conn: asyncpg.Connection,
        test_env_name: str) -> TestEnvId:
    await conn.execute('''
        INSERT INTO test_envs (test_env_name)
        VALUES ($1)
        ON CONFLICT DO NOTHING
    ''', test_env_name)

    result = await conn.fetchrow('''
        SELECT test_env_id
        FROM test_envs
        WHERE test_env_name = $1
    ''', test_env_name)
    
    print(test_env_name, result)
    return TestEnvId(result['test_env_id'])

async def add_result(
        conn: asyncpg.Connection,
        result_src_id: ResultSrcId,
        test_env_id: TestEnvId,
        commit_id: CommitId,
        test_name: TestName,
        value: float) -> None:
    test_id = await lookup_test(conn, test_name)
    await conn.execute('''
        INSERT INTO results (result_src_id, commit_id, test_env_id, test_id, result_value)
        VALUES ($1, $2, $3, $4)
    ''', result_src_id, commit_id, test_env_id, test_id, value)

def parse_compiler_info(fname: Path) -> Dict[str, str]:
    return {k: v for (k,v) in ast.literal_eval(fname.read_text().strip())}
