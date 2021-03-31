from typing import Optional, NewType
from datetime import datetime
import asyncpg

CommitSha = NewType('CommitSha', str)
CommitId = NewType('CommitId', int)
TestName = NewType('TestName', str)
TestId = NewType('TestId', int)
TestEnvName = NewType('TestEnvName', str)
TestEnvId = NewType('TestEnvId', int)

async def lookup_commit_id(conn: asyncpg.Connection, commit: CommitSha) -> Optional[CommitId]:
    result = await conn.fetchrow('''
        SELECT commit_id
        FROM commits
        WHERE commit_sha = $1
        ''', commit)
    if result is None:
        return None
    else:
        return CommitId(result['commit_id'])

async def lookup_test(conn: asyncpg.Connection, test_name: TestName) -> TestId:
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

async def lookup_test_env(conn: asyncpg.Connection, test_env_name: str) -> TestEnvId:
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
        test_env_id: TestEnvId,
        commit_id: CommitId,
        test_name: TestName,
        value: float,
        result_date: Optional[datetime]):
    test_id = await lookup_test(conn, test_name)
    await conn.execute('''
        INSERT INTO results (commit_id, test_env_id, test_id, result_date, result_value)
        VALUES ($1, $2, $3, $4, $5)
    ''', commit_id, test_env_id, test_id, result_date, value)
