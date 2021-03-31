#!/usr/bin/env python
# -*- coding: utf-8 -*-

import gitlab
import traceback
import shutil
import ast
from tempfile import TemporaryDirectory
from pathlib import Path
from datetime import datetime
import dateutil.parser
from typing import Optional, NewType
import json
import subprocess
import asyncio
import asyncpg

conn_str = 'postgresql:///ghc_perf'
TEST_ENV_NAME = 'simple-perf'

INTERESTING_METRICS = {
    "bytes allocated",
    "max_bytes_used",
    "GC_cpu_seconds",
    "mut_cpu_seconds",
    "total_cpu_seconds"
}

def parse_info(fname: Path) -> dict:
    lines = fname.read_text().split('\n')[1:]
    return {k: float(v) for (k,v) in ast.literal_eval("".join(lines).strip())}

async def process_job(conn: asyncpg.Connection, job: Gitlab.ProjectJob):
    async with conn.transaction():
        art = job.artifacts()
        open('artifact.zip', 'wb').write(art)
        tmpdir = TemporaryDirectory()
        shutil.unpack_archive('artifact.zip', extract_dir=tmpdir.name)
        outdir = Path(tmpdir.name) / "out"
        commit = job.commit['id']
        result_date = dateutil.parser.isoparse(job.finished_at)
        print(f'Job {job.id}: commit={commit}')

        test_env_id = await lookup_test_env(conn, TEST_ENV_NAME)
        commit_id = await lookup_commit_id(conn, commit)
        if commit_id is None:
            print(f'Failed to find commit id of {commit}.')
            return

        for testdir in outdir.iterdir():
            results = parse_info(testdir / 'rts.log')
            package = testdir.name
            for metric in INTERESTING_METRICS:
                if metric in results:
                    print(package, metric)
                    await add_result(
                            conn,
                            commit_id=commit_id,
                            test_env_id=test_env_id,
                            test_name=TestName(f'{package}/{metric}'),
                            value=results[metric],
                            result_date=result_date)
                else:
                    print("Didn't find metric {metric}")

async def main():
    conn = await asyncpg.connect(conn_str)
    gl = gitlab.Gitlab.from_config()
    proj = gl.projects.get('ghc/ghc')
    for job in proj.jobs.list(as_list=False):
        if job.name == 'perf':
            if job.status != 'success':
                print(f"Job {job.id} didn't pass, skipping...")
                continue

            try:
                await process_job(conn, job)
            except Exception as e:
                print(f'job {job.id}: {e}')
                traceback.print_exc()

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

if __name__ == '__main__':
    asyncio.run(main())
