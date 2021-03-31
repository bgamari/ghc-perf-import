#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Import rts.log from a simple-perf job run into the `results` table.
"""

import gitlab
import traceback
import shutil
import ast
from tempfile import TemporaryDirectory, NamedTemporaryFile
from pathlib import Path
from datetime import datetime
import dateutil.parser
from typing import Optional, NewType
import json
import subprocess
import asyncio
import asyncpg

from ghc_perf_db import *

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

if __name__ == '__main__':
    asyncio.run(main())
