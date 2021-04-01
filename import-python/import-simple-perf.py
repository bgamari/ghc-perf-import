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

async def process_job(conn: asyncpg.Connection, job: gitlab.ProjectJob) -> None:
    async with conn.transaction():
        art = job.artifacts()
        open('artifact.zip', 'wb').write(art)
        tmpdir = TemporaryDirectory()
        shutil.unpack_archive('artifact.zip', extract_dir=tmpdir.name)
        outdir = Path(tmpdir.name) / "out"
        commit = job.commit['id']
        commit_title = job.commit['title']
        commit_date = dateutil.parser.isoparse(job.commit['created_at'])
        result_date = dateutil.parser.isoparse(job.finished_at)
        print(f'Job {job.id}: commit={commit}')

        result_src_id = await insert_result_src(conn, f'simple-perf-job{job.id}', result_date=result_date)
        if result_src_id is None:
            return

        test_env_id = await lookup_test_env(conn, TEST_ENV_NAME)
        commit_id = await lookup_or_insert_commit_id(
                conn,
                commit_sha=commit,
                commit_title=commit_title,
                commit_date=commit_date)
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
                            result_src_id=result_src_id,
                            commit_id=commit_id,
                            test_env_id=test_env_id,
                            test_name=TestName(f'{package}/{metric}'),
                            value=results[metric],
                            result_date=result_date)
                else:
                    print("Didn't find metric {metric}")

async def main() -> None:
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('-j', '--job', type=int)
    args = parser.parse_args()

    conn = await asyncpg.connect(conn_str)
    gl = gitlab.Gitlab.from_config()
    proj = gl.projects.get('ghc/ghc')

    if args.job is not None:
        jobs = [args.job]
    else:
        jobs = proj.jobs.list(as_list=False)

    for job in jobs:
        if job.name == 'perf':
            if job.status != 'success':
                print(f"Job {job.id} didn't pass, skipping...")
                continue

            try:
                await process_job(conn, job)
            except Exception as e:
                print(f'job {job.id}: {e}')
                traceback.print_exc()

def run() -> None:
    asyncio.run(main())

if __name__ == '__main__':
    run()
