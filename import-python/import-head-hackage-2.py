#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re
import gitlab
import traceback
import shutil
import ast
from tempfile import TemporaryDirectory
from pathlib import Path
import json
import subprocess
from typing import NamedTuple, Iterator, List
import asyncpg
import asyncio

schema = """
CREATE TABLE head_hackage_results (
  head_hackage_result_id serial PRIMARY KEY,
  commit_id integer REFERENCES commits(commit_id),
  package text,
  version text,
  module text,
  compiler_pass text,
  allocs real,
  time real
);

create view head_hackage_results_view as
  select
    commit_sha, commit_date,
    package, version, module, compiler_pass, allocs,
    commit_title
  from head_hackage_results as r
  inner join commits on commits.commit_id = r.commit_id;
"""

conn_str = 'postgresql:///ghc_perf'
import_tool = '/home/ben/ghc-perf-import/import/result/bin/perf-import-head-hackage'

def parse_info(fname: Path) -> dict:
    return {k: v for (k,v) in ast.literal_eval(fname.read_text().strip())}

class Record(NamedTuple):
    package: str
    version: str
    module: str
    compiler_pass: str
    allocs: float
    time: float

def process_log(log: Path, package: str, version: str) -> Iterator[Record]:
    txt = log.read_text()
    regexp = r'([\w\d/ ]+) \[([\w\d\.]+)\]: alloc=(\d+) time=(\d+\.\d+)'
    for m in re.findall(regexp, txt):
        compiler_pass, module, allocs_, time_ = m
        allocs = float(allocs_)
        time = float(time_)
        yield Record(package, version, module, compiler_pass, allocs, time)

async def main():
    conn = await asyncpg.connect(conn_str)
    gl = gitlab.Gitlab.from_config()
    proj = gl.projects.get('ghc/head.hackage')
    for job in proj.jobs.list(as_list=False):
        if job.name == 'build-master':
            try:
                async with conn.transaction():
                    await process_job(conn, job)
            except Exception as e:
                print(f'job {job.id}: {e}')
                traceback.print_exc()

async def process_job(conn: asyncpg.Connection, job):
    print(job.name)

    art = job.artifacts()
    tmpdir = TemporaryDirectory()
    with NamedTemporaryFile('artifact.zip') as artifact:
        artifact.open('wb').write(art)
        shutil.unpack_archive(artifact, extract_dir=tmpdir.name)

    outdir = TemporaryDirectory()
    shutil.unpack_archive(f'{tmpdir.name}/results.tar.xz', extract_dir=outdir.name)
    out = Path(outdir.name)

    info = parse_info(out / 'compiler-info')
    commit = info['Project Git commit id']

    for log in (out / 'logs').glob('*'):
        parts = log.name.split('-')
        package = '-'.join(parts[:-2])
        version = parts[-2]
        
        results = list(process_log(log, package, version))
        print(log, package, version, len(results))
        await add_results(conn, commit, results)

async def lookup_commit_id(conn: asyncpg.Connection, commit: str) -> int:
    result = await conn.fetchrow('''
        SELECT commit_id FROM commits WHERE commit_sha = $1
        ''', commit)
    return result['commit_id']

async def add_results(conn: asyncpg.Connection, commit: str, records: List[Record]):
    commit_id = await lookup_commit_id(conn, commit)
    print(commit_id)
    
    await conn.executemany('''
        INSERT INTO head_hackage_results
            (commit_id, package, version, module, compiler_pass, allocs, time)
            VALUES ($1, $2, $3, $4, $5, $6, $7)
        ''',
        [(commit_id, r.package, r.version, r.module, r.compiler_pass, r.allocs, r.time)
         for r in records])

if __name__ == '__main__':
    asyncio.run(main())
