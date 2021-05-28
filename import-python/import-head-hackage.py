#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Import performance metrics from a head.hackage run into the `results`
table.
"""

import gitlab
import shutil
import ast
from tempfile import TemporaryDirectory
from pathlib import Path
import json
import subprocess

from ghc_perf_db import *

conn_str = 'postgresql:///ghc_perf'
import_tool = '/home/ben/ghc-perf-import/import/result/bin/perf-import-head-hackage'

gl = gitlab.Gitlab.from_config()
proj = gl.projects.get('ghc/head.hackage')
for job in proj.jobs.list(all=False):
    if job.name == 'build-master':
        print(job.name)
        art = job.artifacts()
        open('artifact.zip', 'wb').write(art)
        tmpdir = TemporaryDirectory()
        shutil.unpack_archive('artifact.zip', extract_dir=tmpdir.name)
        outdir = TemporaryDirectory()
        shutil.unpack_archive(f'{tmpdir.name}/results.tar.xz', extract_dir=outdir.name)
        out = Path(outdir.name)
        info = parse_compiler_info(out / 'compiler-info')
        commit = info['Project Git commit id']
        logs = list((out / 'logs').glob('*'))
        subprocess.check_call([import_tool, '-c', conn_str, '-C', commit] + [str(path) for path in logs])
