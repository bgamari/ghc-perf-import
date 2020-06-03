#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Import head.hackage and nofib performance logs.
"""

import gitlab
import logging
from wsgiref.simple_server import make_server
from gitlab_webhook import WebhookServer
from gitlab.v4.objects import ProjectJob

import tempfile
from tarfile import TarFile
from zipfile import ZipFile
from pathlib import Path
import json
import subprocess

def determine_nofib_commit(compiler_info: str) -> str:
    import ast
    a = ast.literal_eval(compiler_info.strip().replace('\n',''))
    assert isinstance(a, list)
    b = dict(a)
    return b['Project Git commit id']

class GHCPerfWebhookServer(WebhookServer):
    def __init__(self, port: int, gl: gitlab.Gitlab, conn_string: str):
        WebhookServer.__init__(self, port)
        self.gl = gl
        self.conn_string = conn_string

    def handle_job_event(self, event) -> None:
        if event['build_status'] != 'success':
            return

        proj_id = event['project_id']
        job_id = event['job_id']
        project = self.gl.projects.get(proj_id)

        if project.name == "nofib/nofib":
            job = project.jobs.get(job_id)
            with tempfile.TemporaryDirectory() as tmp_dir:
                self._process_nofib_job(job, Path(tmp_dir))
        elif project.name == "ghc/head.hackage":
            job = project.jobs.get(job_id)
            with tempfile.TemporaryDirectory() as tmp_dir:
                self._process_head_hackage_job(job, Path(tmp_dir))
        else:
            logging.warn(f'unexpected project name {project.name}')

    def _process_nofib_job(self, job: ProjectJob, tmp_dir: Path):
        logging.info(f'Processing nofib job {job.id}...')
        self._fetch_job_artifacts(job, tmp_dir)
        print(list((tmp_dir / 'results').iterdir()))
        results = list((tmp_dir / 'results').glob('*.results.tsv'))
        logging.info(f'Importing {len(results)} logs')

        # Run import tool
        commit = determine_nofib_commit((tmp_dir / 'results' / 'compiler-info').read_text())
        for f in results:
            cmd = ['perf-import-nofib']
            cmd += ['-e', 'nofib']
            cmd += ['-c', self.conn_string]
            cmd += ['-C', commit]
            cmd += [str(f)]
            subprocess.run(cmd, check=True)

    def _process_head_hackage_job(self, job: ProjectJob, tmp_dir: Path):
        logging.info(f'Processing head.hackage job {job.id}...')
        self._fetch_head_hackage_artifacts(job, tmp_dir)

        summary = json.load((tmp_dir / 'results.json').open())
        comp_info = dict(summary['compilerInfo'])
        commit = comp_info['Project Git commit id']

        logs = list((tmp_dir / 'logs').iterdir())
        logging.info(f'Importing {len(logs)} logs')

        # Run import tool
        cmd = ['perf-import-head-hackage']
        cmd += ['-c', self.conn_string]
        cmd += ['-C', commit]
        cmd += [str(log) for log in logs]
        subprocess.run(cmd, check=True)

    def _fetch_head_hackage_artifacts(self, job: ProjectJob, out_dir: Path):
        self._fetch_job_artifacts(job, out_dir)
        with TarFile.open(out_dir / 'results.tar.xz') as archive:
            archive.extractall(path=out_dir)

    def _fetch_job_artifacts(self, job: ProjectJob, out_dir: Path):
        """
        Fetch artifacts archive of the given job and extract into the given
        directory.
        """
        archive_path = out_dir / "archive.zip"
        with archive_path.open("wb") as f:
            job.artifacts(streamed=True, action=f.write)
        with ZipFile(archive_path) as archive:
            archive.extractall(path=out_dir)

def main() -> None:
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--gitlab-root', type=str, help='GitLab root URL')
    parser.add_argument('--access-token', type=str, help='GitLab access token for bot user')
    parser.add_argument('--port', type=int, required=True, help='Listen port')
    parser.add_argument('--verbose', action='store_true', help='Enable verbose output')
    parser.add_argument('--conn-string', type=str, required=True, help='PostgreSQL connection string')
    parser.add_argument('--test-nofib', type=int, help='nofib job to test')
    parser.add_argument('--test-head-hackage', type=int, help='head.hackage job to test')
    args = parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.DEBUG)

    gl = gitlab.Gitlab(args.gitlab_root, args.access_token)

    server = GHCPerfWebhookServer(args.port, gl, args.conn_string)
    if args.test_nofib is not None:
        project = gl.projects.get('ghc/nofib')
        job = project.jobs.get(args.test_nofib)
        with tempfile.TemporaryDirectory() as tmp_dir:
            server._process_nofib_job(job, Path(tmp_dir))

    elif args.test_head_hackage is not None:
        project = gl.projects.get('ghc/head.hackage')
        job = project.jobs.get(args.test_head_hackage)
        with tempfile.TemporaryDirectory() as tmp_dir:
            server._process_head_hackage_job(job, Path(tmp_dir))

    else:
        server.run()

