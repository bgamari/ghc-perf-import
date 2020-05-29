#!/usr/bin/env python
# -*- coding: utf-8 -*-

from setuptools import setup

setup(
    name='ghc-perf-import-service',
    version='0.1',
    packages=['gitlab_webhook', 'ghc_perf_import_service'],
    install_requires=['falcon', 'python-gitlab'],
    entry_points = {
        'console_scripts': [
            'ghc-perf-import-service = ghc_perf_import_service:main',
        ]
    }
)


