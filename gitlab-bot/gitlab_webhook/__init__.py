#!/usr/bin/env python
# -*- coding: utf-8 -*-

import logging
import falcon
import json
from wsgiref.simple_server import make_server

class WebhookServer:
    def __init__(self, port: int):
        self.port = port
        self.api = falcon.API()
        self.api.add_route('/', EventResource(self.handle_event))

    def run(self):
        httpd = make_server('localhost', self.port, self.api)
        httpd.serve_forever()

    def handle_merge_request_event(self, event):
        raise NotImplementedError()

    def handle_pipeline_event(self, event):
        raise NotImplementedError()

    def handle_job_event(self, event):
        raise NotImplementedError()
    
    def handle_event(self, event):
        logging.debug(event)
        kind = event['object_kind']
        if kind == 'issue':
            self.handle_issue_event(event)
        elif kind == 'merge_request':
            self.handle_merge_request_event(event)
        elif kind == 'pipeline':
            self.handle_pipeline_event(event)
        elif kind == 'build':
            self.handle_job_event(event)
        else:
            raise NotImplementedError()

class EventResource:
    def __init__(self, handle_event):
        self.handle_event = handle_event

    def on_post(self, req, resp):
        decoded = json.load(req.bounded_stream)
        self.handle_event(decoded)
