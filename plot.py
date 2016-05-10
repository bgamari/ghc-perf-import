#!/usr/bin/env python

import numpy as np
from matplotlib import pyplot as pl
import psycopg2
from collections import defaultdict

# relative change significance threshold
delta_thresh = 0.05

# Plot relative changes
#plot='delta'

# Plot absolute values
plot='abs'
subtract_offset = True

# Which benchmarks?
#benchmarks = 'compile_time'
benchmarks = 'compile_allocs'

conn = psycopg2.connect('dbname=ghc_perf host=ben-server.local port=5432 user=ben password=mudpie')
cur = conn.cursor()

cur.execute("""SELECT test_name FROM tests WHERE test_name LIKE '%s/%%' LIMIT 5""" % benchmarks)
tests = [rec[0] for rec in cur]
#tests = ['compile-allocs/AbsConc3', 'compile-allocs/Activity']
print tests

results = {}
for test in tests:
    cur.execute(
        """SELECT commit_date, commit_sha, result_value FROM results_view WHERE test_env = 'nomeata' AND branch_name = 'master' AND test_name='%s' ORDER BY commit_date""" % test
    )
    results[test] = np.array([ (rec[0], rec[1], rec[2]) for rec in cur ],
                             dtype=[('date', np.object),
                                    ('commit', 'a40'),
                                    ('value', np.float)])
    print test, len(results[test])

# Plot trajectories and collect big deltas
big_deltas = defaultdict(lambda: [])
for test, values in results.items():
    print test
    v = values['value']
    deltas = (v[1:] - v[:-1]) / v[:-1]

    print '\n'.join('%f   %s  (%s)' % (delta, d['commit'], d['date'])
                    for (delta, d) in zip(deltas[deltas > delta_thresh],
                                          values[1:][deltas > delta_thresh]))
    print

    for (delta, d) in zip(deltas[deltas > delta_thresh],
                          values[1:][deltas > delta_thresh]):
        y = d['value']
        if subtract_offset:
            y -= v[0]
        big_deltas[(d['commit'], d['date'])].append(y)

    if plot == 'delta':
        pl.plot(values['date'][1:], deltas, label=test)
    else:
        if subtract_offset:
            v -= v[0]
        pl.plot(values['date'], v, label=test)

# Plot annotations for big deltas
for (commit, date), ys in big_deltas.items():
    y = max(ys)
    pl.axvline(date, alpha=0.1)
    pl.annotate(commit[:8],
                xy=(date, y),
                xycoords='data',
                xytext=(-16, 56),
                textcoords='offset points',
                bbox=dict(boxstyle='round', fc='0.2', alpha=0.2),
                arrowprops=dict(arrowstyle='->', connectionstyle='arc3,rad=0.2'))

# Shrink current axis by 20% to make room for legend
ax = pl.gca()
box = ax.get_position()
ax.set_position([box.x0, box.y0, box.width * 0.8, box.height])

pl.xlabel('time')
pl.ylabel('benchmarked value\n%s' % benchmarks)
pl.legend(loc='center left', bbox_to_anchor=(1, 0.5))
pl.show()
