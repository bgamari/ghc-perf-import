#!/usr/bin/env python

import numpy as np
from matplotlib import pyplot as pl
from matplotlib import dates as mdate
import psycopg2
from collections import defaultdict
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('--threshold', '-t', type=float, default=0.05,
                    help='relative change threshold')
parser.add_argument('--env', '-e', type=str, default='nomeata',
                    help='name of the test environment to take samples from')
parser.add_argument('--subtract-offset', '-O', action='store_true',
                    help='subtract baseline offset from individual benchmarks')
parser.add_argument('--limit', '-l', type=int,
                    help='maximum number of benchmarks to plot')
parser.add_argument('--output', '-o', type=str,
                    help='output filename of plot')
parser.add_argument('benchmark', type=str, nargs='+',
                    help='which benchmarks to plot')
args = parser.parse_args()

with_table = False

# relative change significance threshold
delta_thresh = args.threshold

# Plot relative changes
#plot='delta'

# Plot absolute values
plot='abs'
subtract_offset = args.subtract_offset

# Which benchmarks?
#benchmarks = 'compile_time'
benchmarks = '|'.join(args.benchmark)

conn = psycopg2.connect('dbname=ghc_perf host=ben-server.local port=5432 user=ben password=mudpie')
cur = conn.cursor()

cmd = "SELECT test_name FROM tests"
cmd += " WHERE test_name SIMILAR TO '%%(%s)%%'" % benchmarks
if args.limit is not None:
    cmd += " LIMIT %d" % args.limit
cur.execute(cmd)
tests = [rec[0] for rec in cur]
print tests

results = {}
for test in tests:
    # TODO: Why the duplicates?
    cur.execute(
        """SELECT DISTINCT commit_date, commit_sha, commit_title, result_value
           FROM results_view
           WHERE test_env = '{env}'
             AND branch_name = 'master'
             AND test_name='{test_name}'
           ORDER BY commit_date""".format(env=args.env, test_name=test)
    )
    results[test] = np.array([ tuple(rec) for rec in cur ],
                             dtype=[('date', np.object),
                                    ('commit', 'a40'),
                                    ('title', np.object),
                                    ('value', np.float)])
    print test, len(results[test])

# Plot trajectories and collect big deltas
big_deltas = defaultdict(lambda: [])
pl.figure(figsize=(12,12))
rows = []
for test, values in results.items():
    print test
    v = values['value']
    deltas = (v[1:] - v[:-1]) / v[:-1]

    for (delta, d) in zip(deltas[deltas > delta_thresh],
                          values[1:][deltas > delta_thresh]):
        y = d['value']
        if subtract_offset:
            y -= v[0]
        big_deltas[(d['commit'], d['date'])].append(y)
        rows.append((test,
                     '%+6.1f%%' % (100*delta),
                     d['commit'][:8],
                     d['title'] if len(d['title']) < 60 else d['title'][:60]+"..."))
        print '%+6.1f%%    %s  (%s): %s' % (100*delta, d['commit'], d['date'], d['title'])

    print

    if plot == 'delta':
        pl.plot(values['date'][1:], deltas, label=test)
    else:
        if subtract_offset:
            v -= v[0]
        pl.plot(values['date'], v, label=test)

if with_table:
    rows.sort()
    tbl = pl.table(cellText=rows,
                cellLoc='left',
                colLabels=['test', 'delta', 'commit', 'title'],
                colWidths=[0.3,    0.1,     0.1,      0.6],
                loc='bottom')
    tbl.auto_set_font_size(False)
    tbl.set_fontsize(8)
    table_cells = tbl.properties()['child_artists']
    for cell in table_cells: cell.set_height(0.1)

# Plot annotations for big deltas
for (commit, date), ys in big_deltas.items():
    y = max(ys)
    pl.axvline(date, alpha=0.2, color='k')
    pl.annotate(commit[:8],
                xy=(date, y),
                xycoords='data',
                xytext=(-16, 56),
                textcoords='offset points',
                bbox=dict(boxstyle='round', fc='0.9', alpha=1.0),
                arrowprops=dict(arrowstyle='->', connectionstyle='arc3,rad=0.2'))

# Shrink current axis by a bit to make room for legend
#pl.subplots_adjust(bottom=0.7, right=0.65)

# Reduce tick count
loc = mdate.AutoDateLocator(interval_multiples=True, maxticks=8)
pl.gca().get_xaxis().set_major_locator(loc)
pl.gca().get_xaxis().set_major_formatter(mdate.AutoDateFormatter(loc))

pl.xlabel('time')
ylabel = 'benchmarked value'
if subtract_offset: ylabel += ' (offset removed)'
pl.ylabel(ylabel)
pl.legend(loc='center left', bbox_to_anchor=(1, 0.5))
if args.output is not None:
    pl.savefig(args.output)
else:
    pl.show()
