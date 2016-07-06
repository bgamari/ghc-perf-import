const entries = require('object.entries');
const values = require('object.values');

Plotly.setPlotConfig({ logging: 2 });

const limit = 100000;

const root_url = "http://home.smart-cactus.org:8080";

function populate_tests() {
    fetch(`${root_url}/tests`)
        .then(resp => {
            return resp.json().then(resp => {
                for (let test of resp) {
                    const input = $("<input type='checkbox'>")
                          .attr('name', test.test_name)
                          .attr('id', test.test_name)
                          .on('change', (ev) => {
                              if (ev.target.checked) {
                                  add_test(ev.target.name);
                              } else {
                                  delete test_points[ev.target.name];
                                  update_plots();
                                  fill_deltas_table(deltas);
                              }
                          });
                    const label = $("<label/>")
                          .attr('for', test.test_name)
                          .html(test.test_name);
                    $('div.tests ul').append($(`<li/>`).append(input).append(label));
                }
            });
        });
}

const graph_div = document.getElementById('plot');
// map from test name to {points, yaxis}
let test_points = {};

function add_test(test) {
    console.log(`Adding test ${test}`);

    $("body").addClass('working');
    fetch(`${root_url}/results_view?test_name=eq.${test}&branch_name=eq.master&test_env=eq.nomeata&order=sequence_n&limit=${limit}`)
        .then(resp => {
            return resp.json().then(resp => {
                console.log(`Have ${resp.length} points for ${test}`);
                test_points[test] = { points: resp, yaxis: null };
                update_all();
            });
        });
}

function update_all() {
    $("body").addClass('working');
    deltas = flatten(
        entries(test_points)
            .map(([_, {points: points}]) => find_deltas(points))
            .sort((x, y) => x.sequence_n < y.sequence_n));
    fill_deltas_table(deltas);
    update_plots();
    $("body").removeClass('working');
}

function update_plots() {
    const axis_width = 0.05;
    const n_traces = Object.keys(test_points).length;
    let data = [];
    let layout = {
        yaxis: { title: "Benchmarked Value" }, // default when there are otherwise no traces
        xaxis: {
            showgrid: false,                  // remove the x-axis grid lines
            autorange: true,
            rangemode: 'normal',
            domain: [axis_width*n_traces, 0.95]
        },
        margin: {
            l: 70, b: 80, r: 10, t: 20
        }
    };
    let trace_n = 0;
    for (let [test_name, {points: points}] of entries(test_points)) {
        const axis_name = trace_n > 0 ? `yaxis${trace_n+1}` : 'yaxis';
        const short_axis_name = trace_n > 0 ? `y${trace_n+1}` : 'y';
        layout[axis_name] = { title: test_name,
                              position: axis_width*trace_n };
        if (trace_n > 0) {
            layout[axis_name]['overlaying'] = 'y';
        }
        test_points[test_name].yaxis = short_axis_name;

        data.push({
            x: points.map(r => r.sequence_n),
            y: points.map(r => r.result_value),
            yaxis: short_axis_name,
            text: points.map(r => r.commit_sha),
            type: "scatter",
            line: { width: 1 },
            name: test_name
        });
        trace_n = trace_n + 1;
    }

    // This needs to be done after the above loop so that yaxis is filled in
    layout.annotations = deltas_annots(deltas);

    Plotly.purge(graph_div);
    Plotly.newPlot(graph_div, data, layout);
    graph_div.on('plotly_hover', function (ev) {
        const commit = ev.points[0].data.text[ev.points[0].pointNumber];
        $(".selected").removeClass('selected');
        $(`[data-commit=${commit}]`).addClass('selected');
    });
}

const zip = rows => rows[0].map((_,c) => rows.map(row => row[c]));
const flatten = xs => [].concat(...xs);
Array.prototype.flatten = flatten;

let deltas = [];

function find_deltas(points) {
    let deltas = [];
    const threshold = $('#delta-threshold')[0].value / 100;
    zip([points.slice(0,-1), points.slice(1)]).forEach(pair => {
        const fst = pair[0];
        const snd = pair[1];
        const delta = (snd.result_value - fst.result_value) / snd.result_value;
        if (Math.abs(delta) > threshold) {
            snd['delta'] = delta;
            deltas.push(snd);
        }
    });
    return deltas;
}

let selected_commit = null;

function deltas_annots(deltas) {
    let anns = {};
    for (let x of deltas) {
        anns[x.commit_sha] = {
            x: x.sequence_n,
            y: x.result_value,
            xref: 'x',
            yref: test_points[x.test_name].yaxis,
            text: x.commit_sha.slice(0,8),
            font: selected_commit == x.commit_sha ? {color: 'red'} : {}
        };
    }
    return values(anns);
}

function fill_deltas_table(deltas) {
    const tbl = $("#deltas tbody");
    tbl.children().remove();
    deltas.sort((x,y) => x.commit_date < y.commit_date).forEach(x => {
        tbl.append(
            $('<tr>')
                .append($("<td/>").html(new Date(x.commit_date).toDateString()))
                .append($("<td/>")
                        .html(x.commit_sha)
                        .addClass('commit'))
                .append($("<td/>").html(x.test_name))
                .append($("<td/>")
                        .html((100*x.delta).toPrecision(2) + '%')
                        .addClass(x.delta > 0 ? 'regression' : 'improvement'))
                .append($("<td/>").html(x.commit_title))
                .on('click', ev => {
                    selected_commit = x.commit_sha;
                    $('.selected').removeClass('selected');
                    $(ev.currentTarget).addClass('selected');
                    Plotly.relayout(graph_div, { 'annotations': deltas_annots(deltas) });
                })
                .attr('data-commit', x.commit_sha)
                .attr('data-test_name', x.test_name)
        );
    });
}

function update_test_filter() {
    const filters = $('#test-filter')[0].value.split();
    for (let x of $('.tests li')) {
        if (filters.every(filter => x.textContent.includes(filter))) {
            $(x).show();
        } else {
            $(x).hide();
        }
    }
}

$(document).ready(() => {
    populate_tests();
    Plotly.plot(graph_div, [], {});
    update_plots();
    update_test_filter();
});
