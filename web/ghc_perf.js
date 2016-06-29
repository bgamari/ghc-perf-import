Plotly.setPlotConfig({ logging: 2 });

const limit = 100000;

function populate_tests() {
    fetch(`http://home.smart-cactus.org:8080/tests`)
        .then(resp => {
            return resp.json().then(resp => {
                for (let test of resp) {
                    const input = $(`<input type="checkbox" name="${test.test_name}">`)
                        .on('change', (ev) => {
                            if (ev.target.checked) {
                                add_test(ev.target.name);
                            } else {
                                delete test_points[ev.target.name];
                                fill_deltas_table(deltas);
                            }
                        });
                    const label = $("<label/>")
                          .attr('ref', test.test_name)
                          .html(test.test_name);
                    $('#tests').append($(`<li/>`).append(input).append(label));
                }
            });
        });
}

const graph_div = document.getElementById('plot');
// map from test name to its points
let test_points = {};

function add_test(test) {
    console.log(`Adding test ${test}`);

    $("body").addClass('working');
    fetch(`http://home.smart-cactus.org:8080/results_view?test_name=eq.${test}&branch_name=eq.master&test_env=eq.nomeata&order=commit_date&limit=${limit}`)
        .then(resp => {
            return resp.json().then(resp => {
                console.log(`Have ${resp.length} points for ${test}`);
                test_points[test] = resp;

                deltas = flatten(Object.entries(test_points).map(x => find_deltas(x[1])).sort((x, y) => x.commit_date < y.commit_date));
                fill_deltas_table(deltas);
                update_plots();
                $("body").removeClass('working');
            });
        });
}

function update_plots() {
    const n_traces = Object.keys(test_points).length;
    let data = [];
    let layout = {
        yaxis: { title: "Benchmarked Value" }, // default when there are otherwise no traces
        xaxis: {
            showgrid: false,                  // remove the x-axis grid lines
            type: 'date',
            tickformat: "%B, %Y",             // customize the date format to "month, day"
            autorange: true,
            rangemode: 'normal',
            domain: [0.12*n_traces, 0.95]
        },
        margin: {
            l: 70, b: 80, r: 10, t: 20
        },
        annotations: []
    };
    let trace_n = 0;
    for (let x of Object.entries(test_points)) {
        const axis_name = trace_n > 0 ? `yaxis${trace_n+1}` : 'yaxis';
        const short_axis_name = trace_n > 0 ? `y${trace_n+1}` : 'y';
        const test_name = x[0];
        const points = x[1];
        layout[axis_name] = { title: test_name, position: 0.12*trace_n };
        if (trace_n > 0) {
            layout[axis_name]['overlaying'] = 'y';
        }
        x['yaxis'] = short_axis_name;

        data.push({
            x: points.map(r => Date.parse(r.commit_date)),
            y: points.map(r => r.result_value),
            yaxis: short_axis_name,
            text: points.map(r => r.commit_sha),
            type: "scatter",
            line: { width: 1 },
            name: test_name
        });
        trace_n = trace_n + 1;
    }
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
    zip([points.slice(0,-1), points.slice(1)]).forEach(pair => {
        const fst = pair[0];
        const snd = pair[1];
        const delta = (snd.result_value - fst.result_value) / snd.result_value;
        if (delta > 0.05) {
            snd['delta'] = delta;
            deltas.push(snd);
        }
    });
    return deltas;
}

let selected_commit;

function deltas_annots(deltas) {
    let anns = {};
    for (let x of deltas) {
        anns[x.commit_sha] = {
            x: Date.parse(x.commit_date),
            y: x.result_value,
            xref: 'x',
            yref: x.yaxis,
            text: x.commit_sha.slice(0,8),
            font: selected_commit == x.commit_sha ? {color: 'red'} : {}
        };
    }
    return Object.values(anns);
}

function fill_deltas_table(deltas) {
    const tbl = $("#deltas tbody");
    tbl.children().remove();
    for (let x of deltas) {
        tbl.append(
            $('<tr>')
                .append($("<td/>").html(new Date(x.commit_date).toDateString()))
                .append($("<td/>").html($(`<span class="commit">${x.commit_sha}</span>`)))
                .append($("<td/>").html(x.test_name))
                .append($("<td/>").html((100*x.delta).toPrecision(2) + '%'))
                .append($("<td/>").html(x.commit_title))
                .on('click', ev => { selected_commit = x.commit_sha;
                                     add_deltas_annots(deltas);
                                   })
                .attr('data-commit', x.commit_sha)
                .attr('data-test_name', x.test_name)
        );
    }
}

populate_tests();
Plotly.plot(graph_div, [], {});
update_plots();

//add_test('compile-allocs/AbsConc3');
//add_test('compile-allocs/AbstractEval2');
