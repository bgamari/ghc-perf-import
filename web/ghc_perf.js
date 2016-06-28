Plotly.setPlotConfig({ logging: 2 });

const limit = 100000;

function populate_tests() {
    fetch(`http://home.smart-cactus.org:8080/tests`)
        .then(resp => {
            return resp.json().then(resp => {
                for (var test of resp) {
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
let trace_n = 0;

function add_test(test) {
    function add_trace(name, points) {
        const data = {
            x: points.map(r => Date.parse(r.commit_date)),
            y: points.map(r => r.result_value),
            yaxis: trace_n > 0 ? `y${trace_n+1}` : 'y',
            text: points.map(r => r.commit_sha),
            type: "scatter",
            line: { width: 1 },
            name: name
        };

        console.log(points.slice(0,10));
        let layout = {};
        layout['xaxis.domain'] = [0.12*trace_n, 0.95];
        const axis_name = trace_n > 0 ? `yaxis${trace_n+1}` : 'yaxis';
        layout[axis_name] = { title: test, position: 0.12*trace_n, overlaying: 'y' };

        Plotly.relayout(graph_div, layout);
        Plotly.addTraces(graph_div, data);
        trace_n = trace_n + 1;
    }

    console.log(`Adding test ${test}`);

    $("body").addClass('working');
    fetch(`http://home.smart-cactus.org:8080/results_view?test_name=eq.${test}&order=commit_date&limit=${limit}`)
        .then(resp => {
            return resp.json().then(resp => {
                console.log(`Have ${resp.length} points for ${test}`);
                test_points[test] = resp;
                add_trace(test, resp);

                deltas = flatten(Object.entries(test_points).map(x => find_deltas(x[1])).sort((x, y) => x.commit_date < y.commit_date));
                fill_deltas_table(deltas);
                add_deltas_annots(deltas);
                $("body").removeClass('working');
            });
        });
}

function init_plot() {
    let layout = {
        yaxis: { title: "Benchmarked Value", overlaying: 'y' },
        xaxis: {
            showgrid: false,                  // remove the x-axis grid lines
            type: 'date',
            tickformat: "%B, %Y"              // customize the date format to "month, day"
        },
        margin: {
            l: 70, b: 80, r: 10, t: 20
        },
        annotations: []
    };
    trace_n = 0;
    Plotly.plot(graph_div, [], layout);
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

function add_deltas_annots(deltas) {
    let anns = [];
    for (var x of deltas) {
        anns.push({
            x: Date.parse(x.commit_date),
            y: x.result_value,
            xref: 'x',
            yref: 'y',
            text: x.commit_sha.slice(0,8),
            font: selected_commit == x.commit_sha ? {color: 'red'} : {}
        });
    }
    Plotly.relayout(graph_div, {annotations: anns});
}

function fill_deltas_table(deltas) {
    const tbl = $("#deltas tbody");
    tbl.children().remove();
    for (var x of deltas) {
        tbl.append(
            $('<tr>')
                .append($("<td/>").html(new Date(x.commit_date).toDateString()))
                .append($("<td/>").html($(`<span class="commit">${x.commit_sha}</span>`)))
                .append($("<td/>").html(x.test_name))
                .append($("<td/>").html((100*x.delta).toPrecision(2) + '%'))
                .on('click', ev => { selected_commit = x.commit_sha;
                                     add_deltas_annots(deltas); })
        );
    }
}

populate_tests();
init_plot();
//add_test('compile-allocs/AbsConc3');
//add_test('compile-allocs/AbstractEval2');
