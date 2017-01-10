const {ipcRenderer} = require('electron');
const Plotly = require('plotly.js');

function preload() {
    let paneContainer = document.getElementById('pane-container');
    let navs = document.querySelectorAll('.nav-group-item');
    let fileform = document.getElementById('file-form');
    let filepicker = document.getElementById('filepicker');
    let filesTable = document.getElementById('files-table');
    let filesubmit = document.getElementById('file-submit');
    let plotly = document.getElementById('plotly');

    // Simple navigation
    for (var i = 0; i < navs.length; i++) {
        navs[i].addEventListener('click', function (e) {
            var curNav = e.target;
            var paneId = curNav.id.replace('menu-', 'pane-');
            var curPane = document.getElementById(paneId);
            Array.from(paneContainer.children).map(pane => pane.classList.add('hidden'));
            Array.from(navs).map(nav => nav.classList.remove('active'));
            curNav.classList.add('active');
            curPane.classList.remove('hidden');
        });
    }

    filepicker.addEventListener('change', function (event) {
        let files = event.target.files;

        if (files.length == 0) {
            filesTable.classList.add('hidden');
            filesubmit.addAttribute('disabled', 'disabled');
        } else {
            let tbody = filesTable.querySelector('tbody');
            while (tbody.hasChildNodes()) {
                tbody.removeChild(tbody.lastChild);
            }

            for (let i = 0; i < files.length; i++) {
                let item = document.createElement('tr');
                item.innerHTML = `<td>${files[i].path}</td>`;
                tbody.appendChild(item);
            }
            filesTable.classList.remove('hidden');
            filesubmit.removeAttribute('disabled');
        }
    }, false);

    fileform.addEventListener('submit', function (event) {
        event.preventDefault();
        let file = filepicker.files[0];
        ipcRenderer.send('fileform-channel', {
            "tmDocFilePath": file.path
        });
    }, false);

    // Ipc events from main process
    let chartLength = 0;
    let chartData;
    let chartUpdateInterval = 2000;

    ipcRenderer.on('chart-initialize', function() {
        plotly.innerHTML = 'Waiting for data...';
        queryChartData();
    });

    ipcRenderer.on('chart-reply', function (data) {
        let metrics = data._metrics.reverse();
        if (metrics.length > 0) {
            if (chartData == null) {
                chartData = [];
                for (let i = 0; i < metrics[0].length; i++) {
                    chartData.push({x: [], y: [], type: 'scatter'});
                }
                Plotly.newPlot('plotly', chartData);
            } else {
                for (let i = chartLength; i < metrics.length; i++) {
                    for (let j = 0; j < metrics[i].length; j++) {
                        chartData[j].x.push(i);
                        chartData[j].y.push(metrics[i][j]);
                    }
                }
                Plotly.redraw('plotly');
            }
        }
        
        let state = data._appState;
        if (state == "Processing") {
            setTimeout(queryChartData, chartUpdateInterval);
        }
    });

    function queryChartData() {
        ipcRendered.send('chart-channel');
    }
}

document.addEventListener('DOMContentLoaded', preload);
