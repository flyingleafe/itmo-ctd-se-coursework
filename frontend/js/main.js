const {ipcRenderer} = require('electron');
// const Plotly = require('plotly.js');

function preload() {
    let paneContainer = document.getElementById('pane-container');
    let navs = document.querySelectorAll('.nav-group-item');
    
    let fileform = document.getElementById('file-form');
    let filepicker = document.getElementById('filepicker');
    let filesTable = document.getElementById('files-table');
    let filesubmit = document.getElementById('file-submit');
    let plotly = document.getElementById('plotly');

    let usageForm = document.getElementById('usage-form');
    let usagePicker = document.getElementById('usage-file');
    let usageSubmit = document.getElementById('usage-submit');

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

    // Dataset form logic
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

    // Usage form logic
    usagePicker.addEventListener('change', function (event) {
        let files = event.target.files;

        if (files.length == 0) {
            usageSubmit.addAttribute('disabled', 'disabled');
        } else {
            usageSubmit.removeAttribute('disabled');
        }
    });

    usageForm.addEventListener('submit', function (event) {
        event.preventDefault();
        
    }, false);

    // Ipc events from main process
    let chartLength = 0;
    let chartData;
    let chartUpdateInterval = 2000;
    let chartLayout = {
        title: 'Model metrics'
    };
    let metricsNames = ['Mean in-cluster distance', 'Mean distance between clusters'];

    ipcRenderer.on('chart-initialize', function() {
        plotly.innerHTML = 'Waiting for data...';
        queryChartData();
    });

    ipcRenderer.on('chart-reply', function (e, data) {
        let metrics = data._metrics.reverse();
        if (metrics.length > 0) {
            if (chartData == null) {
                plotly.innerHTML = '';
                chartData = [];
                for (let i = 0; i < 2; i++) {
                    chartData.push({
                        x: [],
                        y: [],
                        type: 'scatter',
                        name: metricsNames[i]
                    });
                }
                Plotly.newPlot('plotly', chartData, chartLayout);
            }

            for (let i = chartLength; i < metrics.length; i++) {
                for (let j = 0; j < metrics[i].length; j++) {
                    chartData[j].x.push(i);
                    chartData[j].y.push(metrics[i][j]);
                }
            }
            Plotly.redraw('plotly');
            chartLength = metrics.length;
            console.log(chartData);
        }

        let state = data._appState;
        if (state == "Processing") {
            setTimeout(queryChartData, chartUpdateInterval);
        }
    });

    function queryChartData() {
        ipcRenderer.send('chart-channel');
    }
}

document.addEventListener('DOMContentLoaded', preload);
