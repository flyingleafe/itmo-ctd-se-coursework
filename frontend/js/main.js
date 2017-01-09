const {ipcRenderer} = require('electron');

function preload() {
    var paneContainer = document.getElementById('pane-container');
    var navs = document.querySelectorAll('.nav-group-item');

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
}

document.addEventListener('DOMContentLoaded', preload);
