'use strict';
const rp = require('request-promise');

const baseUri = 'http://localhost:8090';

exports.initialize = function (config) {
    return rp.post({
        uri: baseUri + '/initialize',
        body: config,
        json: true
    });
};

exports.getState = function () {
    return rp.get({
        uri: baseUri + '/get_state',
        json: true
    });
};
