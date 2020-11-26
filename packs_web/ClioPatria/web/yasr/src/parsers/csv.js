'use strict';
var $ = require("jquery");
var root = module.exports = function(queryResponse) {
	return require("./dlv.js")(queryResponse, ",");
};