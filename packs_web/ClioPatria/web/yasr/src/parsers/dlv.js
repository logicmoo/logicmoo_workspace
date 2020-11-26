'use strict';
var $ = require('jquery');
require("../../lib/jquery.csv-0.71.js");
var root = module.exports = function(queryResponse, separator) {
	var json = {};
	var arrays = $.csv.toArrays(queryResponse, {
		separator: separator
	});
	var detectType = function(value) {
		if (value.indexOf("http") == 0) {
			return "uri";
		} else {
			return null;
		}
	};

	var getBoolean = function() {
		if (arrays.length == 2 && arrays[0].length == 1 && arrays[1].length == 1 && arrays[0][0] == "boolean" && (arrays[1][0] == "1" || arrays[1][0] == "0")) {
			json.boolean = (arrays[1][0] == "1" ? true : false);
			return true;
		}
		return false;
	};

	var getVariables = function() {
		if (arrays.length > 0 && arrays[0].length > 0) {
			json.head = {
				vars: arrays[0]
			};
			return true;
		}
		return false;
	};

	var getBindings = function() {
		if (arrays.length > 1) {
			json.results = {
				bindings: []
			};
			for (var rowIt = 1; rowIt < arrays.length; rowIt++) {
				var binding = {};
				for (var colIt = 0; colIt < arrays[rowIt].length; colIt++) {
					var varName = json.head.vars[colIt];
					if (varName) {
						var value = arrays[rowIt][colIt];
						var detectedType = detectType(value);
						binding[varName] = {
							value: value
						};
						if (detectedType) binding[varName].type = detectedType;
					}
				}

				json.results.bindings.push(binding);
			}
			json.head = {
				vars: arrays[0]
			};
			return true;
		}
		return false;
	};
	var isBoolean = getBoolean();
	if (!isBoolean) {
		var varsFetched = getVariables();
		if (varsFetched) getBindings();
	}

	return json;
};