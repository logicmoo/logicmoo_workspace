'use strict';
var $ = require("jquery");

/**
 * arg1 can be:
 * - a string (an exception, or e.g. a csv result string)
 * - an object (e.g. xml or json object)
 * - an object with exception key (contains solely a string to display)
 * - a jqXHR object (when this function is used as 'fail' callback for jquery)
 * arg2 can be:
 * - the textstatus of the response
 * arg3 can be:
 * - a jqXHR object (when this function is used as 'done/success' callback for jquery)
 * - an 'errorThrown' string (
 */
var root = module.exports = function(dataOrJqXhr, textStatus, jqXhrOrErrorString) {
	var parsers = {
		xml: require("./xml.js"),
		json: require("./json.js"),
		tsv: require("./tsv.js"),
		csv: require("./csv.js")
	};
	var contentType = null;
	var origResponse = null;
	var json = null;
	var type = null; //json, xml, csv, or tsv
	var exception = null;

	var init = function() {
		if (typeof dataOrJqXhr == "object") {
			/**
			 * Extract exception info (if there is any)
			 */
			if (dataOrJqXhr.exception) {
				//this object just has this exception string, nothing more. (here for backwards compatability)
				exception = dataOrJqXhr.exception;
			} else if (dataOrJqXhr.status != undefined && (dataOrJqXhr.status >= 300 || dataOrJqXhr.status === 0)) {
				//this is an exception, and jquery response
				exception = {
					status: dataOrJqXhr.status
				};
				if (typeof jqXhrOrErrorString == "string") exception.errorString = jqXhrOrErrorString;
				if (dataOrJqXhr.responseText) exception.responseText = dataOrJqXhr.responseText;
				if (dataOrJqXhr.statusText) exception.statusText = dataOrJqXhr.statusText;
			}

			/**
			 * Extract content type info (if there is any)
			 */
			if (dataOrJqXhr.contentType) {
				//this is not a jqXhr object, but a manually generated object (mostly for backwards compatability)
				contentType = dataOrJqXhr.contentType.toLowerCase();
			} else if (dataOrJqXhr.getResponseHeader && dataOrJqXhr.getResponseHeader("content-type")) {
				var ct = dataOrJqXhr.getResponseHeader("content-type").trim().toLowerCase();
				if (ct.length > 0) contentType = ct;
			}

			/**
			 * extract original response
			 */
			if (dataOrJqXhr.response) {
				//this is not a jqXhr object, but a manually generated object (mostly for backwards compatability)
				origResponse = dataOrJqXhr.response;
			} else if (!textStatus && !jqXhrOrErrorString) {
				//not called from jquery, as these other arguments are undefined.
				//so, we can only assume the current object is a proper response (e.g. xml or json) object
				origResponse = dataOrJqXhr;
			}
		}
		if (!exception && !origResponse) {
			//if this is called via a jquery complete callback, we should fetch the result for the jqXHR object
			if (dataOrJqXhr.responseText) {
				origResponse = dataOrJqXhr.responseText;
			} else {
				//if all else fails, assume first arg to be data object
				//(which should be the case for most situations)
				origResponse = dataOrJqXhr;
			}
		}
	};

	var getAsJson = function() {
		if (json) return json;
		if (json === false || exception) return false; //already tried parsing this, and failed. do not try again... 
		var getParserFromContentType = function() {
			if (contentType) {
				if (contentType.indexOf("json") > -1) {
					try {
						json = parsers.json(origResponse);
					} catch (e) {
						exception = e;
					}
					type = "json";
				} else if (contentType.indexOf("xml") > -1) {
					try {
						json = parsers.xml(origResponse);
					} catch (e) {
						exception = e;
					}
					type = "xml";
				} else if (contentType.indexOf("csv") > -1) {
					try {
						json = parsers.csv(origResponse);
					} catch (e) {
						exception = e;
					}
					type = "csv";
				} else if (contentType.indexOf("tab-separated") > -1) {
					try {
						json = parsers.tsv(origResponse);
					} catch (e) {
						exception = e;
					}
					type = "tsv";
				}
			}
		};


		var doLuckyGuess = function() {
			json = parsers.json(origResponse);
			if (json) {
				type = "json";
			} else {
				try {
					json = parsers.xml(origResponse);
					if (json) type = "xml";
				} catch (err) {};
			}
		};


		getParserFromContentType();
		if (!json) {
			doLuckyGuess();
		}
		if (!json) json = false; //explicitly set to false, so we don't try to parse this thing again..
		return json;
	};


	var getVariables = function() {
		var json = getAsJson();
		if (json && "head" in json) {
			return json.head.vars;
		} else {
			return null;
		}
	};

	var getBindings = function() {
		var json = getAsJson();
		if (json && "results" in json) {
			return json.results.bindings;
		} else {
			return null;
		}
	};

	var getBoolean = function() {
		var json = getAsJson();
		if (json && "boolean" in json) {
			return json.boolean;
		} else {
			return null;
		}
	};
	var getOriginalResponse = function() {
		return origResponse;
	};
	var getOriginalResponseAsString = function() {
		var responseString = "";
		if (typeof origResponse == "string") {
			responseString = origResponse;
		} else if (type == "json") {
			responseString = JSON.stringify(origResponse, undefined, 2); //prettifies as well
		} else if (type == "xml") {
			responseString = new XMLSerializer().serializeToString(origResponse);
		}
		return responseString;
	};
	var getException = function() {
		return exception;
	};
	var getType = function() {
		if (type == null) getAsJson(); //detects type as well
		return type;
	};

	//process the input parameters in such a way that we can store it in local storage (i.e., no function)
	//and, make sure we can easily pass it on back to this wrapper function when loading it again from storage
	var getAsStoreObject = function() {
		var storeArray = [];
		var arg1 = {};
		if (dataOrJqXhr.status) {
			//jqXhr object
			arg1.status = dataOrJqXhr.status;
			arg1.responseText = dataOrJqXhr.responseText;
			arg1.statusText = dataOrJqXhr.statusText;
			arg1.contentType = contentType; //this is a function in a jqXhr object (problem for storing). but this wrapper will read it as string as well
		} else {
			//the other instances of this param (whether it is a json, xml, or exception object), we can normally store
			arg1 = dataOrJqXhr;
		}


		var arg2 = textStatus;
		var arg3 = undefined;
		if (typeof jqXhrOrErrorString == "string") arg3 = jqXhrOrErrorString;

		return [arg1, arg2, arg3];
	};



	init();
	json = getAsJson();

	return {
		getAsStoreObject: getAsStoreObject,
		getAsJson: getAsJson,
		getOriginalResponse: getOriginalResponse,
		getOriginalResponseAsString: getOriginalResponseAsString,
		getOriginalContentType: function() {
			return contentType;
		},
		getVariables: getVariables,
		getBindings: getBindings,
		getBoolean: getBoolean,
		getType: getType,
		getException: getException
	};
};