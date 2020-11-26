'use strict';
var $ = require("jquery");
var root = module.exports = function(xml) {



	/**
	 * head
	 */
	var parseHead = function(node) {
		json.head = {};
		for (var headNodeIt = 0; headNodeIt < node.childNodes.length; headNodeIt++) {
			var headNode = node.childNodes[headNodeIt];
			if (headNode.nodeName == "variable") {
				if (!json.head.vars) json.head.vars = [];
				var name = headNode.getAttribute("name");
				if (name) json.head.vars.push(name);
			}
		}
	};

	var parseResults = function(node) {
		json.results = {};
		json.results.bindings = [];
		for (var resultIt = 0; resultIt < node.childNodes.length; resultIt++) {
			var resultNode = node.childNodes[resultIt];
			var jsonResult = null;

			for (var bindingIt = 0; bindingIt < resultNode.childNodes.length; bindingIt++) {
				var bindingNode = resultNode.childNodes[bindingIt];
				if (bindingNode.nodeName == "binding") {
					var varName = bindingNode.getAttribute("name");
					if (varName) {
						jsonResult = jsonResult || {};
						jsonResult[varName] = {};
						for (var bindingInfIt = 0; bindingInfIt < bindingNode.childNodes.length; bindingInfIt++) {
							var bindingInf = bindingNode.childNodes[bindingInfIt];
							var type = bindingInf.nodeName;
							if (type == "#text") continue;
							jsonResult[varName].type = type;
							jsonResult[varName].value = bindingInf.innerHTML;
							var dataType = bindingInf.getAttribute("datatype");
							if (dataType) jsonResult[varName].datatype = dataType;

						}
					}
				}
			}
			if (jsonResult) json.results.bindings.push(jsonResult);
		}
	};

	var parseBoolean = function(node) {
		if (node.innerHTML == "true") {
			json.boolean = true;
		} else {
			json.boolean = false;
		}
	};
	var mainXml = null;
	if (typeof xml == "string") {
		mainXml = $.parseXML(xml);
	} else if ($.isXMLDoc(xml)) {
		mainXml = xml;
	}
	var xml = null;
	if (mainXml.childNodes.length > 0) {
		//enter the main 'sparql' node
		xml = mainXml.childNodes[0];
	} else {
		return null;
	}
	var json = {};


	for (var i = 0; i < xml.childNodes.length; i++) {
		var node = xml.childNodes[i];
		if (node.nodeName == "head") parseHead(node);
		if (node.nodeName == "results") parseResults(node);
		if (node.nodeName == "boolean") parseBoolean(node);
	}

	return json;
};