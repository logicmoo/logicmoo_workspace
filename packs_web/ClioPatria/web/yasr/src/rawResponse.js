'use strict';
var $ = require("jquery"),
	CodeMirror = require("codemirror");

require('codemirror/addon/fold/foldcode.js');
require('codemirror/addon/fold/foldgutter.js');
require('codemirror/addon/fold/xml-fold.js');
require('codemirror/addon/fold/brace-fold.js');

require('codemirror/addon/edit/matchbrackets.js');
require('codemirror/mode/xml/xml.js');
require('codemirror/mode/javascript/javascript.js');

var root = module.exports = function(yasr) {
	var plugin = {};
	var options = $.extend(true, {}, root.defaults);
	var cm = null;
	var draw = function() {
		var cmOptions = options.CodeMirror;
		cmOptions.value = yasr.results.getOriginalResponseAsString();

		var mode = yasr.results.getType();
		if (mode) {
			if (mode == "json") {
				mode = {
					name: "javascript",
					json: true
				};
			}
			cmOptions.mode = mode;
		}

		cm = CodeMirror(yasr.resultsContainer.get()[0], cmOptions);

		//CM has some issues with folding and unfolding (blank parts in the codemirror area, which are only filled after clicking it)
		//so, refresh cm after folding/unfolding
		cm.on('fold', function() {
			cm.refresh();
		});
		cm.on('unfold', function() {
			cm.refresh();
		});

	};
	var canHandleResults = function() {
		if (!yasr.results) return false;
		if (!yasr.results.getOriginalResponseAsString) return false;
		var response = yasr.results.getOriginalResponseAsString();
		if ((!response || response.length == 0) && yasr.results.getException()) return false; //in this case, show exception instead, as we have nothing to show anyway
		return true;
	};

	var getDownloadInfo = function() {
		if (!yasr.results) return null;
		var contentType = yasr.results.getOriginalContentType();
		var type = yasr.results.getType();
		return {
			getContent: function() {
				return yasr.results.getOriginalResponse();
			},
			filename: "queryResults" + (type ? "." + type : ""),
			contentType: (contentType ? contentType : "text/plain"),
			buttonTitle: "Download raw response"
		};
	};

	return {
		draw: draw,
		name: "Raw Response",
		canHandleResults: canHandleResults,
		getPriority: 2,
		getDownloadInfo: getDownloadInfo,

	}
};



root.defaults = {
	CodeMirror: {
		readOnly: true,
		lineNumbers: true,
		lineWrapping: true,
		foldGutter: true,
		gutters: ["CodeMirror-linenumbers", "CodeMirror-foldgutter"]
	}
};

root.version = {
	"YASR-rawResponse": require("../package.json").version,
	"jquery": $.fn.jquery,
	"CodeMirror": CodeMirror.version
};