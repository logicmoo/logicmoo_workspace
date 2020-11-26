'use strict';
var $ = require("jquery");

/**
 * Constructor of plugin which displays SPARQL errors
 * 
 * @param yasr {object}
 * @param parent {DOM element}
 * @param options {object}
 * @class YASR.plugins.boolean
 * @return yasr-erro (doc)
 * 
 */
var root = module.exports = function(yasr) {
	var $container = $("<div class='errorResult'></div>");
	var options = $.extend(true, {}, root.defaults);

	var getTryBtn = function() {
		var $tryBtn = null;
		if (options.tryQueryLink) {
			var link = options.tryQueryLink();
			$tryBtn = $('<button>', {
					class: 'yasr_btn yasr_tryQuery'
				})
				.text('Try query in new browser window')
				.click(function() {
					window.open(link, '_blank');
					$(this).blur();
				})
		}
		return $tryBtn;
	}

	var draw = function() {
		var error = yasr.results.getException();
		$container.empty().appendTo(yasr.resultsContainer);
		var $header = $("<div>", {
			class: 'errorHeader'
		}).appendTo($container);

		if (error.status !== 0) {
			var statusText = 'Error';
			if (error.statusText && error.statusText.length < 100) {
				//use a max: otherwise the alert span will look ugly
				statusText = error.statusText;
			}
			statusText += ' (#' + error.status + ')';

			$header
				.append(
					$("<span>", {
						class: 'exception'
					})
					.text(statusText)
				)
				.append(getTryBtn());

			var responseText = null;
			if (error.responseText) {
				responseText = error.responseText;
			} else if (typeof error == "string") {
				//for backwards compatability (when creating the error string was done externally
				responseText = error;
			}
			if (responseText) $container.append($("<pre>").text(responseText));
		} else {
			$header.append(getTryBtn());
			//cors disabled, wrong url, or endpoint down
			$container
				.append(
					$('<div>', {
						class: 'corsMessage'
					})
					.append(options.corsMessage)
				);
		}

	};


	var canHandleResults = function(yasr) {
		return yasr.results.getException() || false;
	};

	return {
		name: null, //don't need to set this: we don't show it in the selection widget anyway, so don't need a human-friendly name
		draw: draw,
		getPriority: 20,
		hideFromSelection: true,
		canHandleResults: canHandleResults,
	}
};

/**
 * Defaults for error plugin
 * 
 * @type object
 * @attribute YASR.plugins.error.defaults
 */
root.defaults = {
	corsMessage: 'Unable to get response from endpoint',
	tryQueryLink: null,
};