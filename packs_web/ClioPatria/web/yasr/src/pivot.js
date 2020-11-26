'use strict';
var $ = require("jquery"),
	utils = require('./utils.js'),
	yUtils = require('yasgui-utils'),
	imgs = require('./imgs.js');
require('jquery-ui/sortable');
require('pivottable');

if (!$.fn.pivotUI) throw new Error("Pivot lib not loaded");
var root = module.exports = function(yasr) {
	var plugin = {};
	var options = $.extend(true, {}, root.defaults);

	if (options.useD3Chart) {
		try {
			var d3 = require('d3');
			if (d3) require('../node_modules/pivottable/dist/d3_renderers.js');
		} catch (e) {
			//do nothing. just make sure we don't use this renderer
		}
		if ($.pivotUtilities.d3_renderers) $.extend(true, $.pivotUtilities.renderers, $.pivotUtilities.d3_renderers);
	}



	var $pivotWrapper;
	var mergeLabelPostfix = null;
	var getShownVariables = function() {
		var variables = yasr.results.getVariables();
		if (!options.mergeLabelsWithUris) return variables;
		var shownVariables = [];

		mergeLabelPostfix = (typeof options.mergeLabelsWithUris == "string" ? options.mergeLabelsWithUris : "Label");
		variables.forEach(function(variable) {
			if (variable.indexOf(mergeLabelPostfix, variable.length - mergeLabelPostfix.length) !== -1) {
				//this one ends with a postfix
				if (variables.indexOf(variable.substring(0, variable.length - mergeLabelPostfix.length)) >= 0) {
					//we have a shorter version of this variable. So, do not include the ..<postfix> variable in the table
					return;
				}
			}
			shownVariables.push(variable);
		});
		return shownVariables;
	};

	var formatForPivot = function(callback) {

		var vars = getShownVariables();
		var usedPrefixes = null;
		if (yasr.options.getUsedPrefixes) {
			usedPrefixes = (typeof yasr.options.getUsedPrefixes == "function" ? yasr.options.getUsedPrefixes(yasr) : yasr.options.getUsedPrefixes);
		}
		yasr.results.getBindings().forEach(function(binding) {
			var rowObj = {};
			vars.forEach(function(variable) {
				if (variable in binding) {
					var val = binding[variable].value;
					if (mergeLabelPostfix && binding[variable + mergeLabelPostfix]) {
						val = binding[variable + mergeLabelPostfix].value;
					} else if (binding[variable].type == "uri") {
						val = utils.uriToPrefixed(usedPrefixes, val);
					}
					rowObj[variable] = val;
				} else {
					rowObj[variable] = null;
				}
			});
			callback(rowObj);
		});
	}


	var validatePivotTableOptions = function(pivotOptions) {
		//validate settings. we may have different variables, or renderers might be gone
		if (pivotOptions) {
			if (yasr.results) {
				var vars = yasr.results.getVariables();
				var keepColsAndRows = true;
				pivotOptions.cols.forEach(function(variable) {
					if (vars.indexOf(variable) < 0) keepColsAndRows = false;
				});
				if (keepColsAndRows) {
					pivotOptionse.rows.forEach(function(variable) {
						if (vars.indexOf(variable) < 0) keepColsAndRows = false;
					});
				}
				if (!keepColsAndRows) {
					pivotOptions.cols = [];
					pivotOptions.rows = [];
				}
				if (!$.pivotUtilities.renderers[settings.rendererName]) delete pivotOptions.rendererName;
			}
		} else {
			pivotOptions = {};
		}
		return pivotOptions;
	};
	var draw = function() {
		var doDraw = function() {
			var onRefresh = function(pivotObj) {
				options.pivotTable.cols = pivotObj.cols;
				options.pivotTable.rows = pivotObj.rows;
				options.pivotTable.rendererName = pivotObj.rendererName;
				options.pivotTable.aggregatorName = pivotObj.aggregatorName;
				options.pivotTable.vals = pivotObj.vals;
				yasr.store();

				if (pivotObj.rendererName.toLowerCase().indexOf(' chart') >= 0) {
					openGchartBtn.show();
				} else {
					openGchartBtn.hide();
				}
				yasr.updateHeader();
			};


			var openGchartBtn = $('<button>', {
					class: 'openPivotGchart yasr_btn'
				})
				.text('Chart Config')
				.click(function() {
					$pivotWrapper.find('div[dir="ltr"]').dblclick();
				}).appendTo(yasr.resultsContainer);
			$pivotWrapper = $('<div>', {
				class: 'pivotTable'
			}).appendTo($(yasr.resultsContainer));

			options.pivotTable.onRefresh = (function() {
				var originalRefresh = options.pivotTable.onRefresh;
				return function(pivotObj) {
					onRefresh(pivotObj);
					if (originalRefresh) originalRefresh(pivotObj);
				};
			})();

			window.pivot = $pivotWrapper.pivotUI(formatForPivot, options.pivotTable);

			/**
			 * post process
			 */
			//use 'move' handler for variables. This removes the 'filter' button though. Might want to re-enable this in the future
			var icon = $(yUtils.svg.getElement(imgs.move));
			$pivotWrapper.find('.pvtTriangle').replaceWith(icon);

			//add headers to selector rows
			$('.pvtCols').prepend($('<div>', {
				class: 'containerHeader'
			}).text("Columns"));
			$('.pvtRows').prepend($('<div>', {
				class: 'containerHeader'
			}).text("Rows"));
			$('.pvtUnused').prepend($('<div>', {
				class: 'containerHeader'
			}).text("Available Variables"));
			$('.pvtVals').prepend($('<div>', {
				class: 'containerHeader'
			}).text("Cells"));

			//hmmm, directly after the callback finishes (i.e., directly after this line), the svg is draw.
			//just use a short timeout to update the header
			setTimeout(yasr.updateHeader, 400);
		}

		if (yasr.options.useGoogleCharts && options.useGoogleCharts && !$.pivotUtilities.gchart_renderers) {
			require('./gChartLoader.js')
				.on('done', function() {
					try {
						require('../node_modules/pivottable/dist/gchart_renderers.js');
						$.extend(true, $.pivotUtilities.renderers, $.pivotUtilities.gchart_renderers);
					} catch (e) {
						//hmm, still something went wrong. forget about it;
						options.useGoogleCharts = false;
					}
					doDraw();
				})
				.on('error', function() {
					console.log('could not load gchart');
					options.useGoogleCharts = false;
					doDraw();
				})
				.googleLoad();
		} else {
			//everything is already loaded. just draw
			doDraw();
		}
	};
	var canHandleResults = function() {
		return yasr.results && yasr.results.getVariables && yasr.results.getVariables() && yasr.results.getVariables().length > 0;
	};

	var getDownloadInfo = function() {
		if (!yasr.results) return null;
		var svgEl = yasr.resultsContainer.find('.pvtRendererArea svg');
		if (svgEl.length > 0) {

			return {
				getContent: function() {
					if (svgEl[0].outerHTML) {
						return svgEl[0].outerHTML;
					} else {
						//outerHTML not supported. use workaround
						return $('<div>').append(svgEl.clone()).html();
					}
				},

				filename: "queryResults.svg",
				contentType: "image/svg+xml",
				buttonTitle: "Download SVG Image"
			};
		}

		//ok, not a svg. is it a table?
		var $table = yasr.resultsContainer.find('.pvtRendererArea table');
		if ($table.length > 0) {
			return {
				getContent: function() {
					return $table.tableToCsv();
				},
				filename: "queryResults.csv",
				contentType: "text/csv",
				buttonTitle: "Download as CSV"
			};
		}

	};
	var getEmbedHtml = function() {
		if (!yasr.results) return null;

		var svgEl = yasr.resultsContainer.find('.pvtRendererArea svg')
			.clone() //create clone, as we'd like to remove height/width attributes
			.removeAttr('height').removeAttr('width')
			.css('height', '').css('width', '');
		if (svgEl.length == 0) return null;

		var htmlString = svgEl[0].outerHTML;
		if (!htmlString) {
			//outerHTML not supported. use workaround
			htmlString = $('<div>').append(svgEl.clone()).html();
		}
		//wrap in div, so users can more easily tune width/height
		//don't use jquery, so we can easily influence indentation
		return '<div style="width: 800px; height: 600px;">\n' + htmlString + '\n</div>';
	};
	return {
		getPersistentSettings: function() {
			return {
				pivotTable: options.pivotTable
			};
		},
		setPersistentSettings: function(newSettings) {
			if (newSettings.pivotTable) {
				options.pivotTable = validatePivotTableOptions(newSettings.pivotTable);
			}

		},
		getDownloadInfo: getDownloadInfo,
		getEmbedHtml: getEmbedHtml,
		options: options,
		draw: draw,
		name: "Pivot Table",
		canHandleResults: canHandleResults,
		getPriority: 4,
	}
};



root.defaults = {
	mergeLabelsWithUris: false,
	useGoogleCharts: true,
	useD3Chart: true,
	persistencyId: 'pivot',
	pivotTable: {}
};

root.version = {
	"YASR-rawResponse": require("../package.json").version,
	"jquery": $.fn.jquery,
};