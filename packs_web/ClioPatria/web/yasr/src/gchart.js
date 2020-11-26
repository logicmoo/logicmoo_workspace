'use strict';
/**
 * todo: chart height as option
 * 
 */
var $ = require('jquery'),
	utils = require('./utils.js'),
	yUtils = require('yasgui-utils');

var root = module.exports = function(yasr) {

	var options = $.extend(true, {}, root.defaults);
	var id = yasr.container.closest('[id]').attr('id');

	var chartWrapper = null;
	var editor = null;

	var initEditor = function(callback) {
		var google = require('google');
		editor = new google.visualization.ChartEditor();
		google.visualization.events.addListener(editor, 'ok', function() {
			var tmp;
			chartWrapper = editor.getChartWrapper();
			tmp = chartWrapper.getDataTable();
			chartWrapper.setDataTable(null);
			//ugly: need to parse json string to json obj again, as google chart does not provide access to object directly
			options.chartConfig = JSON.parse(chartWrapper.toJSON());
			//remove container ID though, for portability
			if (options.chartConfig.containerId) delete options.chartConfig['containerId'];
			yasr.store();
			chartWrapper.setDataTable(tmp);
			chartWrapper.setOption("width", options.width);
			chartWrapper.setOption("height", options.height);
			chartWrapper.draw();
			yasr.updateHeader();
		});
		if (callback) callback();
	};

	return {
		name: "Google Chart",
		hideFromSelection: false,
		priority: 7,
		options: options,
		getPersistentSettings: function() {
			return {
				chartConfig: options.chartConfig,
				motionChartState: options.motionChartState
			}
		},
		setPersistentSettings: function(persSettings) {
			if (persSettings['chartConfig']) options.chartConfig = persSettings['chartConfig'];
			if (persSettings['motionChartState']) options.motionChartState = persSettings['motionChartState'];
		},
		canHandleResults: function(yasr) {
			var results, variables;
			return (results = yasr.results) != null && (variables = results.getVariables()) && variables.length > 0;
		},
		getDownloadInfo: function() {
			if (!yasr.results) return null;
			var svgEl = yasr.resultsContainer.find('svg');
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
			var $table = yasr.resultsContainer.find('.google-visualization-table-table');
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
		},
		getEmbedHtml: function() {
			if (!yasr.results) return null;

			var svgEl = yasr.resultsContainer.find('svg')
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
		},
		draw: function() {
			var doDraw = function() {
				//clear previous results (if any)
				yasr.resultsContainer.empty();
				var wrapperId = id + '_gchartWrapper';

				yasr.resultsContainer.append(
					$('<button>', {
						class: 'openGchartBtn yasr_btn'
					})
					.text('Chart Config')
					.click(function() {
						editor.openDialog(chartWrapper);
					})
				).append(
					$('<div>', {
						id: wrapperId,
						class: 'gchartWrapper'
					})
				);
				var dataTable = new google.visualization.DataTable();
				var jsonResults = yasr.results.getAsJson();

				jsonResults.head.vars.forEach(function(variable) {
					var type = 'string';
					try {
						type = utils.getGoogleTypeForBindings(jsonResults.results.bindings, variable);
					} catch (e) {
						if (e instanceof require('./exceptions.js').GoogleTypeException) {
							yasr.warn(e.toHtml())
						} else {
							throw e;
						}
					}
					dataTable.addColumn(type, variable);
				});
				var usedPrefixes = null;
				if (yasr.options.getUsedPrefixes) {
					usedPrefixes = (typeof yasr.options.getUsedPrefixes == "function" ? yasr.options.getUsedPrefixes(yasr) : yasr.options.getUsedPrefixes);
				}
				jsonResults.results.bindings.forEach(function(binding) {
					var row = [];
					jsonResults.head.vars.forEach(function(variable, columnId) {
						row.push(utils.castGoogleType(binding[variable], usedPrefixes, dataTable.getColumnType(columnId)));
					})
					dataTable.addRow(row);
				});

				if (options.chartConfig && options.chartConfig.chartType) {
					options.chartConfig.containerId = wrapperId;
					chartWrapper = new google.visualization.ChartWrapper(options.chartConfig);
					if (chartWrapper.getChartType() === "MotionChart" && options.motionChartState) {
						chartWrapper.setOption("state", options.motionChartState);
						google.visualization.events.addListener(chartWrapper, 'ready', function() {
							var motionChart;
							motionChart = chartWrapper.getChart();
							google.visualization.events.addListener(motionChart, 'statechange', function() {
								options.motionChartState = motionChart.getState();
								yasr.store();
							});
						});
					}
					chartWrapper.setDataTable(dataTable);
				} else {
					chartWrapper = new google.visualization.ChartWrapper({
						chartType: 'Table',
						dataTable: dataTable,
						containerId: wrapperId
					});
				}
				chartWrapper.setOption("width", options.width);
				chartWrapper.setOption("height", options.height);
				chartWrapper.draw();
				google.visualization.events.addListener(chartWrapper, 'ready', yasr.updateHeader);
			}

			if (!require('google') || !require('google').visualization || !editor) {
				require('./gChartLoader.js')
					.on('done', function() {
						initEditor();
						doDraw();
					})
					.on('error', function() {
						//TODO: disable or something?
					})
					.googleLoad();
			} else {
				//everything (editor as well) is already initialized
				doDraw();
			}
		}
	};
};
root.defaults = {
	height: "100%",
	width: "100%",
	persistencyId: 'gchart',
	chartConfig: null,
	motionChartState: null
};

function deepEq$(x, y, type) {
	var toString = {}.toString,
		hasOwnProperty = {}.hasOwnProperty,
		has = function(obj, key) {
			return hasOwnProperty.call(obj, key);
		};
	var first = true;
	return eq(x, y, []);

	function eq(a, b, stack) {
		var className, length, size, result, alength, blength, r, key, ref, sizeB;
		if (a == null || b == null) {
			return a === b;
		}
		if (a.__placeholder__ || b.__placeholder__) {
			return true;
		}
		if (a === b) {
			return a !== 0 || 1 / a == 1 / b;
		}
		className = toString.call(a);
		if (toString.call(b) != className) {
			return false;
		}
		switch (className) {
			case '[object String]':
				return a == String(b);
			case '[object Number]':
				return a != +a ? b != +b : (a == 0 ? 1 / a == 1 / b : a == +b);
			case '[object Date]':
			case '[object Boolean]':
				return +a == +b;
			case '[object RegExp]':
				return a.source == b.source &&
					a.global == b.global &&
					a.multiline == b.multiline &&
					a.ignoreCase == b.ignoreCase;
		}
		if (typeof a != 'object' || typeof b != 'object') {
			return false;
		}
		length = stack.length;
		while (length--) {
			if (stack[length] == a) {
				return true;
			}
		}
		stack.push(a);
		size = 0;
		result = true;
		if (className == '[object Array]') {
			alength = a.length;
			blength = b.length;
			if (first) {
				switch (type) {
					case '===':
						result = alength === blength;
						break;
					case '<==':
						result = alength <= blength;
						break;
					case '<<=':
						result = alength < blength;
						break;
				}
				size = alength;
				first = false;
			} else {
				result = alength === blength;
				size = alength;
			}
			if (result) {
				while (size--) {
					if (!(result = size in a == size in b && eq(a[size], b[size], stack))) {
						break;
					}
				}
			}
		} else {
			if ('constructor' in a != 'constructor' in b || a.constructor != b.constructor) {
				return false;
			}
			for (key in a) {
				if (has(a, key)) {
					size++;
					if (!(result = has(b, key) && eq(a[key], b[key], stack))) {
						break;
					}
				}
			}
			if (result) {
				sizeB = 0;
				for (key in b) {
					if (has(b, key)) {
						++sizeB;
					}
				}
				if (first) {
					if (type === '<<=') {
						result = size < sizeB;
					} else if (type === '<==') {
						result = size <= sizeB
					} else {
						result = size === sizeB;
					}
				} else {
					first = false;
					result = size === sizeB;
				}
			}
		}
		stack.pop();
		return result;
	}
}