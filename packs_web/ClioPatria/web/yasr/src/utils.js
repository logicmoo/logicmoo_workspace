'use strict';
var $ = require('jquery'),
	GoogleTypeException = require('./exceptions.js').GoogleTypeException;

module.exports = {
	escapeHtmlEntities: function(unescaped) {
		//taken from http://stackoverflow.com/questions/5499078/fastest-method-to-escape-html-tags-as-html-entities
		return unescaped.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
	},
	uriToPrefixed: function(prefixes, uri) {
		if (prefixes) {
			for (var prefix in prefixes) {
				if (uri.indexOf(prefixes[prefix]) == 0) {
					uri = prefix + ':' + uri.substring(prefixes[prefix].length);
					break;
				}
			}
		}
		return uri;
	},
	getGoogleTypeForBinding: function(binding) {
		if (binding == null) return null;
		if (binding.type != null && (binding.type === 'typed-literal' || binding.type === 'literal')) {
			switch (binding.datatype) {
				case 'http://www.w3.org/2001/XMLSchema#float':
				case 'http://www.w3.org/2001/XMLSchema#decimal':
				case 'http://www.w3.org/2001/XMLSchema#int':
				case 'http://www.w3.org/2001/XMLSchema#integer':
				case 'http://www.w3.org/2001/XMLSchema#long':
				case 'http://www.w3.org/2001/XMLSchema#gYearMonth':
				case 'http://www.w3.org/2001/XMLSchema#gYear':
				case 'http://www.w3.org/2001/XMLSchema#gMonthDay':
				case 'http://www.w3.org/2001/XMLSchema#gDay':
				case 'http://www.w3.org/2001/XMLSchema#gMonth':
					return "number";
				case 'http://www.w3.org/2001/XMLSchema#date':
					return "date";
				case 'http://www.w3.org/2001/XMLSchema#dateTime':
					return "datetime";
				case 'http://www.w3.org/2001/XMLSchema#time':
					return "timeofday";
				default:
					return "string";
			}
		} else {
			return "string";
		}
	},
	getGoogleTypeForBindings: function(bindings, varName) {
		var types = {};
		var typeCount = 0;
		bindings.forEach(function(binding) {
			var type = module.exports.getGoogleTypeForBinding(binding[varName]);
			if (type != null) {
				if (!(type in types)) {
					types[type] = 0;
					typeCount++;
				}
				types[type]++;
			}
		});
		if (typeCount == 0) {
			return 'string';
		} else if (typeCount == 1) {
			for (var type in types) {
				return type; //just return this one
			}
		} else {
			//we have conflicting types. Throw error
			throw new GoogleTypeException(types, varName);
		}
	},

	castGoogleType: function(binding, prefixes, googleType) {
		if (binding == null) {
			return null;
		}

		if (googleType != 'string' && binding.type != null && (binding.type === 'typed-literal' || binding.type === 'literal')) {
			switch (binding.datatype) {
				case 'http://www.w3.org/2001/XMLSchema#float':
				case 'http://www.w3.org/2001/XMLSchema#decimal':
				case 'http://www.w3.org/2001/XMLSchema#int':
				case 'http://www.w3.org/2001/XMLSchema#integer':
				case 'http://www.w3.org/2001/XMLSchema#long':
				case 'http://www.w3.org/2001/XMLSchema#gYearMonth':
				case 'http://www.w3.org/2001/XMLSchema#gYear':
				case 'http://www.w3.org/2001/XMLSchema#gMonthDay':
				case 'http://www.w3.org/2001/XMLSchema#gDay':
				case 'http://www.w3.org/2001/XMLSchema#gMonth':
					return Number(binding.value);
				case 'http://www.w3.org/2001/XMLSchema#date':
					//grrr, the date function does not parse -any- date (including most xsd dates!)
					//datetime and time seem to be fine though.
					//so, first try our custom parser. if that does not work, try the regular date parser anyway
					var date = parseXmlSchemaDate(binding.value);
					if (date) return date;
				case 'http://www.w3.org/2001/XMLSchema#dateTime':
				case 'http://www.w3.org/2001/XMLSchema#time':
					return new Date(binding.value);
				default:
					return binding.value;
			}
		} else {
			if (binding.type = 'uri') {
				return module.exports.uriToPrefixed(prefixes, binding.value);
			} else {
				return binding.value;
			}
		}
	},
	fireClick: function($els) {
		if (!$els)
			return;
		$els.each(function(i, el) {
			var $el = $(el);
			if (document.dispatchEvent) { // W3C
				var oEvent = document.createEvent("MouseEvents");
				oEvent.initMouseEvent("click", true, true, window, 1, 1, 1, 1, 1,
					false, false, false, false, 0, $el[0]);
				$el[0].dispatchEvent(oEvent);
			} else if (document.fireEvent) { // IE
				$el[0].click();
			}
		});
	}
};
//There are no PROPER xml schema to js date parsers
//A few libraries exist: moment, jsdate, Xdate, but none of them parse valid xml schema dates (e.g. 1999-11-05+02:00).
//And: I'm not going to write one myself
//There are other hacky solutions (regular expressions based on trial/error) such as http://stackoverflow.com/questions/2731579/convert-an-xml-schema-date-string-to-a-javascript-date
//But if we're doing hacky stuff, I at least want to do it MYSELF!
var parseXmlSchemaDate = function(dateString) {
	//change +02:00 to Z+02:00 (something which is parseable by js date)
	var date = new Date(dateString.replace(/(\d)([\+-]\d{2}:\d{2})/, '$1Z$2'));
	if (isNaN(date)) return null;
	return date;
};