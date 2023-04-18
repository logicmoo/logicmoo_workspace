// Transcrypt'ed from Python, 2023-03-06 00:46:41
var arc_types = {};
var constants = {};
import {AssertionError, AttributeError, BaseException, DeprecationWarning, Exception, IndexError, IterableError, KeyError, NotImplementedError, RuntimeWarning, StopIteration, UserWarning, ValueError, Warning, __JsIterator__, __PyIterator__, __Terminal__, __add__, __and__, __call__, __class__, __envir__, __eq__, __floordiv__, __ge__, __get__, __getcm__, __getitem__, __getslice__, __getsm__, __gt__, __i__, __iadd__, __iand__, __idiv__, __ijsmod__, __ilshift__, __imatmul__, __imod__, __imul__, __in__, __init__, __ior__, __ipow__, __irshift__, __isub__, __ixor__, __jsUsePyNext__, __jsmod__, __k__, __kwargtrans__, __le__, __lshift__, __lt__, __matmul__, __mergefields__, __mergekwargtrans__, __mod__, __mul__, __ne__, __neg__, __nest__, __or__, __pow__, __pragma__, __pyUseJsNext__, __rshift__, __setitem__, __setproperty__, __setslice__, __sort__, __specialattrib__, __sub__, __super__, __t__, __terminal__, __truediv__, __withblock__, __xor__, abs, all, any, assert, bool, bytearray, bytes, callable, chr, copy, deepcopy, delattr, dict, dir, divmod, enumerate, filter, float, getattr, hasattr, input, int, isinstance, issubclass, len, list, map, max, min, object, ord, pow, print, property, py_TypeError, py_iter, py_metatype, py_next, py_reversed, py_typeof, range, repr, round, set, setattr, sorted, str, sum, tuple, zip} from './org.transcrypt.__runtime__.js';
import {DOWN, DOWN_LEFT, EIGHT, F, FIVE, FOUR, LEFT, NEG_ONE, NEG_TWO, NEG_UNITY, NINE, ONE, ORIGIN, RIGHT, SEVEN, SIX, T, TEN, THREE, THREE_BY_THREE, TWO, TWO_BY_TWO, TWO_BY_ZERO, UNITY, UP, UP_RIGHT, ZERO, ZERO_BY_TWO} from './constants.js';
import {} from './solvers.js';
import {A, B, C, D, E, G, H, I, J, K, test_add, test_adjacent, test_apply, test_argmax, test_argmin, test_asindices, test_asobject, test_astuple, test_backdrop, test_bordering, test_both, test_bottomhalf, test_box, test_branch, test_canvas, test_cellwise, test_center, test_centerofmass, test_chain, test_cmirror, test_color, test_colorcount, test_colorfilter, test_combine, test_compose, test_compress, test_connect, test_contained, test_corners, test_cover, test_crement, test_crop, test_decrement, test_dedupe, test_delta, test_difference, test_divide, test_dmirror, test_dneighbors, test_double, test_downscale, test_either, test_equality, test_even, test_extract, test_fgpartition, test_fill, test_first, test_flip, test_fork, test_frontiers, test_gravitate, test_greater, test_halve, test_hconcat, test_height, test_hfrontier, test_hline, test_hmatching, test_hmirror, test_hperiod, test_hsplit, test_hupscale, test_identity, test_inbox, test_increment, test_index, test_ineighbors, test_initset, test_insert, test_intersection, test_interval, test_invert, test_last, test_lbind, test_leastcolor, test_leastcommon, test_lefthalf, test_leftmost, test_llcorner, test_lowermost, test_lrcorner, test_manhattan, test_mapply, test_matcher, test_maximum, test_merge, test_mfilter, test_minimum, test_mostcolor, test_mostcommon, test_move, test_mpapply, test_multiply, test_neighbors, test_normalize, test_numcolors, test_objects, test_occurrences, test_ofcolor, test_order, test_other, test_outbox, test_paint, test_pair, test_palette, test_papply, test_partition, test_portrait, test_position, test_positive, test_power, test_prapply, test_product, test_rapply, test_rbind, test_recolor, test_remove, test_repeat, test_replace, test_righthalf, test_rightmost, test_rot180, test_rot270, test_rot90, test_sfilter, test_shape, test_shift, test_shoot, test_sign, test_size, test_sizefilter, test_square, test_subgrid, test_subtract, test_switch, test_toindices, test_toivec, test_tojvec, test_toobject, test_tophalf, test_totuple, test_trim, test_ulcorner, test_underfill, test_underpaint, test_uppermost, test_upscale, test_urcorner, test_valmax, test_valmin, test_vconcat, test_vfrontier, test_vline, test_vmatching, test_vmirror, test_vperiod, test_vsplit, test_vupscale, test_width} from './tests.js';
import {, py_replace, py_switch} from './dsl.js';
import * as __module_constants__ from './constants.js';
__nest__ (constants, '', __module_constants__);
import * as __module_arc_types__ from './arc_types.js';
__nest__ (arc_types, '', __module_arc_types__);
import {Boolean, Cell, ContainerContainer, Element, Grid, Indices, IndicesSet, Integer, IntegerSet, IntegerTuple, Numerical, Object, Objects, Patch, Piece, TupleTuple} from './arc_types.js';
import {, Address, Area, Article, Aside, Audio, Blockquote, Body, Br, Button, Canvas, Caption, Cite, Code, Col, Datalist, Dd, Div, Dl, Dt, Em, Embed, Fieldset, Figcaption, Figure, Footer, Form, H1, H2, H3, H4, H5, H6, Head, Header, Hr, Html, HtmlElement, Img, Input, KWElement, Label, Legend, Li, Link, Main, Map, Meta, Meter, Nav, Noscript, Ol, Optgroup, Option, Output, P, Param, Pre, Progress, S, Samp, Script, Section, Select, Small, Source, Span, Strong, Style, Sub, Sup, Table, Tbody, Td, Textarea, Tfoot, Th, Thead, Title, Tr, Track, U, Ul, Video, convertAttrKeys, indented, renderCss, renderInlineStyle} from './htmltree.htmltree.js';
var __name__ = '__main__';
export var Common =  __class__ ('Common', [object], {
	__module__: __name__,
	get __init__ () {return __get__ (this, function (self, nitems, stepsize) {
		self.nitems = nitems;
		self.stepsize = stepsize;
		self.statekeys = (function () {
			var __accu0__ = [];
			for (var n = 0; n < nitems; n++) {
				__accu0__.append ('item{}'.format (n));
			}
			return __accu0__;
		}) ();
	});}
});
export var common = Common (10, 0.5);
try {
	
	try {
		var _state = dict ({});
		var _prior_state = dict ({});
		var _readouts = null;
		var makeBody = function () {
			var banner = H1 ('Nearly Pure Python Web App Demo', __kwargtrans__ ({style: dict (__kwargtrans__ ({color: 'yellow'}))}));
			var projectlink = A ('Source Code on GitHub', __kwargtrans__ ({href: 'https://github.com/Michael-F-Ellis/NearlyPurePythonWebAppDemo'}));
			var subbanner = H2 (projectlink);
			var header = Div (banner, subbanner, __kwargtrans__ ({style: dict (__kwargtrans__ ({text_align: 'center'}))}));
			var readouts = [];
			for (var datakey of common.statekeys) {
				var meter = Meter (__kwargtrans__ ({min: '0.1', low: '2.0', high: '8.0', max: '10.0', style: dict (__kwargtrans__ ({width: '25%', margin_top: '5px', margin_bottom: '5px'}))}));
				var value = Span ();
				readouts.append (Div (meter, value, __kwargtrans__ ({_class: 'readout', data_key: datakey})));
			}
			var slider = Input (__kwargtrans__ ({id: 'stepinput', _type: 'range', min: '0.1', max: '10.0', step: '0.1', style: dict (__kwargtrans__ ({margin: '1em'}))}));
			var stepinput = Label ('Step Size', slider, __kwargtrans__ ({style: dict (__kwargtrans__ ({color: 'white'}))}));
			var stepdiv = Div (stepinput, Span (__kwargtrans__ ({id: 'stepvalue', style: dict (__kwargtrans__ ({color: 'white'}))})), __kwargtrans__ ({style: dict (__kwargtrans__ ({margin: '20px'}))}));
			var bodycontent = Div (header);
			bodycontent.C.extend (readouts);
			bodycontent.C.append (stepdiv);
			print (bodycontent.render (0));
			document.body.innerHTML = bodycontent.render ();
		};
		var triggerCustomEvent = function (py_name, data) {
			if (window.CustomEvent) {
				var event = new CustomEvent (py_name, dict ({'detail': data}));
			}
			else {
				var event = document.createEvent ('CustomEvent');
				event.initCustomEvent (py_name, true, true, data);
			}
			document.dispatchEvent (event);
		};
		var getJSON = function (url, f) {
			var request = new XMLHttpRequest ();
			request.open ('GET', url, true);
			var onload = function () {
				if ((200 <= request.status && request.status < 400)) {
					var data = JSON.parse (request.responseText);
					f (data);
				}
				else {
					var _ = 'Server returned {} for getJSON request on {}'.format (request.status, url);
					console.log (_);
				}
			};
			var onerror = function () {
				var _ = 'Connection error for getJSON request on {}'.format (url);
				console.log (_);
			};
			request.onload = onload;
			request.onerror = onerror;
			request.send ();
		};
		var post = function (url, data) {
			var request = new XMLHttpRequest ();
			request.open ('POST', url, true);
			request.setRequestHeader ('Content-Type', 'application/x-www-form-urlencoded; charset=UTF-8');
			var ldata = [];
			for (var [k, v] of data.py_items ()) {
				if (data.hasOwnProperty (k)) {
					var lh = encodeURIComponent (k);
					var rh = encodeURIComponent (v);
					ldata.append ('{}={}'.format (lh, rh));
				}
			}
			request.send ('&'.join (ldata));
		};
		var getState = function () {
			var f = function (data) {
				_prior_state.py_update (_state);
				_state = data;
				triggerCustomEvent ('state:update', dict ({}));
			};
			getJSON ('/getstate', f);
			return ;
		};
		var update_readouts = function () {
			var queue = [];
			for (var el of _readouts) {
				var key = el.getAttribute ('data-key');
				var value = _state [key];
				var valuef = float (value);
				if (valuef <= 2.0) {
					var color = 'deepskyblue';
				}
				else if (valuef >= 8.0) {
					var color = 'red';
				}
				else {
					var color = 'green';
				}
				queue.append (tuple ([el, value, color]));
			}
			for (var [el, value, color] of queue) {
				var __left0__ = tuple ([el.children [0], el.children [1]]);
				var meter = __left0__ [0];
				var span = __left0__ [1];
				span.textContent = value;
				span.setAttribute ('style', 'padding-left:0.5em; color:{}; font-size:32;'.format (color));
				meter.value = value;
			}
			var inp = document.getElementById ('stepinput');
			var readout = document.getElementById ('stepvalue');
			if (inp != document.activeElement) {
				inp.value = _state ['stepsize'];
				readout.style.color = 'white';
				readout.innerHTML = _state ['stepsize'];
			}
		};
		var handle_stepchange = function (event) {
			var fail_msg = 'Step size must be a number between 0 and 10';
			var v = document.getElementById ('stepinput').value;
			var vj = parseFloat(v); var isfloat = !isNaN(vj);
			if (isfloat && (0.0 <= vj && vj <= 10.0)) {
				post ('/setstepsize', dict ({'stepsize': v}));
				return false;
			}
			else {
				alert (fail_msg);
				return false;
			}
		};
		var handle_stepinput = function (event) {
			var readout = document.getElementById ('stepvalue');
			var v = document.getElementById ('stepinput').value;
			readout.style.color = 'yellow';
			readout.innerHTML = v;
			return false;
		};
		var start = function () {
			makeBody ();
			_readouts = document.querySelectorAll ('.readout');
			for (var el of _readouts) {
				el.style.fontSize = '12';
			}
			var ssinput = document.getElementById ('stepinput');
			ssinput.addEventListener ('change', handle_stepchange);
			ssinput.addEventListener ('input', handle_stepinput);
			document.addEventListener ('state:update', update_readouts);
			var py_update = function () {
				getState ();
				if (_prior_state !== null && _prior_state.hasOwnProperty ('server_start_time')) {
					if (_state ['server_start_time'] > _prior_state ['server_start_time']) {
						location.reload (true);
					}
				}
			};
			py_update ();
			window.setInterval (py_update, 500);
		};
		document.addEventListener ('DOMContentLoaded', start);
	}
	catch (__except0__) {
	}
}
catch (__except0__) {
	if (isinstance (__except0__, NameError)) {
		// pass;
	}
	else {
		throw __except0__;
	}
}
export var identity = function (x) {
	return x;
};
export var add = function (a, b) {
	if (isinstance (a, int) && isinstance (b, int)) {
		return a + b;
	}
	else if (isinstance (a, tuple) && isinstance (b, tuple)) {
		return tuple ([a [0] + b [0], a [1] + b [1]]);
	}
	else if (isinstance (a, int) && isinstance (b, tuple)) {
		return tuple ([a + b [0], a + b [1]]);
	}
	return tuple ([a [0] + b, a [1] + b]);
};
export var subtract = function (a, b) {
	if (isinstance (a, int) && isinstance (b, int)) {
		return a - b;
	}
	else if (isinstance (a, tuple) && isinstance (b, tuple)) {
		return tuple ([a [0] - b [0], a [1] - b [1]]);
	}
	else if (isinstance (a, int) && isinstance (b, tuple)) {
		return tuple ([a - b [0], a - b [1]]);
	}
	return tuple ([a [0] - b, a [1] - b]);
};
export var multiply = function (a, b) {
	if (isinstance (a, int) && isinstance (b, int)) {
		return a * b;
	}
	else if (isinstance (a, tuple) && isinstance (b, tuple)) {
		return tuple ([a [0] * b [0], a [1] * b [1]]);
	}
	else if (isinstance (a, int) && isinstance (b, tuple)) {
		return tuple ([a * b [0], a * b [1]]);
	}
	return tuple ([a [0] * b, a [1] * b]);
};
export var divide = function (a, b) {
	if (isinstance (a, int) && isinstance (b, int)) {
		return Math.floor (a / b);
	}
	else if (isinstance (a, tuple) && isinstance (b, tuple)) {
		return tuple ([Math.floor (a [0] / b [0]), Math.floor (a [1] / b [1])]);
	}
	else if (isinstance (a, int) && isinstance (b, tuple)) {
		return tuple ([Math.floor (a / b [0]), Math.floor (a / b [1])]);
	}
	return tuple ([Math.floor (a [0] / b), Math.floor (a [1] / b)]);
};
export var invert = function (n) {
	return (isinstance (n, int) ? -(n) : tuple ([-(n [0]), -(n [1])]));
};
export var even = function (n) {
	return __mod__ (n, 2) == 0;
};
export var double = function (n) {
	return (isinstance (n, int) ? n * 2 : tuple ([n [0] * 2, n [1] * 2]));
};
export var halve = function (n) {
	return (isinstance (n, int) ? Math.floor (n / 2) : tuple ([Math.floor (n [0] / 2), Math.floor (n [1] / 2)]));
};
export var flip = function (b) {
	return !(b);
};
export var equality = function (a, b) {
	return a == b;
};
export var contained = function (value, container) {
	return __in__ (value, container);
};
export var combine = function (a, b) {
	return py_typeof (a) (tuple ([a, b]));
};
export var intersection = function (a, b) {
	return a & b;
};
export var difference = function (a, b) {
	return a - b;
};
export var dedupe = function (tup) {
	return tuple ((function () {
		var __accu0__ = [];
		for (var [i, e] of enumerate (tup)) {
			if (tup.index (e) == i) {
				__accu0__.append (e);
			}
		}
		return py_iter (__accu0__);
	}) ());
};
export var order = function (container, compfunc) {
	return tuple (sorted (container, __kwargtrans__ ({key: compfunc})));
};
export var repeat = function (item, num) {
	return tuple ((function () {
		var __accu0__ = [];
		for (var i = 0; i < num; i++) {
			__accu0__.append (item);
		}
		return py_iter (__accu0__);
	}) ());
};
export var greater = function (a, b) {
	return a > b;
};
export var size = function (container) {
	return len (container);
};
export var merge = function (containers) {
	return py_typeof (containers) ((function () {
		var __accu0__ = [];
		for (var c of containers) {
			for (var e of c) {
				__accu0__.append (e);
			}
		}
		return py_iter (__accu0__);
	}) ());
};
export var maximum = function (container) {
	return max (container, __kwargtrans__ ({py_default: 0}));
};
export var minimum = function (container) {
	return min (container, __kwargtrans__ ({py_default: 0}));
};
export var valmax = function (container, compfunc) {
	return compfunc (max (container, __kwargtrans__ ({key: compfunc, py_default: 0})));
};
export var valmin = function (container, compfunc) {
	return compfunc (min (container, __kwargtrans__ ({key: compfunc, py_default: 0})));
};
export var argmax = function (container, compfunc) {
	return max (container, __kwargtrans__ ({key: compfunc}));
};
export var argmin = function (container, compfunc) {
	return min (container, __kwargtrans__ ({key: compfunc}));
};
export var mostcommon = function (container) {
	return max (set (container), __kwargtrans__ ({key: container.count}));
};
export var leastcommon = function (container) {
	return min (set (container), __kwargtrans__ ({key: container.count}));
};
export var initset = function (value) {
	return frozenset (new set ([value]));
};
export var both = function (a, b) {
	return a && b;
};
export var either = function (a, b) {
	return a || b;
};
export var increment = function (x) {
	return (isinstance (x, int) ? x + 1 : tuple ([x [0] + 1, x [1] + 1]));
};
export var decrement = function (x) {
	return (isinstance (x, int) ? x - 1 : tuple ([x [0] - 1, x [1] - 1]));
};
export var crement = function (x) {
	if (isinstance (x, int)) {
		return (x == 0 ? 0 : (x > 0 ? x + 1 : x - 1));
	}
	return tuple ([(x [0] == 0 ? 0 : (x [0] > 0 ? x [0] + 1 : x [0] - 1)), (x [1] == 0 ? 0 : (x [1] > 0 ? x [1] + 1 : x [1] - 1))]);
};
export var sign = function (x) {
	if (isinstance (x, int)) {
		return (x == 0 ? 0 : (x > 0 ? 1 : -(1)));
	}
	return tuple ([(x [0] == 0 ? 0 : (x [0] > 0 ? 1 : -(1))), (x [1] == 0 ? 0 : (x [1] > 0 ? 1 : -(1)))]);
};
export var positive = function (x) {
	return x > 0;
};
export var toivec = function (i) {
	return tuple ([i, 0]);
};
export var tojvec = function (j) {
	return tuple ([0, j]);
};
export var sfilter = function (container, condition) {
	return py_typeof (container) ((function () {
		var __accu0__ = [];
		for (var e of container) {
			if (condition (e)) {
				__accu0__.append (e);
			}
		}
		return py_iter (__accu0__);
	}) ());
};
export var mfilter = function (container, function) {
	return merge (sfilter (container, function));
};
export var extract = function (container, condition) {
	return py_next ((function () {
		var __accu0__ = [];
		for (var e of container) {
			if (condition (e)) {
				__accu0__.append (e);
			}
		}
		return py_iter (__accu0__);
	}) ());
};
export var totuple = function (container) {
	return tuple (container);
};
export var first = function (container) {
	return py_next (py_iter (container));
};
export var last = function (container) {
	return max (enumerate (container)) [1];
};
export var insert = function (value, container) {
	return container.union (frozenset (new set ([value])));
};
export var remove = function (value, container) {
	return py_typeof (container) ((function () {
		var __accu0__ = [];
		for (var e of container) {
			if (e != value) {
				__accu0__.append (e);
			}
		}
		return py_iter (__accu0__);
	}) ());
};
export var other = function (container, value) {
	return first (remove (value, container));
};
export var interval = function (start, stop, step) {
	return tuple (range (start, stop, step));
};
export var astuple = function (a, b) {
	return tuple ([a, b]);
};
export var product = function (a, b) {
	return frozenset ((function () {
		var __accu0__ = [];
		for (var j of b) {
			for (var i of a) {
				__accu0__.append (tuple ([i, j]));
			}
		}
		return py_iter (__accu0__);
	}) ());
};
export var pair = function (a, b) {
	return tuple (zip (a, b));
};
export var branch = function (condition, a, b) {
	return (condition ? a : b);
};
export var compose = function (outer, inner) {
	return (function __lambda__ (x) {
		return outer (inner (x));
	});
};
export var chain = function (h, g, f) {
	return (function __lambda__ (x) {
		return h (g (f (x)));
	});
};
export var matcher = function (function, target) {
	return (function __lambda__ (x) {
		return function (x) == target;
	});
};
export var rbind = function (function, fixed) {
	var n = function.__code__.co_argcount;
	if (n == 2) {
		return (function __lambda__ (x) {
			return function (x, fixed);
		});
	}
	else if (n == 3) {
		return (function __lambda__ (x, y) {
			return function (x, y, fixed);
		});
	}
	else {
		return (function __lambda__ (x, y, z) {
			return function (x, y, z, fixed);
		});
	}
};
export var lbind = function (function, fixed) {
	var n = function.__code__.co_argcount;
	if (n == 2) {
		return (function __lambda__ (y) {
			return function (fixed, y);
		});
	}
	else if (n == 3) {
		return (function __lambda__ (y, z) {
			return function (fixed, y, z);
		});
	}
	else {
		return (function __lambda__ (y, z, a) {
			return function (fixed, y, z, a);
		});
	}
};
export var power = function (function, n) {
	if (n == 1) {
		return function;
	}
	return compose (function, power (function, n - 1));
};
export var fork = function (outer, a, b) {
	return (function __lambda__ (x) {
		return outer (a (x), b (x));
	});
};
export var apply = function (function, container) {
	return py_typeof (container) ((function () {
		var __accu0__ = [];
		for (var e of container) {
			__accu0__.append (function (e));
		}
		return py_iter (__accu0__);
	}) ());
};
export var rapply = function (functions, value) {
	return py_typeof (functions) ((function () {
		var __accu0__ = [];
		for (var function of functions) {
			__accu0__.append (function (value));
		}
		return py_iter (__accu0__);
	}) ());
};
export var mapply = function (function, container) {
	return merge (apply (function, container));
};
export var papply = function (function, a, b) {
	return tuple ((function () {
		var __accu0__ = [];
		for (var [i, j] of zip (a, b)) {
			__accu0__.append (function (i, j));
		}
		return py_iter (__accu0__);
	}) ());
};
export var mpapply = function (function, a, b) {
	return merge (papply (function, a, b));
};
export var prapply = function (function, a, b) {
	return frozenset ((function () {
		var __accu0__ = [];
		for (var j of b) {
			for (var i of a) {
				__accu0__.append (function (i, j));
			}
		}
		return py_iter (__accu0__);
	}) ());
};
export var mostcolor = function (element) {
	var py_values = (isinstance (element, tuple) ? (function () {
		var __accu0__ = [];
		for (var r of element) {
			for (var v of r) {
				__accu0__.append (v);
			}
		}
		return __accu0__;
	}) () : (function () {
		var __accu0__ = [];
		for (var [v, _] of element) {
			__accu0__.append (v);
		}
		return __accu0__;
	}) ());
	return max (set (py_values), __kwargtrans__ ({key: py_values.count}));
};
export var leastcolor = function (element) {
	var py_values = (isinstance (element, tuple) ? (function () {
		var __accu0__ = [];
		for (var r of element) {
			for (var v of r) {
				__accu0__.append (v);
			}
		}
		return __accu0__;
	}) () : (function () {
		var __accu0__ = [];
		for (var [v, _] of element) {
			__accu0__.append (v);
		}
		return __accu0__;
	}) ());
	return min (set (py_values), __kwargtrans__ ({key: py_values.count}));
};
export var height = function (piece) {
	if (len (piece) == 0) {
		return 0;
	}
	if (isinstance (piece, tuple)) {
		return len (piece);
	}
	return (lowermost (piece) - uppermost (piece)) + 1;
};
export var width = function (piece) {
	if (len (piece) == 0) {
		return 0;
	}
	if (isinstance (piece, tuple)) {
		return len (piece [0]);
	}
	return (rightmost (piece) - leftmost (piece)) + 1;
};
export var shape = function (piece) {
	return tuple ([height (piece), width (piece)]);
};
export var portrait = function (piece) {
	return height (piece) > width (piece);
};
export var colorcount = function (element, value) {
	if (isinstance (element, tuple)) {
		return sum ((function () {
			var __accu0__ = [];
			for (var row of element) {
				__accu0__.append (row.count (value));
			}
			return py_iter (__accu0__);
		}) ());
	}
	return sum ((function () {
		var __accu0__ = [];
		for (var [v, _] of element) {
			__accu0__.append (v == value);
		}
		return py_iter (__accu0__);
	}) ());
};
export var colorfilter = function (objs, value) {
	return frozenset ((function () {
		var __accu0__ = [];
		for (var obj of objs) {
			if (py_next (py_iter (obj)) [0] == value) {
				__accu0__.append (obj);
			}
		}
		return py_iter (__accu0__);
	}) ());
};
export var sizefilter = function (container, n) {
	return frozenset ((function () {
		var __accu0__ = [];
		for (var item of container) {
			if (len (item) == n) {
				__accu0__.append (item);
			}
		}
		return py_iter (__accu0__);
	}) ());
};
export var asindices = function (grid) {
	return frozenset ((function () {
		var __accu0__ = [];
		for (var i = 0; i < len (grid); i++) {
			for (var j = 0; j < len (grid [0]); j++) {
				__accu0__.append (tuple ([i, j]));
			}
		}
		return py_iter (__accu0__);
	}) ());
};
export var ofcolor = function (grid, value) {
	return frozenset ((function () {
		var __accu0__ = [];
		for (var [i, r] of enumerate (grid)) {
			for (var [j, v] of enumerate (r)) {
				if (v == value) {
					__accu0__.append (tuple ([i, j]));
				}
			}
		}
		return py_iter (__accu0__);
	}) ());
};
export var ulcorner = function (patch) {
	return tuple (map (min, zip (...toindices (patch))));
};
export var urcorner = function (patch) {
	return tuple (map ((function __lambda__ (ix) {
		return dict ({0: min, 1: max}) [ix [0]] (ix [1]);
	}), enumerate (zip (...toindices (patch)))));
};
export var llcorner = function (patch) {
	return tuple (map ((function __lambda__ (ix) {
		return dict ({0: max, 1: min}) [ix [0]] (ix [1]);
	}), enumerate (zip (...toindices (patch)))));
};
export var lrcorner = function (patch) {
	return tuple (map (max, zip (...toindices (patch))));
};
export var crop = function (grid, start, dims) {
	return tuple ((function () {
		var __accu0__ = [];
		for (var r of grid.__getslice__ (start [0], start [0] + dims [0], 1)) {
			__accu0__.append (r.__getslice__ (start [1], start [1] + dims [1], 1));
		}
		return py_iter (__accu0__);
	}) ());
};
export var toindices = function (patch) {
	if (len (patch) == 0) {
		return frozenset ();
	}
	if (isinstance (py_next (py_iter (patch)) [1], tuple)) {
		return frozenset ((function () {
			var __accu0__ = [];
			for (var [value, index] of patch) {
				__accu0__.append (index);
			}
			return py_iter (__accu0__);
		}) ());
	}
	return patch;
};
export var recolor = function (value, patch) {
	return frozenset ((function () {
		var __accu0__ = [];
		for (var index of toindices (patch)) {
			__accu0__.append (tuple ([value, index]));
		}
		return py_iter (__accu0__);
	}) ());
};
export var shift = function (patch, directions) {
	var __left0__ = directions;
	var di = __left0__ [0];
	var dj = __left0__ [1];
	if (isinstance (py_next (py_iter (patch)) [1], tuple)) {
		return frozenset ((function () {
			var __accu0__ = [];
			for (var [value, [i, j]] of patch) {
				__accu0__.append (tuple ([value, tuple ([i + di, j + dj])]));
			}
			return py_iter (__accu0__);
		}) ());
	}
	return frozenset ((function () {
		var __accu0__ = [];
		for (var [i, j] of patch) {
			__accu0__.append (tuple ([i + di, j + dj]));
		}
		return py_iter (__accu0__);
	}) ());
};
export var normalize = function (patch) {
	return shift (patch, tuple ([-(uppermost (patch)), -(leftmost (patch))]));
};
export var dneighbors = function (loc) {
	return frozenset (new set ([tuple ([loc [0] - 1, loc [1]]), tuple ([loc [0] + 1, loc [1]]), tuple ([loc [0], loc [1] - 1]), tuple ([loc [0], loc [1] + 1])]));
};
export var ineighbors = function (loc) {
	return frozenset (new set ([tuple ([loc [0] - 1, loc [1] - 1]), tuple ([loc [0] - 1, loc [1] + 1]), tuple ([loc [0] + 1, loc [1] - 1]), tuple ([loc [0] + 1, loc [1] + 1])]));
};
export var neighbors = function (loc) {
	return dneighbors (loc) | ineighbors (loc);
};
export var objects = function (grid, univalued, diagonal, without_bg) {
	var bg = (without_bg ? mostcolor (grid) : null);
	var objs = set ();
	var occupied = set ();
	var __left0__ = tuple ([len (grid), len (grid [0])]);
	var h = __left0__ [0];
	var w = __left0__ [1];
	var unvisited = asindices (grid);
	var diagfun = (diagonal ? neighbors : dneighbors);
	for (var loc of unvisited) {
		if (__in__ (loc, occupied)) {
			continue;
		}
		var val = grid [loc [0]] [loc [1]];
		if (val == bg) {
			continue;
		}
		var obj = new set ([tuple ([val, loc])]);
		var cands = new set ([loc]);
		while (len (cands) > 0) {
			var neighborhood = set ();
			for (var cand of cands) {
				var v = grid [cand [0]] [cand [1]];
				if ((univalued ? val == v : v != bg)) {
					obj.add (tuple ([v, cand]));
					occupied.add (cand);
					neighborhood |= (function () {
						var __accu0__ = [];
						for (var [i, j] of diagfun (cand)) {
							if ((0 <= i && i < h) && (0 <= j && j < w)) {
								__accu0__.append (tuple ([i, j]));
							}
						}
						return set (__accu0__);
					}) ();
				}
			}
			var cands = neighborhood - occupied;
		}
		objs.add (frozenset (obj));
	}
	return frozenset (objs);
};
export var partition = function (grid) {
	return frozenset ((function () {
		var __accu0__ = [];
		for (var value of palette (grid)) {
			__accu0__.append (frozenset ((function () {
				var __accu1__ = [];
				for (var [i, r] of enumerate (grid)) {
					for (var [j, v] of enumerate (r)) {
						if (v == value) {
							__accu1__.append (tuple ([v, tuple ([i, j])]));
						}
					}
				}
				return py_iter (__accu1__);
			}) ()));
		}
		return py_iter (__accu0__);
	}) ());
};
export var fgpartition = function (grid) {
	return frozenset ((function () {
		var __accu0__ = [];
		for (var value of palette (grid) - new set ([mostcolor (grid)])) {
			__accu0__.append (frozenset ((function () {
				var __accu1__ = [];
				for (var [i, r] of enumerate (grid)) {
					for (var [j, v] of enumerate (r)) {
						if (v == value) {
							__accu1__.append (tuple ([v, tuple ([i, j])]));
						}
					}
				}
				return py_iter (__accu1__);
			}) ()));
		}
		return py_iter (__accu0__);
	}) ());
};
export var uppermost = function (patch) {
	return min ((function () {
		var __accu0__ = [];
		for (var [i, j] of toindices (patch)) {
			__accu0__.append (i);
		}
		return py_iter (__accu0__);
	}) ());
};
export var lowermost = function (patch) {
	return max ((function () {
		var __accu0__ = [];
		for (var [i, j] of toindices (patch)) {
			__accu0__.append (i);
		}
		return py_iter (__accu0__);
	}) ());
};
export var leftmost = function (patch) {
	return min ((function () {
		var __accu0__ = [];
		for (var [i, j] of toindices (patch)) {
			__accu0__.append (j);
		}
		return py_iter (__accu0__);
	}) ());
};
export var rightmost = function (patch) {
	return max ((function () {
		var __accu0__ = [];
		for (var [i, j] of toindices (patch)) {
			__accu0__.append (j);
		}
		return py_iter (__accu0__);
	}) ());
};
export var square = function (piece) {
	return (isinstance (piece, tuple) ? len (piece) == len (piece [0]) : height (piece) * width (piece) == len (piece) && height (piece) == width (piece));
};
export var vline = function (patch) {
	return height (patch) == len (patch) && width (patch) == 1;
};
export var hline = function (patch) {
	return width (patch) == len (patch) && height (patch) == 1;
};
export var hmatching = function (a, b) {
	return len (set ((function () {
		var __accu0__ = [];
		for (var [i, j] of toindices (a)) {
			__accu0__.append (i);
		}
		return py_iter (__accu0__);
	}) ()) & set ((function () {
		var __accu0__ = [];
		for (var [i, j] of toindices (b)) {
			__accu0__.append (i);
		}
		return py_iter (__accu0__);
	}) ())) > 0;
};
export var vmatching = function (a, b) {
	return len (set ((function () {
		var __accu0__ = [];
		for (var [i, j] of toindices (a)) {
			__accu0__.append (j);
		}
		return py_iter (__accu0__);
	}) ()) & set ((function () {
		var __accu0__ = [];
		for (var [i, j] of toindices (b)) {
			__accu0__.append (j);
		}
		return py_iter (__accu0__);
	}) ())) > 0;
};
export var manhattan = function (a, b) {
	return min ((function () {
		var __accu0__ = [];
		for (var [ai, aj] of toindices (a)) {
			for (var [bi, bj] of toindices (b)) {
				__accu0__.append (abs (ai - bi) + abs (aj - bj));
			}
		}
		return py_iter (__accu0__);
	}) ());
};
export var adjacent = function (a, b) {
	return manhattan (a, b) == 1;
};
export var bordering = function (patch, grid) {
	return uppermost (patch) == 0 || leftmost (patch) == 0 || lowermost (patch) == len (grid) - 1 || rightmost (patch) == len (grid [0]) - 1;
};
export var centerofmass = function (patch) {
	return tuple (map ((function __lambda__ (x) {
		return Math.floor (sum (x) / len (patch));
	}), zip (...toindices (patch))));
};
export var palette = function (element) {
	if (isinstance (element, tuple)) {
		return frozenset ((function () {
			var __accu0__ = [];
			for (var r of element) {
				for (var v of r) {
					__accu0__.append (v);
				}
			}
			return set (__accu0__);
		}) ());
	}
	return frozenset ((function () {
		var __accu0__ = [];
		for (var [v, _] of element) {
			__accu0__.append (v);
		}
		return set (__accu0__);
	}) ());
};
export var numcolors = function (element) {
	return len (palette (element));
};
export var color = function (obj) {
	return py_next (py_iter (obj)) [0];
};
export var toobject = function (patch, grid) {
	var __left0__ = tuple ([len (grid), len (grid [0])]);
	var h = __left0__ [0];
	var w = __left0__ [1];
	return frozenset ((function () {
		var __accu0__ = [];
		for (var [i, j] of toindices (patch)) {
			if ((0 <= i && i < h) && (0 <= j && j < w)) {
				__accu0__.append (tuple ([grid [i] [j], tuple ([i, j])]));
			}
		}
		return py_iter (__accu0__);
	}) ());
};
export var asobject = function (grid) {
	return frozenset ((function () {
		var __accu0__ = [];
		for (var [i, r] of enumerate (grid)) {
			for (var [j, v] of enumerate (r)) {
				__accu0__.append (tuple ([v, tuple ([i, j])]));
			}
		}
		return py_iter (__accu0__);
	}) ());
};
export var rot90 = function (grid) {
	return tuple ((function () {
		var __accu0__ = [];
		for (var row of zip (...grid.__getslice__ (0, null, -(1)))) {
			__accu0__.append (row);
		}
		return py_iter (__accu0__);
	}) ());
};
export var rot180 = function (grid) {
	return tuple ((function () {
		var __accu0__ = [];
		for (var row of grid.__getslice__ (0, null, -(1))) {
			__accu0__.append (tuple (row.__getslice__ (0, null, -(1))));
		}
		return py_iter (__accu0__);
	}) ());
};
export var rot270 = function (grid) {
	return tuple ((function () {
		var __accu0__ = [];
		for (var row of zip (...grid.__getslice__ (0, null, -(1)))) {
			__accu0__.append (tuple (row.__getslice__ (0, null, -(1))));
		}
		return py_iter (__accu0__);
	}) ()).__getslice__ (0, null, -(1));
};
export var hmirror = function (piece) {
	if (isinstance (piece, tuple)) {
		return piece.__getslice__ (0, null, -(1));
	}
	var d = ulcorner (piece) [0] + lrcorner (piece) [0];
	if (isinstance (py_next (py_iter (piece)) [1], tuple)) {
		return frozenset ((function () {
			var __accu0__ = [];
			for (var [v, [i, j]] of piece) {
				__accu0__.append (tuple ([v, tuple ([d - i, j])]));
			}
			return py_iter (__accu0__);
		}) ());
	}
	return frozenset ((function () {
		var __accu0__ = [];
		for (var [i, j] of piece) {
			__accu0__.append (tuple ([d - i, j]));
		}
		return py_iter (__accu0__);
	}) ());
};
export var vmirror = function (piece) {
	if (isinstance (piece, tuple)) {
		return tuple ((function () {
			var __accu0__ = [];
			for (var row of piece) {
				__accu0__.append (row.__getslice__ (0, null, -(1)));
			}
			return py_iter (__accu0__);
		}) ());
	}
	var d = ulcorner (piece) [1] + lrcorner (piece) [1];
	if (isinstance (py_next (py_iter (piece)) [1], tuple)) {
		return frozenset ((function () {
			var __accu0__ = [];
			for (var [v, [i, j]] of piece) {
				__accu0__.append (tuple ([v, tuple ([i, d - j])]));
			}
			return py_iter (__accu0__);
		}) ());
	}
	return frozenset ((function () {
		var __accu0__ = [];
		for (var [i, j] of piece) {
			__accu0__.append (tuple ([i, d - j]));
		}
		return py_iter (__accu0__);
	}) ());
};
export var dmirror = function (piece) {
	if (isinstance (piece, tuple)) {
		return tuple (zip (...piece));
	}
	var __left0__ = ulcorner (piece);
	var a = __left0__ [0];
	var b = __left0__ [1];
	if (isinstance (py_next (py_iter (piece)) [1], tuple)) {
		return frozenset ((function () {
			var __accu0__ = [];
			for (var [v, [i, j]] of piece) {
				__accu0__.append (tuple ([v, tuple ([(j - b) + a, (i - a) + b])]));
			}
			return py_iter (__accu0__);
		}) ());
	}
	return frozenset ((function () {
		var __accu0__ = [];
		for (var [i, j] of piece) {
			__accu0__.append (tuple ([(j - b) + a, (i - a) + b]));
		}
		return py_iter (__accu0__);
	}) ());
};
export var cmirror = function (piece) {
	if (isinstance (piece, tuple)) {
		return tuple (zip (...(function () {
			var __accu0__ = [];
			for (var r of piece.__getslice__ (0, null, -(1))) {
				__accu0__.append (r.__getslice__ (0, null, -(1)));
			}
			return py_iter (__accu0__);
		}) ()));
	}
	return vmirror (dmirror (vmirror (piece)));
};
export var fill = function (grid, value, patch) {
	var __left0__ = tuple ([len (grid), len (grid [0])]);
	var h = __left0__ [0];
	var w = __left0__ [1];
	var grid_filled = list ((function () {
		var __accu0__ = [];
		for (var row of grid) {
			__accu0__.append (list (row));
		}
		return py_iter (__accu0__);
	}) ());
	for (var [i, j] of toindices (patch)) {
		if ((0 <= i && i < h) && (0 <= j && j < w)) {
			grid_filled [i] [j] = value;
		}
	}
	return tuple ((function () {
		var __accu0__ = [];
		for (var row of grid_filled) {
			__accu0__.append (tuple (row));
		}
		return py_iter (__accu0__);
	}) ());
};
export var paint = function (grid, obj) {
	var __left0__ = tuple ([len (grid), len (grid [0])]);
	var h = __left0__ [0];
	var w = __left0__ [1];
	var grid_painted = list ((function () {
		var __accu0__ = [];
		for (var row of grid) {
			__accu0__.append (list (row));
		}
		return py_iter (__accu0__);
	}) ());
	for (var [value, [i, j]] of obj) {
		if ((0 <= i && i < h) && (0 <= j && j < w)) {
			grid_painted [i] [j] = value;
		}
	}
	return tuple ((function () {
		var __accu0__ = [];
		for (var row of grid_painted) {
			__accu0__.append (tuple (row));
		}
		return py_iter (__accu0__);
	}) ());
};
export var underfill = function (grid, value, patch) {
	var __left0__ = tuple ([len (grid), len (grid [0])]);
	var h = __left0__ [0];
	var w = __left0__ [1];
	var bg = mostcolor (grid);
	var g = list ((function () {
		var __accu0__ = [];
		for (var r of grid) {
			__accu0__.append (list (r));
		}
		return py_iter (__accu0__);
	}) ());
	for (var [i, j] of toindices (patch)) {
		if ((0 <= i && i < h) && (0 <= j && j < w)) {
			if (g [i] [j] == bg) {
				g [i] [j] = value;
			}
		}
	}
	return tuple ((function () {
		var __accu0__ = [];
		for (var r of g) {
			__accu0__.append (tuple (r));
		}
		return py_iter (__accu0__);
	}) ());
};
export var underpaint = function (grid, obj) {
	var __left0__ = tuple ([len (grid), len (grid [0])]);
	var h = __left0__ [0];
	var w = __left0__ [1];
	var bg = mostcolor (grid);
	var g = list ((function () {
		var __accu0__ = [];
		for (var r of grid) {
			__accu0__.append (list (r));
		}
		return py_iter (__accu0__);
	}) ());
	for (var [value, [i, j]] of obj) {
		if ((0 <= i && i < h) && (0 <= j && j < w)) {
			if (g [i] [j] == bg) {
				g [i] [j] = value;
			}
		}
	}
	return tuple ((function () {
		var __accu0__ = [];
		for (var r of g) {
			__accu0__.append (tuple (r));
		}
		return py_iter (__accu0__);
	}) ());
};
export var hupscale = function (grid, factor) {
	var g = tuple ();
	for (var row of grid) {
		var r = tuple ();
		for (var value of row) {
			var r = r + tuple ((function () {
				var __accu0__ = [];
				for (var num = 0; num < factor; num++) {
					__accu0__.append (value);
				}
				return py_iter (__accu0__);
			}) ());
		}
		var g = g + tuple ([r]);
	}
	return g;
};
export var vupscale = function (grid, factor) {
	var g = tuple ();
	for (var row of grid) {
		var g = g + tuple ((function () {
			var __accu0__ = [];
			for (var num = 0; num < factor; num++) {
				__accu0__.append (row);
			}
			return py_iter (__accu0__);
		}) ());
	}
	return g;
};
export var upscale = function (element, factor) {
	if (isinstance (element, tuple)) {
		var g = tuple ();
		for (var row of element) {
			var upscaled_row = tuple ();
			for (var value of row) {
				var upscaled_row = upscaled_row + tuple ((function () {
					var __accu0__ = [];
					for (var num = 0; num < factor; num++) {
						__accu0__.append (value);
					}
					return py_iter (__accu0__);
				}) ());
			}
			var g = g + tuple ((function () {
				var __accu0__ = [];
				for (var num = 0; num < factor; num++) {
					__accu0__.append (upscaled_row);
				}
				return py_iter (__accu0__);
			}) ());
		}
		return g;
	}
	else {
		if (len (element) == 0) {
			return frozenset ();
		}
		var __left0__ = ulcorner (element);
		var di_inv = __left0__ [0];
		var dj_inv = __left0__ [1];
		var __left0__ = tuple ([-(di_inv), -(dj_inv)]);
		var di = __left0__ [0];
		var dj = __left0__ [1];
		var normed_obj = shift (element, tuple ([di, dj]));
		var o = set ();
		for (var [value, [i, j]] of normed_obj) {
			for (var io = 0; io < factor; io++) {
				for (var jo = 0; jo < factor; jo++) {
					o.add (tuple ([value, tuple ([i * factor + io, j * factor + jo])]));
				}
			}
		}
		return shift (frozenset (o), tuple ([di_inv, dj_inv]));
	}
};
export var downscale = function (grid, factor) {
	var __left0__ = tuple ([len (grid), len (grid [0])]);
	var h = __left0__ [0];
	var w = __left0__ [1];
	var g = tuple ();
	for (var i = 0; i < h; i++) {
		var r = tuple ();
		for (var j = 0; j < w; j++) {
			if (__mod__ (j, factor) == 0) {
				var r = r + tuple ([grid [i] [j]]);
			}
		}
		var g = g + tuple ([r]);
	}
	var h = len (g);
	var dsg = tuple ();
	for (var i = 0; i < h; i++) {
		if (__mod__ (i, factor) == 0) {
			var dsg = dsg + tuple ([g [i]]);
		}
	}
	return dsg;
};
export var hconcat = function (a, b) {
	return tuple ((function () {
		var __accu0__ = [];
		for (var [i, j] of zip (a, b)) {
			__accu0__.append (i + j);
		}
		return py_iter (__accu0__);
	}) ());
};
export var vconcat = function (a, b) {
	return a + b;
};
export var subgrid = function (patch, grid) {
	return crop (grid, ulcorner (patch), shape (patch));
};
export var hsplit = function (grid, n) {
	var __left0__ = tuple ([len (grid), Math.floor (len (grid [0]) / n)]);
	var h = __left0__ [0];
	var w = __left0__ [1];
	var offset = __mod__ (len (grid [0]), n) != 0;
	return tuple ((function () {
		var __accu0__ = [];
		for (var i = 0; i < n; i++) {
			__accu0__.append (crop (grid, tuple ([0, w * i + i * offset]), tuple ([h, w])));
		}
		return py_iter (__accu0__);
	}) ());
};
export var vsplit = function (grid, n) {
	var __left0__ = tuple ([Math.floor (len (grid) / n), len (grid [0])]);
	var h = __left0__ [0];
	var w = __left0__ [1];
	var offset = __mod__ (len (grid), n) != 0;
	return tuple ((function () {
		var __accu0__ = [];
		for (var i = 0; i < n; i++) {
			__accu0__.append (crop (grid, tuple ([h * i + i * offset, 0]), tuple ([h, w])));
		}
		return py_iter (__accu0__);
	}) ());
};
export var cellwise = function (a, b, fallback) {
	var __left0__ = tuple ([len (a), len (a [0])]);
	var h = __left0__ [0];
	var w = __left0__ [1];
	var resulting_grid = tuple ();
	for (var i = 0; i < h; i++) {
		var row = tuple ();
		for (var j = 0; j < w; j++) {
			var a_value = a [i] [j];
			var value = (a_value == b [i] [j] ? a_value : fallback);
			var row = row + tuple ([value]);
		}
		var resulting_grid = resulting_grid + tuple ([row]);
	}
	return resulting_grid;
};
export var py_replace = function (grid, replacee, replacer) {
	return tuple ((function () {
		var __accu0__ = [];
		for (var r of grid) {
			__accu0__.append (tuple ((function () {
				var __accu1__ = [];
				for (var v of r) {
					__accu1__.append ((v == replacee ? replacer : v));
				}
				return py_iter (__accu1__);
			}) ()));
		}
		return py_iter (__accu0__);
	}) ());
};
export var py_switch = function (grid, a, b) {
	return tuple ((function () {
		var __accu0__ = [];
		for (var r of grid) {
			__accu0__.append (tuple ((function () {
				var __accu1__ = [];
				for (var v of r) {
					__accu1__.append ((v != a && v != b ? v : dict ([[a, b], [b, a]]) [v]));
				}
				return py_iter (__accu1__);
			}) ()));
		}
		return py_iter (__accu0__);
	}) ());
};
export var center = function (patch) {
	return tuple ([uppermost (patch) + Math.floor (height (patch) / 2), leftmost (patch) + Math.floor (width (patch) / 2)]);
};
export var position = function (a, b) {
	var __left0__ = center (toindices (a));
	var ia = __left0__ [0];
	var ja = __left0__ [1];
	var __left0__ = center (toindices (b));
	var ib = __left0__ [0];
	var jb = __left0__ [1];
	if (ia == ib) {
		return tuple ([0, (ja < jb ? 1 : -(1))]);
	}
	else if (ja == jb) {
		return tuple ([(ia < ib ? 1 : -(1)), 0]);
	}
	else if (ia < ib) {
		return tuple ([1, (ja < jb ? 1 : -(1))]);
	}
	else if (ia > ib) {
		return tuple ([-(1), (ja < jb ? 1 : -(1))]);
	}
};
export var index = function (grid, loc) {
	var __left0__ = loc;
	var i = __left0__ [0];
	var j = __left0__ [1];
	var __left0__ = tuple ([len (grid), len (grid [0])]);
	var h = __left0__ [0];
	var w = __left0__ [1];
	if (!((0 <= i && i < h) && (0 <= j && j < w))) {
		return null;
	}
	return grid [loc [0]] [loc [1]];
};
export var canvas = function (value, dimensions) {
	return tuple ((function () {
		var __accu0__ = [];
		for (var i = 0; i < dimensions [0]; i++) {
			__accu0__.append (tuple ((function () {
				var __accu1__ = [];
				for (var j = 0; j < dimensions [1]; j++) {
					__accu1__.append (value);
				}
				return py_iter (__accu1__);
			}) ()));
		}
		return py_iter (__accu0__);
	}) ());
};
export var corners = function (patch) {
	return frozenset (new set ([ulcorner (patch), urcorner (patch), llcorner (patch), lrcorner (patch)]));
};
export var connect = function (a, b) {
	var __left0__ = a;
	var ai = __left0__ [0];
	var aj = __left0__ [1];
	var __left0__ = b;
	var bi = __left0__ [0];
	var bj = __left0__ [1];
	var si = min (ai, bi);
	var ei = max (ai, bi) + 1;
	var sj = min (aj, bj);
	var ej = max (aj, bj) + 1;
	if (ai == bi) {
		return frozenset ((function () {
			var __accu0__ = [];
			for (var j = sj; j < ej; j++) {
				__accu0__.append (tuple ([ai, j]));
			}
			return py_iter (__accu0__);
		}) ());
	}
	else if (aj == bj) {
		return frozenset ((function () {
			var __accu0__ = [];
			for (var i = si; i < ei; i++) {
				__accu0__.append (tuple ([i, aj]));
			}
			return py_iter (__accu0__);
		}) ());
	}
	else if (bi - ai == bj - aj) {
		return frozenset ((function () {
			var __accu0__ = [];
			for (var [i, j] of zip (range (si, ei), range (sj, ej))) {
				__accu0__.append (tuple ([i, j]));
			}
			return py_iter (__accu0__);
		}) ());
	}
	else if (bi - ai == aj - bj) {
		return frozenset ((function () {
			var __accu0__ = [];
			for (var [i, j] of zip (range (si, ei), range (ej - 1, sj - 1, -(1)))) {
				__accu0__.append (tuple ([i, j]));
			}
			return py_iter (__accu0__);
		}) ());
	}
	return frozenset ();
};
export var cover = function (grid, patch) {
	return fill (grid, mostcolor (grid), toindices (patch));
};
export var trim = function (grid) {
	return tuple ((function () {
		var __accu0__ = [];
		for (var r of grid.__getslice__ (1, -(1), 1)) {
			__accu0__.append (r.__getslice__ (1, -(1), 1));
		}
		return py_iter (__accu0__);
	}) ());
};
export var move = function (grid, obj, offset) {
	return paint (cover (grid, obj), shift (obj, offset));
};
export var tophalf = function (grid) {
	return grid.__getslice__ (0, Math.floor (len (grid) / 2), 1);
};
export var bottomhalf = function (grid) {
	return grid.__getslice__ (Math.floor (len (grid) / 2) + __mod__ (len (grid), 2), null, 1);
};
export var lefthalf = function (grid) {
	return rot270 (tophalf (rot90 (grid)));
};
export var righthalf = function (grid) {
	return rot270 (bottomhalf (rot90 (grid)));
};
export var vfrontier = function (location) {
	return frozenset ((function () {
		var __accu0__ = [];
		for (var i = 0; i < 30; i++) {
			__accu0__.append (tuple ([i, location [1]]));
		}
		return py_iter (__accu0__);
	}) ());
};
export var hfrontier = function (location) {
	return frozenset ((function () {
		var __accu0__ = [];
		for (var j = 0; j < 30; j++) {
			__accu0__.append (tuple ([location [0], j]));
		}
		return py_iter (__accu0__);
	}) ());
};
export var backdrop = function (patch) {
	var indices = toindices (patch);
	var __left0__ = ulcorner (indices);
	var si = __left0__ [0];
	var sj = __left0__ [1];
	var __left0__ = lrcorner (patch);
	var ei = __left0__ [0];
	var ej = __left0__ [1];
	return frozenset ((function () {
		var __accu0__ = [];
		for (var i = si; i < ei + 1; i++) {
			for (var j = sj; j < ej + 1; j++) {
				__accu0__.append (tuple ([i, j]));
			}
		}
		return py_iter (__accu0__);
	}) ());
};
export var delta = function (patch) {
	return backdrop (patch) - toindices (patch);
};
export var gravitate = function (source, destination) {
	var __left0__ = center (source);
	var si = __left0__ [0];
	var sj = __left0__ [1];
	var __left0__ = center (destination);
	var di = __left0__ [0];
	var dj = __left0__ [1];
	var __left0__ = tuple ([0, 0]);
	var i = __left0__ [0];
	var j = __left0__ [1];
	if (vmatching (source, destination)) {
		var i = (si < di ? 1 : -(1));
	}
	else {
		var j = (sj < dj ? 1 : -(1));
	}
	var __left0__ = tuple ([i, j]);
	var gi = __left0__ [0];
	var gj = __left0__ [1];
	var c = 0;
	while (!(adjacent (source, destination)) && c < 42) {
		c++;
		gi += i;
		gj += j;
		var source = shift (source, tuple ([i, j]));
	}
	return tuple ([gi - i, gj - j]);
};
export var inbox = function (patch) {
	var __left0__ = tuple ([uppermost (patch) + 1, leftmost (patch) + 1]);
	var ai = __left0__ [0];
	var aj = __left0__ [1];
	var __left0__ = tuple ([lowermost (patch) - 1, rightmost (patch) - 1]);
	var bi = __left0__ [0];
	var bj = __left0__ [1];
	var __left0__ = tuple ([min (ai, bi), min (aj, bj)]);
	var si = __left0__ [0];
	var sj = __left0__ [1];
	var __left0__ = tuple ([max (ai, bi), max (aj, bj)]);
	var ei = __left0__ [0];
	var ej = __left0__ [1];
	var vlines = (function () {
		var __accu0__ = [];
		for (var i = si; i < ei + 1; i++) {
			__accu0__.append (tuple ([i, sj]));
		}
		return set (__accu0__);
	}) () | (function () {
		var __accu0__ = [];
		for (var i = si; i < ei + 1; i++) {
			__accu0__.append (tuple ([i, ej]));
		}
		return set (__accu0__);
	}) ();
	var hlines = (function () {
		var __accu0__ = [];
		for (var j = sj; j < ej + 1; j++) {
			__accu0__.append (tuple ([si, j]));
		}
		return set (__accu0__);
	}) () | (function () {
		var __accu0__ = [];
		for (var j = sj; j < ej + 1; j++) {
			__accu0__.append (tuple ([ei, j]));
		}
		return set (__accu0__);
	}) ();
	return frozenset (vlines | hlines);
};
export var outbox = function (patch) {
	var __left0__ = tuple ([uppermost (patch) - 1, leftmost (patch) - 1]);
	var ai = __left0__ [0];
	var aj = __left0__ [1];
	var __left0__ = tuple ([lowermost (patch) + 1, rightmost (patch) + 1]);
	var bi = __left0__ [0];
	var bj = __left0__ [1];
	var __left0__ = tuple ([min (ai, bi), min (aj, bj)]);
	var si = __left0__ [0];
	var sj = __left0__ [1];
	var __left0__ = tuple ([max (ai, bi), max (aj, bj)]);
	var ei = __left0__ [0];
	var ej = __left0__ [1];
	var vlines = (function () {
		var __accu0__ = [];
		for (var i = si; i < ei + 1; i++) {
			__accu0__.append (tuple ([i, sj]));
		}
		return set (__accu0__);
	}) () | (function () {
		var __accu0__ = [];
		for (var i = si; i < ei + 1; i++) {
			__accu0__.append (tuple ([i, ej]));
		}
		return set (__accu0__);
	}) ();
	var hlines = (function () {
		var __accu0__ = [];
		for (var j = sj; j < ej + 1; j++) {
			__accu0__.append (tuple ([si, j]));
		}
		return set (__accu0__);
	}) () | (function () {
		var __accu0__ = [];
		for (var j = sj; j < ej + 1; j++) {
			__accu0__.append (tuple ([ei, j]));
		}
		return set (__accu0__);
	}) ();
	return frozenset (vlines | hlines);
};
export var box = function (patch) {
	var __left0__ = ulcorner (patch);
	var ai = __left0__ [0];
	var aj = __left0__ [1];
	var __left0__ = lrcorner (patch);
	var bi = __left0__ [0];
	var bj = __left0__ [1];
	var __left0__ = tuple ([min (ai, bi), min (aj, bj)]);
	var si = __left0__ [0];
	var sj = __left0__ [1];
	var __left0__ = tuple ([max (ai, bi), max (aj, bj)]);
	var ei = __left0__ [0];
	var ej = __left0__ [1];
	var vlines = (function () {
		var __accu0__ = [];
		for (var i = si; i < ei + 1; i++) {
			__accu0__.append (tuple ([i, sj]));
		}
		return set (__accu0__);
	}) () | (function () {
		var __accu0__ = [];
		for (var i = si; i < ei + 1; i++) {
			__accu0__.append (tuple ([i, ej]));
		}
		return set (__accu0__);
	}) ();
	var hlines = (function () {
		var __accu0__ = [];
		for (var j = sj; j < ej + 1; j++) {
			__accu0__.append (tuple ([si, j]));
		}
		return set (__accu0__);
	}) () | (function () {
		var __accu0__ = [];
		for (var j = sj; j < ej + 1; j++) {
			__accu0__.append (tuple ([ei, j]));
		}
		return set (__accu0__);
	}) ();
	return frozenset (vlines | hlines);
};
export var shoot = function (start, direction) {
	return connect (start, tuple ([start [0] + 42 * direction [0], start [1] + 42 * direction [1]]));
};
export var occurrences = function (grid, obj) {
	var occs = set ();
	var normed = normalize (obj);
	var __left0__ = tuple ([len (grid), len (grid [0])]);
	var h = __left0__ [0];
	var w = __left0__ [1];
	var __left0__ = shape (obj);
	var oh = __left0__ [0];
	var ow = __left0__ [1];
	var __left0__ = tuple ([(h - oh) + 1, (w - ow) + 1]);
	var h2 = __left0__ [0];
	var w2 = __left0__ [1];
	for (var i = 0; i < h2; i++) {
		for (var j = 0; j < w2; j++) {
			var occurs = true;
			for (var [v, [a, b]] of shift (normed, tuple ([i, j]))) {
				if (!((0 <= a && a < h) && (0 <= b && b < w) && grid [a] [b] == v)) {
					var occurs = false;
					break;
				}
			}
			if (occurs) {
				occs.add (tuple ([i, j]));
			}
		}
	}
	return frozenset (occs);
};
export var frontiers = function (grid) {
	var __left0__ = tuple ([len (grid), len (grid [0])]);
	var h = __left0__ [0];
	var w = __left0__ [1];
	var row_indices = tuple ((function () {
		var __accu0__ = [];
		for (var [i, r] of enumerate (grid)) {
			if (len (set (r)) == 1) {
				__accu0__.append (i);
			}
		}
		return py_iter (__accu0__);
	}) ());
	var column_indices = tuple ((function () {
		var __accu0__ = [];
		for (var [j, c] of enumerate (dmirror (grid))) {
			if (len (set (c)) == 1) {
				__accu0__.append (j);
			}
		}
		return py_iter (__accu0__);
	}) ());
	var hfrontiers = frozenset ((function () {
		var __accu0__ = [];
		for (var i of row_indices) {
			__accu0__.append (frozenset ((function () {
				var __accu1__ = [];
				for (var j = 0; j < w; j++) {
					__accu1__.append (tuple ([grid [i] [j], tuple ([i, j])]));
				}
				return set (__accu1__);
			}) ()));
		}
		return set (__accu0__);
	}) ());
	var vfrontiers = frozenset ((function () {
		var __accu0__ = [];
		for (var j of column_indices) {
			__accu0__.append (frozenset ((function () {
				var __accu1__ = [];
				for (var i = 0; i < h; i++) {
					__accu1__.append (tuple ([grid [i] [j], tuple ([i, j])]));
				}
				return set (__accu1__);
			}) ()));
		}
		return set (__accu0__);
	}) ());
	return hfrontiers | vfrontiers;
};
export var compress = function (grid) {
	var ri = tuple ((function () {
		var __accu0__ = [];
		for (var [i, r] of enumerate (grid)) {
			if (len (set (r)) == 1) {
				__accu0__.append (i);
			}
		}
		return py_iter (__accu0__);
	}) ());
	var ci = tuple ((function () {
		var __accu0__ = [];
		for (var [j, c] of enumerate (dmirror (grid))) {
			if (len (set (c)) == 1) {
				__accu0__.append (j);
			}
		}
		return py_iter (__accu0__);
	}) ());
	return tuple ((function () {
		var __accu0__ = [];
		for (var [i, r] of enumerate (grid)) {
			if (!__in__ (i, ri)) {
				__accu0__.append (tuple ((function () {
					var __accu1__ = [];
					for (var [j, v] of enumerate (r)) {
						if (!__in__ (j, ci)) {
							__accu1__.append (v);
						}
					}
					return py_iter (__accu1__);
				}) ()));
			}
		}
		return py_iter (__accu0__);
	}) ());
};
export var hperiod = function (obj) {
	var normalized = normalize (obj);
	var w = width (normalized);
	for (var p = 1; p < w; p++) {
		var offsetted = shift (normalized, tuple ([0, -(p)]));
		var pruned = frozenset ((function () {
			var __accu0__ = [];
			for (var [c, [i, j]] of offsetted) {
				if (j >= 0) {
					__accu0__.append (tuple ([c, tuple ([i, j])]));
				}
			}
			return set (__accu0__);
		}) ());
		if (pruned.issubset (normalized)) {
			return p;
		}
	}
	return w;
};
export var vperiod = function (obj) {
	var normalized = normalize (obj);
	var h = height (normalized);
	for (var p = 1; p < h; p++) {
		var offsetted = shift (normalized, tuple ([-(p), 0]));
		var pruned = frozenset ((function () {
			var __accu0__ = [];
			for (var [c, [i, j]] of offsetted) {
				if (i >= 0) {
					__accu0__.append (tuple ([c, tuple ([i, j])]));
				}
			}
			return set (__accu0__);
		}) ());
		if (pruned.issubset (normalized)) {
			return p;
		}
	}
	return h;
};
export var solve_67a3c6ac = function (I) {
	var O = vmirror (I);
	return O;
};
export var solve_68b16354 = function (I) {
	var O = hmirror (I);
	return O;
};
export var solve_74dd1130 = function (I) {
	var O = dmirror (I);
	return O;
};
export var solve_3c9b0459 = function (I) {
	var O = rot180 (I);
	return O;
};
export var solve_6150a2bd = function (I) {
	var O = rot180 (I);
	return O;
};
export var solve_9172f3a0 = function (I) {
	var O = upscale (I, THREE);
	return O;
};
export var solve_9dfd6313 = function (I) {
	var O = dmirror (I);
	return O;
};
export var solve_a416b8f3 = function (I) {
	var O = hconcat (I, I);
	return O;
};
export var solve_b1948b0a = function (I) {
	var O = py_replace (I, SIX, TWO);
	return O;
};
export var solve_c59eb873 = function (I) {
	var O = upscale (I, TWO);
	return O;
};
export var solve_c8f0f002 = function (I) {
	var O = py_replace (I, SEVEN, FIVE);
	return O;
};
export var solve_d10ecb37 = function (I) {
	var O = crop (I, ORIGIN, TWO_BY_TWO);
	return O;
};
export var solve_d511f180 = function (I) {
	var O = py_switch (I, FIVE, EIGHT);
	return O;
};
export var solve_ed36ccf7 = function (I) {
	var O = rot270 (I);
	return O;
};
export var solve_4c4377d9 = function (I) {
	var x1 = hmirror (I);
	var O = vconcat (x1, I);
	return O;
};
export var solve_6d0aefbc = function (I) {
	var x1 = vmirror (I);
	var O = hconcat (I, x1);
	return O;
};
export var solve_6fa7a44f = function (I) {
	var x1 = hmirror (I);
	var O = vconcat (I, x1);
	return O;
};
export var solve_5614dbcf = function (I) {
	var x1 = py_replace (I, FIVE, ZERO);
	var O = downscale (x1, THREE);
	return O;
};
export var solve_5bd6f4ac = function (I) {
	var x1 = tojvec (SIX);
	var O = crop (I, x1, THREE_BY_THREE);
	return O;
};
export var solve_5582e5ca = function (I) {
	var x1 = mostcolor (I);
	var O = canvas (x1, THREE_BY_THREE);
	return O;
};
export var solve_8be77c9e = function (I) {
	var x1 = hmirror (I);
	var O = vconcat (I, x1);
	return O;
};
export var solve_c9e6f938 = function (I) {
	var x1 = vmirror (I);
	var O = hconcat (I, x1);
	return O;
};
export var solve_2dee498d = function (I) {
	var x1 = hsplit (I, THREE);
	var O = first (x1);
	return O;
};
export var solve_1cf80156 = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = first (x1);
	var O = subgrid (x2, I);
	return O;
};
export var solve_32597951 = function (I) {
	var x1 = ofcolor (I, EIGHT);
	var x2 = delta (x1);
	var O = fill (I, THREE, x2);
	return O;
};
export var solve_25ff71a9 = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = first (x1);
	var O = move (I, x2, DOWN);
	return O;
};
export var solve_0b148d64 = function (I) {
	var x1 = partition (I);
	var x2 = argmin (x1, size);
	var O = subgrid (x2, I);
	return O;
};
export var solve_1f85a75f = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = argmax (x1, size);
	var O = subgrid (x2, I);
	return O;
};
export var solve_23b5c85d = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = argmin (x1, size);
	var O = subgrid (x2, I);
	return O;
};
export var solve_9ecd008a = function (I) {
	var x1 = vmirror (I);
	var x2 = ofcolor (I, ZERO);
	var O = subgrid (x2, x1);
	return O;
};
export var solve_ac0a08a4 = function (I) {
	var x1 = colorcount (I, ZERO);
	var x2 = subtract (NINE, x1);
	var O = upscale (I, x2);
	return O;
};
export var solve_be94b721 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = argmax (x1, size);
	var O = subgrid (x2, I);
	return O;
};
export var solve_c909285e = function (I) {
	var x1 = leastcolor (I);
	var x2 = ofcolor (I, x1);
	var O = subgrid (x2, I);
	return O;
};
export var solve_f25ffba3 = function (I) {
	var x1 = bottomhalf (I);
	var x2 = hmirror (x1);
	var O = vconcat (x2, x1);
	return O;
};
export var solve_c1d99e64 = function (I) {
	var x1 = frontiers (I);
	var x2 = merge (x1);
	var O = fill (I, TWO, x2);
	return O;
};
export var solve_b91ae062 = function (I) {
	var x1 = numcolors (I);
	var x2 = decrement (x1);
	var O = upscale (I, x2);
	return O;
};
export var solve_3aa6fb7a = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = mapply (corners, x1);
	var O = underfill (I, ONE, x2);
	return O;
};
export var solve_7b7f7511 = function (I) {
	var x1 = portrait (I);
	var x2 = branch (x1, tophalf, lefthalf);
	var O = x2 (I);
	return O;
};
export var solve_4258a5f9 = function (I) {
	var x1 = ofcolor (I, FIVE);
	var x2 = mapply (neighbors, x1);
	var O = fill (I, ONE, x2);
	return O;
};
export var solve_2dc579da = function (I) {
	var x1 = vsplit (I, TWO);
	var x2 = rbind (hsplit, TWO);
	var x3 = mapply (x2, x1);
	var O = argmax (x3, numcolors);
	return O;
};
export var solve_28bf18c6 = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = first (x1);
	var x3 = subgrid (x2, I);
	var O = hconcat (x3, x3);
	return O;
};
export var solve_3af2c5a8 = function (I) {
	var x1 = vmirror (I);
	var x2 = hconcat (I, x1);
	var x3 = hmirror (x2);
	var O = vconcat (x2, x3);
	return O;
};
export var solve_44f52bb0 = function (I) {
	var x1 = vmirror (I);
	var x2 = equality (x1, I);
	var x3 = branch (x2, ONE, SEVEN);
	var O = canvas (x3, UNITY);
	return O;
};
export var solve_62c24649 = function (I) {
	var x1 = vmirror (I);
	var x2 = hconcat (I, x1);
	var x3 = hmirror (x2);
	var O = vconcat (x2, x3);
	return O;
};
export var solve_67e8384a = function (I) {
	var x1 = vmirror (I);
	var x2 = hconcat (I, x1);
	var x3 = hmirror (x2);
	var O = vconcat (x2, x3);
	return O;
};
export var solve_7468f01a = function (I) {
	var x1 = objects (I, F, T, T);
	var x2 = first (x1);
	var x3 = subgrid (x2, I);
	var O = vmirror (x3);
	return O;
};
export var solve_662c240a = function (I) {
	var x1 = vsplit (I, THREE);
	var x2 = fork (equality, dmirror, identity);
	var x3 = compose (flip, x2);
	var O = extract (x1, x3);
	return O;
};
export var solve_42a50994 = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = sizefilter (x1, ONE);
	var x3 = merge (x2);
	var O = cover (I, x3);
	return O;
};
export var solve_56ff96f3 = function (I) {
	var x1 = fgpartition (I);
	var x2 = fork (recolor, color, backdrop);
	var x3 = mapply (x2, x1);
	var O = paint (I, x3);
	return O;
};
export var solve_50cb2852 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = compose (backdrop, inbox);
	var x3 = mapply (x2, x1);
	var O = fill (I, EIGHT, x3);
	return O;
};
export var solve_4347f46a = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = fork (difference, toindices, box);
	var x3 = mapply (x2, x1);
	var O = fill (I, ZERO, x3);
	return O;
};
export var solve_46f33fce = function (I) {
	var x1 = rot180 (I);
	var x2 = downscale (x1, TWO);
	var x3 = rot180 (x2);
	var O = upscale (x3, FOUR);
	return O;
};
export var solve_a740d043 = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = merge (x1);
	var x3 = subgrid (x2, I);
	var O = py_replace (x3, ONE, ZERO);
	return O;
};
export var solve_a79310a0 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = first (x1);
	var x3 = move (I, x2, DOWN);
	var O = py_replace (x3, EIGHT, TWO);
	return O;
};
export var solve_aabf363d = function (I) {
	var x1 = leastcolor (I);
	var x2 = py_replace (I, x1, ZERO);
	var x3 = leastcolor (x2);
	var O = py_replace (x2, x3, x1);
	return O;
};
export var solve_ae4f1146 = function (I) {
	var x1 = objects (I, F, F, T);
	var x2 = rbind (colorcount, ONE);
	var x3 = argmax (x1, x2);
	var O = subgrid (x3, I);
	return O;
};
export var solve_b27ca6d3 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = sizefilter (x1, TWO);
	var x3 = mapply (outbox, x2);
	var O = fill (I, THREE, x3);
	return O;
};
export var solve_ce22a75a = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = apply (outbox, x1);
	var x3 = mapply (backdrop, x2);
	var O = fill (I, ONE, x3);
	return O;
};
export var solve_dc1df850 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = colorfilter (x1, TWO);
	var x3 = mapply (outbox, x2);
	var O = fill (I, ONE, x3);
	return O;
};
export var solve_f25fbde4 = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = first (x1);
	var x3 = subgrid (x2, I);
	var O = upscale (x3, TWO);
	return O;
};
export var solve_44d8ac46 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = apply (delta, x1);
	var x3 = mfilter (x2, square);
	var O = fill (I, TWO, x3);
	return O;
};
export var solve_1e0a9b12 = function (I) {
	var x1 = rot270 (I);
	var x2 = rbind (order, identity);
	var x3 = apply (x2, x1);
	var O = rot90 (x3);
	return O;
};
export var solve_0d3d703e = function (I) {
	var x1 = py_switch (I, THREE, FOUR);
	var x2 = py_switch (x1, EIGHT, NINE);
	var x3 = py_switch (x2, TWO, SIX);
	var O = py_switch (x3, ONE, FIVE);
	return O;
};
export var solve_3618c87e = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = sizefilter (x1, ONE);
	var x3 = merge (x2);
	var O = move (I, x3, TWO_BY_ZERO);
	return O;
};
export var solve_1c786137 = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = argmax (x1, height);
	var x3 = subgrid (x2, I);
	var O = trim (x3);
	return O;
};
export var solve_8efcae92 = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = colorfilter (x1, ONE);
	var x3 = compose (size, delta);
	var x4 = argmax (x2, x3);
	var O = subgrid (x4, I);
	return O;
};
export var solve_445eab21 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = fork (multiply, height, width);
	var x3 = argmax (x1, x2);
	var x4 = color (x3);
	var O = canvas (x4, TWO_BY_TWO);
	return O;
};
export var solve_6f8cd79b = function (I) {
	var x1 = asindices (I);
	var x2 = apply (initset, x1);
	var x3 = rbind (bordering, I);
	var x4 = mfilter (x2, x3);
	var O = fill (I, EIGHT, x4);
	return O;
};
export var solve_2013d3e2 = function (I) {
	var x1 = objects (I, F, T, T);
	var x2 = first (x1);
	var x3 = subgrid (x2, I);
	var x4 = lefthalf (x3);
	var O = tophalf (x4);
	return O;
};
export var solve_41e4d17e = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = fork (combine, vfrontier, hfrontier);
	var x3 = compose (x2, center);
	var x4 = mapply (x3, x1);
	var O = underfill (I, SIX, x4);
	return O;
};
export var solve_9565186b = function (I) {
	var x1 = shape (I);
	var x2 = objects (I, T, F, F);
	var x3 = argmax (x2, size);
	var x4 = canvas (FIVE, x1);
	var O = paint (x4, x3);
	return O;
};
export var solve_aedd82e4 = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = colorfilter (x1, TWO);
	var x3 = sizefilter (x2, ONE);
	var x4 = merge (x3);
	var O = fill (I, ONE, x4);
	return O;
};
export var solve_bb43febb = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = colorfilter (x1, FIVE);
	var x3 = compose (backdrop, inbox);
	var x4 = mapply (x3, x2);
	var O = fill (I, TWO, x4);
	return O;
};
export var solve_e98196ab = function (I) {
	var x1 = tophalf (I);
	var x2 = bottomhalf (I);
	var x3 = objects (x1, T, F, T);
	var x4 = merge (x3);
	var O = paint (x2, x4);
	return O;
};
export var solve_f76d97a5 = function (I) {
	var x1 = palette (I);
	var x2 = first (x1);
	var x3 = last (x1);
	var x4 = py_switch (I, x2, x3);
	var O = py_replace (x4, FIVE, ZERO);
	return O;
};
export var solve_ce9e57f2 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = fork (connect, ulcorner, centerofmass);
	var x3 = mapply (x2, x1);
	var x4 = fill (I, EIGHT, x3);
	var O = py_switch (x4, EIGHT, TWO);
	return O;
};
export var solve_22eb0ac0 = function (I) {
	var x1 = fgpartition (I);
	var x2 = fork (recolor, color, backdrop);
	var x3 = apply (x2, x1);
	var x4 = mfilter (x3, hline);
	var O = paint (I, x4);
	return O;
};
export var solve_9f236235 = function (I) {
	var x1 = compress (I);
	var x2 = objects (I, T, F, F);
	var x3 = vmirror (x1);
	var x4 = valmin (x2, width);
	var O = downscale (x3, x4);
	return O;
};
export var solve_a699fb00 = function (I) {
	var x1 = ofcolor (I, ONE);
	var x2 = shift (x1, RIGHT);
	var x3 = shift (x1, LEFT);
	var x4 = intersection (x2, x3);
	var O = fill (I, TWO, x4);
	return O;
};
export var solve_46442a0e = function (I) {
	var x1 = rot90 (I);
	var x2 = rot180 (I);
	var x3 = rot270 (I);
	var x4 = hconcat (I, x1);
	var x5 = hconcat (x3, x2);
	var O = vconcat (x4, x5);
	return O;
};
export var solve_7fe24cdd = function (I) {
	var x1 = rot90 (I);
	var x2 = rot180 (I);
	var x3 = rot270 (I);
	var x4 = hconcat (I, x1);
	var x5 = hconcat (x3, x2);
	var O = vconcat (x4, x5);
	return O;
};
export var solve_0ca9ddb6 = function (I) {
	var x1 = ofcolor (I, ONE);
	var x2 = ofcolor (I, TWO);
	var x3 = mapply (dneighbors, x1);
	var x4 = mapply (ineighbors, x2);
	var x5 = fill (I, SEVEN, x3);
	var O = fill (x5, FOUR, x4);
	return O;
};
export var solve_543a7ed5 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = colorfilter (x1, SIX);
	var x3 = mapply (outbox, x2);
	var x4 = fill (I, THREE, x3);
	var x5 = mapply (delta, x2);
	var O = fill (x4, FOUR, x5);
	return O;
};
export var solve_0520fde7 = function (I) {
	var x1 = vmirror (I);
	var x2 = lefthalf (x1);
	var x3 = righthalf (x1);
	var x4 = vmirror (x3);
	var x5 = cellwise (x2, x4, ZERO);
	var O = py_replace (x5, ONE, TWO);
	return O;
};
export var solve_dae9d2b5 = function (I) {
	var x1 = lefthalf (I);
	var x2 = righthalf (I);
	var x3 = ofcolor (x1, FOUR);
	var x4 = ofcolor (x2, THREE);
	var x5 = combine (x3, x4);
	var O = fill (x1, SIX, x5);
	return O;
};
export var solve_8d5021e8 = function (I) {
	var x1 = vmirror (I);
	var x2 = hconcat (x1, I);
	var x3 = hmirror (x2);
	var x4 = vconcat (x2, x3);
	var x5 = vconcat (x4, x2);
	var O = hmirror (x5);
	return O;
};
export var solve_928ad970 = function (I) {
	var x1 = ofcolor (I, FIVE);
	var x2 = subgrid (x1, I);
	var x3 = trim (x2);
	var x4 = leastcolor (x3);
	var x5 = inbox (x1);
	var O = fill (I, x4, x5);
	return O;
};
export var solve_b60334d2 = function (I) {
	var x1 = ofcolor (I, FIVE);
	var x2 = py_replace (I, FIVE, ZERO);
	var x3 = mapply (dneighbors, x1);
	var x4 = mapply (ineighbors, x1);
	var x5 = fill (x2, ONE, x3);
	var O = fill (x5, FIVE, x4);
	return O;
};
export var solve_b94a9452 = function (I) {
	var x1 = objects (I, F, F, T);
	var x2 = first (x1);
	var x3 = subgrid (x2, I);
	var x4 = leastcolor (x3);
	var x5 = mostcolor (x3);
	var O = py_switch (x3, x4, x5);
	return O;
};
export var solve_d037b0a7 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = rbind (shoot, DOWN);
	var x3 = compose (x2, center);
	var x4 = fork (recolor, color, x3);
	var x5 = mapply (x4, x1);
	var O = paint (I, x5);
	return O;
};
export var solve_d0f5fe59 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = size (x1);
	var x3 = astuple (x2, x2);
	var x4 = canvas (ZERO, x3);
	var x5 = shoot (ORIGIN, UNITY);
	var O = fill (x4, EIGHT, x5);
	return O;
};
export var solve_e3497940 = function (I) {
	var x1 = lefthalf (I);
	var x2 = righthalf (I);
	var x3 = vmirror (x2);
	var x4 = objects (x3, T, F, T);
	var x5 = merge (x4);
	var O = paint (x1, x5);
	return O;
};
export var solve_e9afcf9a = function (I) {
	var x1 = astuple (TWO, ONE);
	var x2 = crop (I, ORIGIN, x1);
	var x3 = hmirror (x2);
	var x4 = hconcat (x2, x3);
	var x5 = hconcat (x4, x4);
	var O = hconcat (x5, x4);
	return O;
};
export var solve_48d8fb45 = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = matcher (size, ONE);
	var x3 = extract (x1, x2);
	var x4 = lbind (adjacent, x3);
	var x5 = extract (x1, x4);
	var O = subgrid (x5, I);
	return O;
};
export var solve_d406998b = function (I) {
	var x1 = vmirror (I);
	var x2 = ofcolor (x1, FIVE);
	var x3 = compose (even, last);
	var x4 = sfilter (x2, x3);
	var x5 = fill (x1, THREE, x4);
	var O = vmirror (x5);
	return O;
};
export var solve_5117e062 = function (I) {
	var x1 = objects (I, F, T, T);
	var x2 = matcher (numcolors, TWO);
	var x3 = extract (x1, x2);
	var x4 = subgrid (x3, I);
	var x5 = mostcolor (x3);
	var O = py_replace (x4, EIGHT, x5);
	return O;
};
export var solve_3906de3d = function (I) {
	var x1 = rot270 (I);
	var x2 = rbind (order, identity);
	var x3 = py_switch (x1, ONE, TWO);
	var x4 = apply (x2, x3);
	var x5 = py_switch (x4, ONE, TWO);
	var O = cmirror (x5);
	return O;
};
export var solve_00d62c1b = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = colorfilter (x1, ZERO);
	var x3 = rbind (bordering, I);
	var x4 = compose (flip, x3);
	var x5 = mfilter (x2, x4);
	var O = fill (I, FOUR, x5);
	return O;
};
export var solve_7b6016b9 = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = rbind (bordering, I);
	var x3 = compose (flip, x2);
	var x4 = mfilter (x1, x3);
	var x5 = fill (I, TWO, x4);
	var O = py_replace (x5, ZERO, THREE);
	return O;
};
export var solve_67385a82 = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = colorfilter (x1, THREE);
	var x3 = sizefilter (x2, ONE);
	var x4 = difference (x2, x3);
	var x5 = merge (x4);
	var O = fill (I, EIGHT, x5);
	return O;
};
export var solve_a5313dff = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = colorfilter (x1, ZERO);
	var x3 = rbind (bordering, I);
	var x4 = compose (flip, x3);
	var x5 = mfilter (x2, x4);
	var O = fill (I, ONE, x5);
	return O;
};
export var solve_ea32f347 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = py_replace (I, FIVE, FOUR);
	var x3 = argmin (x1, size);
	var x4 = argmax (x1, size);
	var x5 = fill (x2, ONE, x4);
	var O = fill (x5, TWO, x3);
	return O;
};
export var solve_d631b094 = function (I) {
	var x1 = palette (I);
	var x2 = other (x1, ZERO);
	var x3 = ofcolor (I, x2);
	var x4 = size (x3);
	var x5 = astuple (ONE, x4);
	var O = canvas (x2, x5);
	return O;
};
export var solve_10fcaaa3 = function (I) {
	var x1 = leastcolor (I);
	var x2 = hconcat (I, I);
	var x3 = vconcat (x2, x2);
	var x4 = ofcolor (x3, x1);
	var x5 = mapply (ineighbors, x4);
	var O = underfill (x3, EIGHT, x5);
	return O;
};
export var solve_007bbfb7 = function (I) {
	var x1 = hupscale (I, THREE);
	var x2 = vupscale (x1, THREE);
	var x3 = hconcat (I, I);
	var x4 = hconcat (x3, I);
	var x5 = vconcat (x4, x4);
	var x6 = vconcat (x5, x4);
	var O = cellwise (x2, x6, ZERO);
	return O;
};
export var solve_496994bd = function (I) {
	var x1 = width (I);
	var x2 = height (I);
	var x3 = halve (x2);
	var x4 = astuple (x3, x1);
	var x5 = crop (I, ORIGIN, x4);
	var x6 = hmirror (x5);
	var O = vconcat (x5, x6);
	return O;
};
export var solve_1f876c06 = function (I) {
	var x1 = fgpartition (I);
	var x2 = compose (last, first);
	var x3 = power (last, TWO);
	var x4 = fork (connect, x2, x3);
	var x5 = fork (recolor, color, x4);
	var x6 = mapply (x5, x1);
	var O = paint (I, x6);
	return O;
};
export var solve_05f2a901 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = colorfilter (x1, TWO);
	var x3 = first (x2);
	var x4 = colorfilter (x1, EIGHT);
	var x5 = first (x4);
	var x6 = gravitate (x3, x5);
	var O = move (I, x3, x6);
	return O;
};
export var solve_39a8645d = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = totuple (x1);
	var x3 = apply (color, x2);
	var x4 = mostcommon (x3);
	var x5 = matcher (color, x4);
	var x6 = extract (x1, x5);
	var O = subgrid (x6, I);
	return O;
};
export var solve_1b2d62fb = function (I) {
	var x1 = lefthalf (I);
	var x2 = righthalf (I);
	var x3 = ofcolor (x1, ZERO);
	var x4 = ofcolor (x2, ZERO);
	var x5 = intersection (x3, x4);
	var x6 = py_replace (x1, NINE, ZERO);
	var O = fill (x6, EIGHT, x5);
	return O;
};
export var solve_90c28cc7 = function (I) {
	var x1 = objects (I, F, F, T);
	var x2 = first (x1);
	var x3 = subgrid (x2, I);
	var x4 = dedupe (x3);
	var x5 = rot90 (x4);
	var x6 = dedupe (x5);
	var O = rot270 (x6);
	return O;
};
export var solve_b6afb2da = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = py_replace (I, FIVE, TWO);
	var x3 = colorfilter (x1, FIVE);
	var x4 = mapply (box, x3);
	var x5 = fill (x2, FOUR, x4);
	var x6 = mapply (corners, x3);
	var O = fill (x5, ONE, x6);
	return O;
};
export var solve_b9b7f026 = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = argmin (x1, size);
	var x3 = rbind (adjacent, x2);
	var x4 = remove (x2, x1);
	var x5 = extract (x4, x3);
	var x6 = color (x5);
	var O = canvas (x6, UNITY);
	return O;
};
export var solve_ba97ae07 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = totuple (x1);
	var x3 = apply (color, x2);
	var x4 = mostcommon (x3);
	var x5 = ofcolor (I, x4);
	var x6 = backdrop (x5);
	var O = fill (I, x4, x6);
	return O;
};
export var solve_c9f8e694 = function (I) {
	var x1 = height (I);
	var x2 = width (I);
	var x3 = ofcolor (I, ZERO);
	var x4 = astuple (x1, ONE);
	var x5 = crop (I, ORIGIN, x4);
	var x6 = hupscale (x5, x2);
	var O = fill (x6, ZERO, x3);
	return O;
};
export var solve_d23f8c26 = function (I) {
	var x1 = asindices (I);
	var x2 = width (I);
	var x3 = halve (x2);
	var x4 = matcher (last, x3);
	var x5 = compose (flip, x4);
	var x6 = sfilter (x1, x5);
	var O = fill (I, ZERO, x6);
	return O;
};
export var solve_d5d6de2d = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = sfilter (x1, square);
	var x3 = difference (x1, x2);
	var x4 = compose (backdrop, inbox);
	var x5 = mapply (x4, x3);
	var x6 = py_replace (I, TWO, ZERO);
	var O = fill (x6, THREE, x5);
	return O;
};
export var solve_dbc1a6ce = function (I) {
	var x1 = ofcolor (I, ONE);
	var x2 = product (x1, x1);
	var x3 = fork (connect, first, last);
	var x4 = apply (x3, x2);
	var x5 = fork (either, vline, hline);
	var x6 = mfilter (x4, x5);
	var O = underfill (I, EIGHT, x6);
	return O;
};
export var solve_ded97339 = function (I) {
	var x1 = ofcolor (I, EIGHT);
	var x2 = product (x1, x1);
	var x3 = fork (connect, first, last);
	var x4 = apply (x3, x2);
	var x5 = fork (either, vline, hline);
	var x6 = mfilter (x4, x5);
	var O = underfill (I, EIGHT, x6);
	return O;
};
export var solve_ea786f4a = function (I) {
	var x1 = width (I);
	var x2 = shoot (ORIGIN, UNITY);
	var x3 = decrement (x1);
	var x4 = tojvec (x3);
	var x5 = shoot (x4, DOWN_LEFT);
	var x6 = combine (x2, x5);
	var O = fill (I, ZERO, x6);
	return O;
};
export var solve_08ed6ac7 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = totuple (x1);
	var x3 = order (x1, height);
	var x4 = size (x2);
	var x5 = interval (x4, ZERO, NEG_ONE);
	var x6 = mpapply (recolor, x5, x3);
	var O = paint (I, x6);
	return O;
};
export var solve_40853293 = function (I) {
	var x1 = partition (I);
	var x2 = fork (recolor, color, backdrop);
	var x3 = apply (x2, x1);
	var x4 = mfilter (x3, hline);
	var x5 = mfilter (x3, vline);
	var x6 = paint (I, x4);
	var O = paint (x6, x5);
	return O;
};
export var solve_5521c0d9 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = merge (x1);
	var x3 = cover (I, x2);
	var x4 = chain (toivec, invert, height);
	var x5 = fork (shift, identity, x4);
	var x6 = mapply (x5, x1);
	var O = paint (x3, x6);
	return O;
};
export var solve_f8ff0b80 = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = order (x1, size);
	var x3 = apply (color, x2);
	var x4 = rbind (canvas, UNITY);
	var x5 = apply (x4, x3);
	var x6 = merge (x5);
	var O = hmirror (x6);
	return O;
};
export var solve_85c4e7cd = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = compose (invert, size);
	var x3 = order (x1, size);
	var x4 = order (x1, x2);
	var x5 = apply (color, x4);
	var x6 = mpapply (recolor, x5, x3);
	var O = paint (I, x6);
	return O;
};
export var solve_d2abd087 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = matcher (size, SIX);
	var x3 = compose (flip, x2);
	var x4 = mfilter (x1, x2);
	var x5 = mfilter (x1, x3);
	var x6 = fill (I, TWO, x4);
	var O = fill (x6, ONE, x5);
	return O;
};
export var solve_017c7c7b = function (I) {
	var x1 = tophalf (I);
	var x2 = bottomhalf (I);
	var x3 = equality (x1, x2);
	var x4 = crop (I, TWO_BY_ZERO, THREE_BY_THREE);
	var x5 = branch (x3, x2, x4);
	var x6 = vconcat (I, x5);
	var O = py_replace (x6, ONE, TWO);
	return O;
};
export var solve_363442ee = function (I) {
	var x1 = ofcolor (I, ONE);
	var x2 = crop (I, ORIGIN, THREE_BY_THREE);
	var x3 = asobject (x2);
	var x4 = lbind (shift, x3);
	var x5 = compose (x4, decrement);
	var x6 = mapply (x5, x1);
	var O = paint (I, x6);
	return O;
};
export var solve_5168d44c = function (I) {
	var x1 = ofcolor (I, THREE);
	var x2 = height (x1);
	var x3 = equality (x2, ONE);
	var x4 = branch (x3, ZERO_BY_TWO, TWO_BY_ZERO);
	var x5 = ofcolor (I, TWO);
	var x6 = recolor (TWO, x5);
	var O = move (I, x6, x4);
	return O;
};
export var solve_e9614598 = function (I) {
	var x1 = ofcolor (I, ONE);
	var x2 = fork (add, first, last);
	var x3 = x2 (x1);
	var x4 = halve (x3);
	var x5 = dneighbors (x4);
	var x6 = insert (x4, x5);
	var O = fill (I, THREE, x6);
	return O;
};
export var solve_d9fac9be = function (I) {
	var x1 = palette (I);
	var x2 = objects (I, T, F, T);
	var x3 = argmax (x2, size);
	var x4 = color (x3);
	var x5 = remove (ZERO, x1);
	var x6 = other (x5, x4);
	var O = canvas (x6, UNITY);
	return O;
};
export var solve_e50d258f = function (I) {
	var x1 = width (I);
	var x2 = astuple (NINE, x1);
	var x3 = canvas (ZERO, x2);
	var x4 = vconcat (I, x3);
	var x5 = objects (x4, F, F, T);
	var x6 = rbind (colorcount, TWO);
	var x7 = argmax (x5, x6);
	var O = subgrid (x7, I);
	return O;
};
export var solve_810b9b61 = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = apply (toindices, x1);
	var x3 = fork (either, vline, hline);
	var x4 = sfilter (x2, x3);
	var x5 = difference (x2, x4);
	var x6 = fork (equality, identity, box);
	var x7 = mfilter (x5, x6);
	var O = fill (I, THREE, x7);
	return O;
};
export var solve_54d82841 = function (I) {
	var x1 = height (I);
	var x2 = objects (I, T, F, T);
	var x3 = compose (last, center);
	var x4 = apply (x3, x2);
	var x5 = decrement (x1);
	var x6 = lbind (astuple, x5);
	var x7 = apply (x6, x4);
	var O = fill (I, FOUR, x7);
	return O;
};
export var solve_60b61512 = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = rbind (subgrid, I);
	var x3 = compose (asindices, x2);
	var x4 = fork (shift, x3, ulcorner);
	var x5 = mapply (x4, x1);
	var x6 = fill (I, SEVEN, x5);
	var x7 = merge (x1);
	var O = paint (x6, x7);
	return O;
};
export var solve_25d8a9c8 = function (I) {
	var x1 = asindices (I);
	var x2 = objects (I, T, F, F);
	var x3 = sizefilter (x2, THREE);
	var x4 = mfilter (x3, hline);
	var x5 = toindices (x4);
	var x6 = difference (x1, x5);
	var x7 = fill (I, FIVE, x5);
	var O = fill (x7, ZERO, x6);
	return O;
};
export var solve_239be575 = function (I) {
	var x1 = objects (I, F, T, T);
	var x2 = lbind (contained, TWO);
	var x3 = compose (x2, palette);
	var x4 = sfilter (x1, x3);
	var x5 = size (x4);
	var x6 = greater (x5, ONE);
	var x7 = branch (x6, ZERO, EIGHT);
	var O = canvas (x7, UNITY);
	return O;
};
export var solve_67a423a3 = function (I) {
	var x1 = leastcolor (I);
	var x2 = objects (I, T, F, T);
	var x3 = colorfilter (x2, x1);
	var x4 = merge (x3);
	var x5 = delta (x4);
	var x6 = first (x5);
	var x7 = neighbors (x6);
	var O = fill (I, FOUR, x7);
	return O;
};
export var solve_5c0a986e = function (I) {
	var x1 = ofcolor (I, TWO);
	var x2 = ofcolor (I, ONE);
	var x3 = lrcorner (x1);
	var x4 = ulcorner (x2);
	var x5 = shoot (x3, UNITY);
	var x6 = shoot (x4, NEG_UNITY);
	var x7 = fill (I, TWO, x5);
	var O = fill (x7, ONE, x6);
	return O;
};
export var solve_6430c8c4 = function (I) {
	var x1 = tophalf (I);
	var x2 = bottomhalf (I);
	var x3 = astuple (FOUR, FOUR);
	var x4 = ofcolor (x1, ZERO);
	var x5 = ofcolor (x2, ZERO);
	var x6 = intersection (x4, x5);
	var x7 = canvas (ZERO, x3);
	var O = fill (x7, THREE, x6);
	return O;
};
export var solve_94f9d214 = function (I) {
	var x1 = tophalf (I);
	var x2 = bottomhalf (I);
	var x3 = ofcolor (x1, ZERO);
	var x4 = ofcolor (x2, ZERO);
	var x5 = astuple (FOUR, FOUR);
	var x6 = canvas (ZERO, x5);
	var x7 = intersection (x3, x4);
	var O = fill (x6, TWO, x7);
	return O;
};
export var solve_a1570a43 = function (I) {
	var x1 = ofcolor (I, TWO);
	var x2 = ofcolor (I, THREE);
	var x3 = recolor (TWO, x1);
	var x4 = ulcorner (x2);
	var x5 = ulcorner (x1);
	var x6 = subtract (x4, x5);
	var x7 = increment (x6);
	var O = move (I, x3, x7);
	return O;
};
export var solve_ce4f8723 = function (I) {
	var x1 = tophalf (I);
	var x2 = bottomhalf (I);
	var x3 = ofcolor (x1, ZERO);
	var x4 = ofcolor (x2, ZERO);
	var x5 = intersection (x3, x4);
	var x6 = astuple (FOUR, FOUR);
	var x7 = canvas (THREE, x6);
	var O = fill (x7, ZERO, x5);
	return O;
};
export var solve_d13f3404 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = rbind (shoot, UNITY);
	var x3 = compose (x2, center);
	var x4 = fork (recolor, color, x3);
	var x5 = mapply (x4, x1);
	var x6 = astuple (SIX, SIX);
	var x7 = canvas (ZERO, x6);
	var O = paint (x7, x5);
	return O;
};
export var solve_dc433765 = function (I) {
	var x1 = ofcolor (I, THREE);
	var x2 = ofcolor (I, FOUR);
	var x3 = first (x1);
	var x4 = first (x2);
	var x5 = subtract (x4, x3);
	var x6 = sign (x5);
	var x7 = recolor (THREE, x1);
	var O = move (I, x7, x6);
	return O;
};
export var solve_f2829549 = function (I) {
	var x1 = lefthalf (I);
	var x2 = righthalf (I);
	var x3 = ofcolor (x1, ZERO);
	var x4 = ofcolor (x2, ZERO);
	var x5 = intersection (x3, x4);
	var x6 = shape (x1);
	var x7 = canvas (ZERO, x6);
	var O = fill (x7, THREE, x5);
	return O;
};
export var solve_fafffa47 = function (I) {
	var x1 = tophalf (I);
	var x2 = bottomhalf (I);
	var x3 = shape (x2);
	var x4 = ofcolor (x1, ZERO);
	var x5 = ofcolor (x2, ZERO);
	var x6 = intersection (x4, x5);
	var x7 = canvas (ZERO, x3);
	var O = fill (x7, TWO, x6);
	return O;
};
export var solve_fcb5c309 = function (I) {
	var x1 = leastcolor (I);
	var x2 = objects (I, T, F, T);
	var x3 = colorfilter (x2, x1);
	var x4 = difference (x2, x3);
	var x5 = argmax (x4, size);
	var x6 = color (x5);
	var x7 = subgrid (x5, I);
	var O = py_replace (x7, x6, x1);
	return O;
};
export var solve_ff805c23 = function (I) {
	var x1 = hmirror (I);
	var x2 = vmirror (I);
	var x3 = ofcolor (I, ONE);
	var x4 = subgrid (x3, x1);
	var x5 = subgrid (x3, x2);
	var x6 = palette (x4);
	var x7 = contained (ONE, x6);
	var O = branch (x7, x5, x4);
	return O;
};
export var solve_e76a88a6 = function (I) {
	var x1 = objects (I, F, F, T);
	var x2 = argmax (x1, numcolors);
	var x3 = normalize (x2);
	var x4 = remove (x2, x1);
	var x5 = apply (ulcorner, x4);
	var x6 = lbind (shift, x3);
	var x7 = mapply (x6, x5);
	var O = paint (I, x7);
	return O;
};
export var solve_7c008303 = function (I) {
	var x1 = ofcolor (I, THREE);
	var x2 = subgrid (x1, I);
	var x3 = ofcolor (x2, ZERO);
	var x4 = py_replace (I, THREE, ZERO);
	var x5 = py_replace (x4, EIGHT, ZERO);
	var x6 = compress (x5);
	var x7 = upscale (x6, THREE);
	var O = fill (x7, ZERO, x3);
	return O;
};
export var solve_7f4411dc = function (I) {
	var x1 = leastcolor (I);
	var x2 = ofcolor (I, x1);
	var x3 = rbind (difference, x2);
	var x4 = rbind (greater, TWO);
	var x5 = chain (x4, size, x3);
	var x6 = compose (x5, dneighbors);
	var x7 = sfilter (x2, x6);
	var O = fill (I, ZERO, x7);
	return O;
};
export var solve_b230c067 = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = totuple (x1);
	var x3 = apply (normalize, x2);
	var x4 = leastcommon (x3);
	var x5 = matcher (normalize, x4);
	var x6 = extract (x1, x5);
	var x7 = py_replace (I, EIGHT, ONE);
	var O = fill (x7, TWO, x6);
	return O;
};
export var solve_e8593010 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = sizefilter (x1, ONE);
	var x3 = sizefilter (x1, TWO);
	var x4 = merge (x2);
	var x5 = fill (I, THREE, x4);
	var x6 = merge (x3);
	var x7 = fill (x5, TWO, x6);
	var O = py_replace (x7, ZERO, ONE);
	return O;
};
export var solve_6d75e8bb = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = first (x1);
	var x3 = ulcorner (x2);
	var x4 = subgrid (x2, I);
	var x5 = py_replace (x4, ZERO, TWO);
	var x6 = asobject (x5);
	var x7 = shift (x6, x3);
	var O = paint (I, x7);
	return O;
};
export var solve_3f7978a0 = function (I) {
	var x1 = fgpartition (I);
	var x2 = matcher (color, FIVE);
	var x3 = extract (x1, x2);
	var x4 = ulcorner (x3);
	var x5 = subtract (x4, DOWN);
	var x6 = shape (x3);
	var x7 = add (x6, TWO_BY_ZERO);
	var O = crop (I, x5, x7);
	return O;
};
export var solve_1190e5a7 = function (I) {
	var x1 = mostcolor (I);
	var x2 = frontiers (I);
	var x3 = sfilter (x2, vline);
	var x4 = difference (x2, x3);
	var x5 = astuple (x4, x3);
	var x6 = apply (size, x5);
	var x7 = increment (x6);
	var O = canvas (x1, x7);
	return O;
};
export var solve_6e02f1e3 = function (I) {
	var x1 = numcolors (I);
	var x2 = canvas (ZERO, THREE_BY_THREE);
	var x3 = equality (x1, THREE);
	var x4 = equality (x1, TWO);
	var x5 = branch (x3, TWO_BY_ZERO, ORIGIN);
	var x6 = branch (x4, TWO_BY_TWO, ZERO_BY_TWO);
	var x7 = connect (x5, x6);
	var O = fill (x2, FIVE, x7);
	return O;
};
export var solve_a61f2674 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = argmax (x1, size);
	var x3 = argmin (x1, size);
	var x4 = py_replace (I, FIVE, ZERO);
	var x5 = recolor (ONE, x2);
	var x6 = recolor (TWO, x3);
	var x7 = combine (x5, x6);
	var O = paint (x4, x7);
	return O;
};
export var solve_fcc82909 = function (I) {
	var x1 = objects (I, F, T, T);
	var x2 = rbind (add, DOWN);
	var x3 = compose (x2, llcorner);
	var x4 = compose (toivec, numcolors);
	var x5 = fork (add, lrcorner, x4);
	var x6 = fork (astuple, x3, x5);
	var x7 = compose (box, x6);
	var x8 = mapply (x7, x1);
	var O = fill (I, THREE, x8);
	return O;
};
export var solve_72ca375d = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = totuple (x1);
	var x3 = rbind (subgrid, I);
	var x4 = apply (x3, x2);
	var x5 = apply (vmirror, x4);
	var x6 = papply (equality, x4, x5);
	var x7 = pair (x4, x6);
	var x8 = extract (x7, last);
	var O = first (x8);
	return O;
};
export var solve_253bf280 = function (I) {
	var x1 = ofcolor (I, EIGHT);
	var x2 = prapply (connect, x1, x1);
	var x3 = rbind (greater, ONE);
	var x4 = compose (x3, size);
	var x5 = sfilter (x2, x4);
	var x6 = fork (either, vline, hline);
	var x7 = mfilter (x5, x6);
	var x8 = fill (I, THREE, x7);
	var O = fill (x8, EIGHT, x1);
	return O;
};
export var solve_694f12f3 = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = colorfilter (x1, FOUR);
	var x3 = compose (backdrop, inbox);
	var x4 = argmin (x2, size);
	var x5 = argmax (x2, size);
	var x6 = x3 (x4);
	var x7 = x3 (x5);
	var x8 = fill (I, ONE, x6);
	var O = fill (x8, TWO, x7);
	return O;
};
export var solve_1f642eb9 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = sizefilter (x1, ONE);
	var x3 = difference (x1, x2);
	var x4 = first (x3);
	var x5 = rbind (gravitate, x4);
	var x6 = compose (crement, x5);
	var x7 = fork (shift, identity, x6);
	var x8 = mapply (x7, x2);
	var O = paint (I, x8);
	return O;
};
export var solve_31aa019c = function (I) {
	var x1 = leastcolor (I);
	var x2 = ofcolor (I, x1);
	var x3 = first (x2);
	var x4 = neighbors (x3);
	var x5 = astuple (TEN, TEN);
	var x6 = canvas (ZERO, x5);
	var x7 = initset (x3);
	var x8 = fill (x6, x1, x7);
	var O = fill (x8, TWO, x4);
	return O;
};
export var solve_27a28665 = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = valmax (x1, size);
	var x3 = equality (x2, ONE);
	var x4 = equality (x2, FOUR);
	var x5 = equality (x2, FIVE);
	var x6 = branch (x3, TWO, ONE);
	var x7 = branch (x4, THREE, x6);
	var x8 = branch (x5, SIX, x7);
	var O = canvas (x8, UNITY);
	return O;
};
export var solve_7ddcd7ec = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = sizefilter (x1, ONE);
	var x3 = difference (x1, x2);
	var x4 = first (x3);
	var x5 = color (x4);
	var x6 = lbind (position, x4);
	var x7 = fork (shoot, center, x6);
	var x8 = mapply (x7, x2);
	var O = fill (I, x5, x8);
	return O;
};
export var solve_3bd67248 = function (I) {
	var x1 = height (I);
	var x2 = decrement (x1);
	var x3 = decrement (x2);
	var x4 = astuple (x3, ONE);
	var x5 = astuple (x2, ONE);
	var x6 = shoot (x4, UP_RIGHT);
	var x7 = shoot (x5, RIGHT);
	var x8 = fill (I, TWO, x6);
	var O = fill (x8, FOUR, x7);
	return O;
};
export var solve_73251a56 = function (I) {
	var x1 = dmirror (I);
	var x2 = papply (pair, I, x1);
	var x3 = lbind (apply, maximum);
	var x4 = apply (x3, x2);
	var x5 = mostcolor (x4);
	var x6 = py_replace (x4, ZERO, x5);
	var x7 = index (x6, ORIGIN);
	var x8 = shoot (ORIGIN, UNITY);
	var O = fill (x6, x7, x8);
	return O;
};
export var solve_25d487eb = function (I) {
	var x1 = leastcolor (I);
	var x2 = objects (I, T, F, T);
	var x3 = ofcolor (I, x1);
	var x4 = center (x3);
	var x5 = merge (x2);
	var x6 = center (x5);
	var x7 = subtract (x6, x4);
	var x8 = shoot (x4, x7);
	var O = underfill (I, x1, x8);
	return O;
};
export var solve_8f2ea7aa = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = merge (x1);
	var x3 = subgrid (x2, I);
	var x4 = upscale (x3, THREE);
	var x5 = hconcat (x3, x3);
	var x6 = hconcat (x5, x3);
	var x7 = vconcat (x6, x6);
	var x8 = vconcat (x7, x6);
	var O = cellwise (x4, x8, ZERO);
	return O;
};
export var solve_b8825c91 = function (I) {
	var x1 = py_replace (I, FOUR, ZERO);
	var x2 = dmirror (x1);
	var x3 = papply (pair, x1, x2);
	var x4 = lbind (apply, maximum);
	var x5 = apply (x4, x3);
	var x6 = cmirror (x5);
	var x7 = papply (pair, x5, x6);
	var x8 = apply (x4, x7);
	var O = cmirror (x8);
	return O;
};
export var solve_cce03e0d = function (I) {
	var x1 = upscale (I, THREE);
	var x2 = hconcat (I, I);
	var x3 = hconcat (x2, I);
	var x4 = vconcat (x3, x3);
	var x5 = vconcat (x4, x3);
	var x6 = ofcolor (x1, ZERO);
	var x7 = ofcolor (x1, ONE);
	var x8 = combine (x6, x7);
	var O = fill (x5, ZERO, x8);
	return O;
};
export var solve_d364b489 = function (I) {
	var x1 = ofcolor (I, ONE);
	var x2 = shift (x1, DOWN);
	var x3 = fill (I, EIGHT, x2);
	var x4 = shift (x1, UP);
	var x5 = fill (x3, TWO, x4);
	var x6 = shift (x1, RIGHT);
	var x7 = fill (x5, SIX, x6);
	var x8 = shift (x1, LEFT);
	var O = fill (x7, SEVEN, x8);
	return O;
};
export var solve_a5f85a15 = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = interval (ONE, NINE, ONE);
	var x3 = apply (double, x2);
	var x4 = apply (decrement, x3);
	var x5 = papply (astuple, x4, x4);
	var x6 = apply (ulcorner, x1);
	var x7 = lbind (shift, x5);
	var x8 = mapply (x7, x6);
	var O = fill (I, FOUR, x8);
	return O;
};
export var solve_3ac3eb23 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = chain (ineighbors, last, first);
	var x3 = fork (recolor, color, x2);
	var x4 = mapply (x3, x1);
	var x5 = paint (I, x4);
	var x6 = vsplit (x5, THREE);
	var x7 = first (x6);
	var x8 = vconcat (x7, x7);
	var O = vconcat (x7, x8);
	return O;
};
export var solve_444801d8 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = colorfilter (x1, ONE);
	var x3 = rbind (toobject, I);
	var x4 = chain (leastcolor, x3, delta);
	var x5 = rbind (shift, UP);
	var x6 = compose (x5, backdrop);
	var x7 = fork (recolor, x4, x6);
	var x8 = mapply (x7, x2);
	var O = underpaint (I, x8);
	return O;
};
export var solve_22168020 = function (I) {
	var x1 = palette (I);
	var x2 = remove (ZERO, x1);
	var x3 = lbind (ofcolor, I);
	var x4 = lbind (prapply, connect);
	var x5 = fork (x4, x3, x3);
	var x6 = compose (merge, x5);
	var x7 = fork (recolor, identity, x6);
	var x8 = mapply (x7, x2);
	var O = paint (I, x8);
	return O;
};
export var solve_6e82a1ae = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = lbind (sizefilter, x1);
	var x3 = compose (merge, x2);
	var x4 = x3 (TWO);
	var x5 = x3 (THREE);
	var x6 = x3 (FOUR);
	var x7 = fill (I, THREE, x4);
	var x8 = fill (x7, TWO, x5);
	var O = fill (x8, ONE, x6);
	return O;
};
export var solve_b2862040 = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = colorfilter (x1, NINE);
	var x3 = colorfilter (x1, ONE);
	var x4 = rbind (bordering, I);
	var x5 = compose (flip, x4);
	var x6 = mfilter (x2, x5);
	var x7 = rbind (adjacent, x6);
	var x8 = mfilter (x3, x7);
	var O = fill (I, EIGHT, x8);
	return O;
};
export var solve_868de0fa = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = sfilter (x1, square);
	var x3 = compose (even, height);
	var x4 = sfilter (x2, x3);
	var x5 = difference (x2, x4);
	var x6 = merge (x4);
	var x7 = merge (x5);
	var x8 = fill (I, TWO, x6);
	var O = fill (x8, SEVEN, x7);
	return O;
};
export var solve_681b3aeb = function (I) {
	var x1 = rot270 (I);
	var x2 = objects (x1, T, F, T);
	var x3 = argmax (x2, size);
	var x4 = argmin (x2, size);
	var x5 = color (x4);
	var x6 = canvas (x5, THREE_BY_THREE);
	var x7 = normalize (x3);
	var x8 = paint (x6, x7);
	var O = rot90 (x8);
	return O;
};
export var solve_8e5a5113 = function (I) {
	var x1 = crop (I, ORIGIN, THREE_BY_THREE);
	var x2 = rot90 (x1);
	var x3 = rot180 (x1);
	var x4 = astuple (x2, x3);
	var x5 = astuple (FOUR, EIGHT);
	var x6 = apply (tojvec, x5);
	var x7 = apply (asobject, x4);
	var x8 = mpapply (shift, x7, x6);
	var O = paint (I, x8);
	return O;
};
export var solve_025d127b = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = apply (color, x1);
	var x3 = merge (x1);
	var x4 = lbind (colorfilter, x1);
	var x5 = rbind (argmax, rightmost);
	var x6 = compose (x5, x4);
	var x7 = mapply (x6, x2);
	var x8 = difference (x3, x7);
	var O = move (I, x8, RIGHT);
	return O;
};
export var solve_2281f1f4 = function (I) {
	var x1 = ofcolor (I, FIVE);
	var x2 = product (x1, x1);
	var x3 = power (first, TWO);
	var x4 = power (last, TWO);
	var x5 = fork (astuple, x3, x4);
	var x6 = apply (x5, x2);
	var x7 = urcorner (x1);
	var x8 = remove (x7, x6);
	var O = underfill (I, TWO, x8);
	return O;
};
export var solve_cf98881b = function (I) {
	var x1 = hsplit (I, THREE);
	var x2 = first (x1);
	var x3 = remove (x2, x1);
	var x4 = first (x3);
	var x5 = last (x3);
	var x6 = ofcolor (x4, NINE);
	var x7 = ofcolor (x2, FOUR);
	var x8 = fill (x5, NINE, x6);
	var O = fill (x8, FOUR, x7);
	return O;
};
export var solve_d4f3cd78 = function (I) {
	var x1 = ofcolor (I, FIVE);
	var x2 = delta (x1);
	var x3 = fill (I, EIGHT, x2);
	var x4 = box (x1);
	var x5 = difference (x4, x1);
	var x6 = position (x4, x5);
	var x7 = first (x5);
	var x8 = shoot (x7, x6);
	var O = fill (x3, EIGHT, x8);
	return O;
};
export var solve_bda2d7a6 = function (I) {
	var x1 = partition (I);
	var x2 = order (x1, size);
	var x3 = apply (color, x2);
	var x4 = last (x2);
	var x5 = remove (x4, x2);
	var x6 = repeat (x4, ONE);
	var x7 = combine (x6, x5);
	var x8 = mpapply (recolor, x3, x7);
	var O = paint (I, x8);
	return O;
};
export var solve_137eaa0f = function (I) {
	var x1 = objects (I, F, T, T);
	var x2 = matcher (first, FIVE);
	var x3 = rbind (sfilter, x2);
	var x4 = chain (invert, center, x3);
	var x5 = fork (shift, identity, x4);
	var x6 = canvas (ZERO, THREE_BY_THREE);
	var x7 = mapply (x5, x1);
	var x8 = shift (x7, UNITY);
	var O = paint (x6, x8);
	return O;
};
export var solve_6455b5f5 = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = colorfilter (x1, ZERO);
	var x3 = argmax (x1, size);
	var x4 = valmin (x1, size);
	var x5 = sizefilter (x2, x4);
	var x6 = recolor (ONE, x3);
	var x7 = merge (x5);
	var x8 = paint (I, x6);
	var O = fill (x8, EIGHT, x7);
	return O;
};
export var solve_b8cdaf2b = function (I) {
	var x1 = leastcolor (I);
	var x2 = ofcolor (I, x1);
	var x3 = shift (x2, UP);
	var x4 = ulcorner (x3);
	var x5 = urcorner (x3);
	var x6 = shoot (x4, NEG_UNITY);
	var x7 = shoot (x5, UP_RIGHT);
	var x8 = combine (x6, x7);
	var O = underfill (I, x1, x8);
	return O;
};
export var solve_bd4472b8 = function (I) {
	var x1 = width (I);
	var x2 = astuple (TWO, x1);
	var x3 = crop (I, ORIGIN, x2);
	var x4 = tophalf (x3);
	var x5 = dmirror (x4);
	var x6 = hupscale (x5, x1);
	var x7 = repeat (x6, TWO);
	var x8 = merge (x7);
	var O = vconcat (x3, x8);
	return O;
};
export var solve_4be741c5 = function (I) {
	var x1 = portrait (I);
	var x2 = branch (x1, dmirror, identity);
	var x3 = branch (x1, height, width);
	var x4 = x3 (I);
	var x5 = astuple (ONE, x4);
	var x6 = x2 (I);
	var x7 = crop (x6, ORIGIN, x5);
	var x8 = apply (dedupe, x7);
	var O = x2 (x8);
	return O;
};
export var solve_bbc9ae5d = function (I) {
	var x1 = width (I);
	var x2 = palette (I);
	var x3 = halve (x1);
	var x4 = vupscale (I, x3);
	var x5 = rbind (shoot, UNITY);
	var x6 = other (x2, ZERO);
	var x7 = ofcolor (x4, x6);
	var x8 = mapply (x5, x7);
	var O = fill (x4, x6, x8);
	return O;
};
export var solve_d90796e8 = function (I) {
	var x1 = objects (I, F, F, T);
	var x2 = sizefilter (x1, TWO);
	var x3 = lbind (contained, TWO);
	var x4 = compose (x3, palette);
	var x5 = mfilter (x2, x4);
	var x6 = cover (I, x5);
	var x7 = matcher (first, THREE);
	var x8 = sfilter (x5, x7);
	var O = fill (x6, EIGHT, x8);
	return O;
};
export var solve_2c608aff = function (I) {
	var x1 = leastcolor (I);
	var x2 = objects (I, T, F, T);
	var x3 = argmax (x2, size);
	var x4 = toindices (x3);
	var x5 = ofcolor (I, x1);
	var x6 = prapply (connect, x4, x5);
	var x7 = fork (either, vline, hline);
	var x8 = mfilter (x6, x7);
	var O = underfill (I, x1, x8);
	return O;
};
export var solve_f8b3ba0a = function (I) {
	var x1 = compress (I);
	var x2 = astuple (THREE, ONE);
	var x3 = palette (x1);
	var x4 = lbind (colorcount, x1);
	var x5 = compose (invert, x4);
	var x6 = order (x3, x5);
	var x7 = rbind (canvas, UNITY);
	var x8 = apply (x7, x6);
	var x9 = merge (x8);
	var O = crop (x9, DOWN, x2);
	return O;
};
export var solve_80af3007 = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = first (x1);
	var x3 = subgrid (x2, I);
	var x4 = upscale (x3, THREE);
	var x5 = hconcat (x3, x3);
	var x6 = hconcat (x5, x3);
	var x7 = vconcat (x6, x6);
	var x8 = vconcat (x7, x6);
	var x9 = cellwise (x4, x8, ZERO);
	var O = downscale (x9, THREE);
	return O;
};
export var solve_83302e8f = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = colorfilter (x1, ZERO);
	var x3 = sfilter (x2, square);
	var x4 = difference (x2, x3);
	var x5 = merge (x3);
	var x6 = recolor (THREE, x5);
	var x7 = merge (x4);
	var x8 = recolor (FOUR, x7);
	var x9 = paint (I, x6);
	var O = paint (x9, x8);
	return O;
};
export var solve_1fad071e = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = colorfilter (x1, ONE);
	var x3 = sizefilter (x2, FOUR);
	var x4 = size (x3);
	var x5 = subtract (FIVE, x4);
	var x6 = astuple (ONE, x4);
	var x7 = canvas (ONE, x6);
	var x8 = astuple (ONE, x5);
	var x9 = canvas (ZERO, x8);
	var O = hconcat (x7, x9);
	return O;
};
export var solve_11852cab = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = merge (x1);
	var x3 = hmirror (x2);
	var x4 = vmirror (x2);
	var x5 = dmirror (x2);
	var x6 = cmirror (x2);
	var x7 = paint (I, x3);
	var x8 = paint (x7, x4);
	var x9 = paint (x8, x5);
	var O = paint (x9, x6);
	return O;
};
export var solve_3428a4f5 = function (I) {
	var x1 = tophalf (I);
	var x2 = bottomhalf (I);
	var x3 = astuple (SIX, FIVE);
	var x4 = ofcolor (x1, TWO);
	var x5 = ofcolor (x2, TWO);
	var x6 = combine (x4, x5);
	var x7 = intersection (x4, x5);
	var x8 = difference (x6, x7);
	var x9 = canvas (ZERO, x3);
	var O = fill (x9, THREE, x8);
	return O;
};
export var solve_178fcbfb = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = ofcolor (I, TWO);
	var x3 = mapply (vfrontier, x2);
	var x4 = fill (I, TWO, x3);
	var x5 = colorfilter (x1, TWO);
	var x6 = difference (x1, x5);
	var x7 = compose (hfrontier, center);
	var x8 = fork (recolor, color, x7);
	var x9 = mapply (x8, x6);
	var O = paint (x4, x9);
	return O;
};
export var solve_3de23699 = function (I) {
	var x1 = fgpartition (I);
	var x2 = sizefilter (x1, FOUR);
	var x3 = first (x2);
	var x4 = difference (x1, x2);
	var x5 = first (x4);
	var x6 = color (x3);
	var x7 = color (x5);
	var x8 = subgrid (x3, I);
	var x9 = trim (x8);
	var O = py_replace (x9, x7, x6);
	return O;
};
export var solve_54d9e175 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = sizefilter (x1, ONE);
	var x3 = compose (neighbors, center);
	var x4 = fork (recolor, color, x3);
	var x5 = mapply (x4, x2);
	var x6 = paint (I, x5);
	var x7 = py_replace (x6, ONE, SIX);
	var x8 = py_replace (x7, TWO, SEVEN);
	var x9 = py_replace (x8, THREE, EIGHT);
	var O = py_replace (x9, FOUR, NINE);
	return O;
};
export var solve_5ad4f10b = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = argmax (x1, size);
	var x3 = color (x2);
	var x4 = subgrid (x2, I);
	var x5 = leastcolor (x4);
	var x6 = py_replace (x4, x5, ZERO);
	var x7 = py_replace (x6, x3, x5);
	var x8 = height (x7);
	var x9 = divide (x8, THREE);
	var O = downscale (x7, x9);
	return O;
};
export var solve_623ea044 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = first (x1);
	var x3 = center (x2);
	var x4 = color (x2);
	var x5 = astuple (UNITY, NEG_UNITY);
	var x6 = astuple (UP_RIGHT, DOWN_LEFT);
	var x7 = combine (x5, x6);
	var x8 = lbind (shoot, x3);
	var x9 = mapply (x8, x7);
	var O = fill (I, x4, x9);
	return O;
};
export var solve_6b9890af = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = ofcolor (I, TWO);
	var x3 = argmin (x1, size);
	var x4 = subgrid (x2, I);
	var x5 = width (x4);
	var x6 = divide (x5, THREE);
	var x7 = upscale (x3, x6);
	var x8 = normalize (x7);
	var x9 = shift (x8, UNITY);
	var O = paint (x4, x9);
	return O;
};
export var solve_794b24be = function (I) {
	var x1 = ofcolor (I, ONE);
	var x2 = size (x1);
	var x3 = decrement (x2);
	var x4 = canvas (ZERO, THREE_BY_THREE);
	var x5 = tojvec (x3);
	var x6 = connect (ORIGIN, x5);
	var x7 = equality (x2, FOUR);
	var x8 = insert (UNITY, x6);
	var x9 = branch (x7, x8, x6);
	var O = fill (x4, TWO, x9);
	return O;
};
export var solve_88a10436 = function (I) {
	var x1 = objects (I, F, F, T);
	var x2 = colorfilter (x1, FIVE);
	var x3 = first (x2);
	var x4 = center (x3);
	var x5 = difference (x1, x2);
	var x6 = first (x5);
	var x7 = normalize (x6);
	var x8 = shift (x7, x4);
	var x9 = shift (x8, NEG_UNITY);
	var O = paint (I, x9);
	return O;
};
export var solve_88a62173 = function (I) {
	var x1 = lefthalf (I);
	var x2 = righthalf (I);
	var x3 = tophalf (x1);
	var x4 = tophalf (x2);
	var x5 = bottomhalf (x1);
	var x6 = bottomhalf (x2);
	var x7 = astuple (x3, x4);
	var x8 = astuple (x5, x6);
	var x9 = combine (x7, x8);
	var O = leastcommon (x9);
	return O;
};
export var solve_890034e9 = function (I) {
	var x1 = leastcolor (I);
	var x2 = ofcolor (I, x1);
	var x3 = inbox (x2);
	var x4 = recolor (ZERO, x3);
	var x5 = occurrences (I, x4);
	var x6 = normalize (x2);
	var x7 = shift (x6, NEG_UNITY);
	var x8 = lbind (shift, x7);
	var x9 = mapply (x8, x5);
	var O = fill (I, x1, x9);
	return O;
};
export var solve_99b1bc43 = function (I) {
	var x1 = tophalf (I);
	var x2 = bottomhalf (I);
	var x3 = ofcolor (x1, ZERO);
	var x4 = ofcolor (x2, ZERO);
	var x5 = combine (x3, x4);
	var x6 = intersection (x3, x4);
	var x7 = difference (x5, x6);
	var x8 = shape (x1);
	var x9 = canvas (ZERO, x8);
	var O = fill (x9, THREE, x7);
	return O;
};
export var solve_a9f96cdd = function (I) {
	var x1 = ofcolor (I, TWO);
	var x2 = py_replace (I, TWO, ZERO);
	var x3 = shift (x1, NEG_UNITY);
	var x4 = fill (x2, THREE, x3);
	var x5 = shift (x1, UP_RIGHT);
	var x6 = fill (x4, SIX, x5);
	var x7 = shift (x1, DOWN_LEFT);
	var x8 = fill (x6, EIGHT, x7);
	var x9 = shift (x1, UNITY);
	var O = fill (x8, SEVEN, x9);
	return O;
};
export var solve_af902bf9 = function (I) {
	var x1 = ofcolor (I, FOUR);
	var x2 = prapply (connect, x1, x1);
	var x3 = fork (either, vline, hline);
	var x4 = mfilter (x2, x3);
	var x5 = underfill (I, NEG_ONE, x4);
	var x6 = objects (x5, F, F, T);
	var x7 = compose (backdrop, inbox);
	var x8 = mapply (x7, x6);
	var x9 = fill (x5, TWO, x8);
	var O = py_replace (x9, NEG_ONE, ZERO);
	return O;
};
export var solve_b548a754 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = py_replace (I, EIGHT, ZERO);
	var x3 = leastcolor (x2);
	var x4 = py_replace (x2, x3, ZERO);
	var x5 = leastcolor (x4);
	var x6 = merge (x1);
	var x7 = backdrop (x6);
	var x8 = box (x6);
	var x9 = fill (I, x3, x7);
	var O = fill (x9, x5, x8);
	return O;
};
export var solve_bdad9b1f = function (I) {
	var x1 = ofcolor (I, TWO);
	var x2 = ofcolor (I, EIGHT);
	var x3 = center (x1);
	var x4 = center (x2);
	var x5 = hfrontier (x3);
	var x6 = vfrontier (x4);
	var x7 = intersection (x5, x6);
	var x8 = fill (I, TWO, x5);
	var x9 = fill (x8, EIGHT, x6);
	var O = fill (x9, FOUR, x7);
	return O;
};
export var solve_c3e719e8 = function (I) {
	var x1 = mostcolor (I);
	var x2 = hconcat (I, I);
	var x3 = upscale (I, THREE);
	var x4 = ofcolor (x3, x1);
	var x5 = asindices (x3);
	var x6 = difference (x5, x4);
	var x7 = hconcat (x2, I);
	var x8 = vconcat (x7, x7);
	var x9 = vconcat (x8, x7);
	var O = fill (x9, ZERO, x6);
	return O;
};
export var solve_de1cd16c = function (I) {
	var x1 = leastcolor (I);
	var x2 = objects (I, T, F, F);
	var x3 = sizefilter (x2, ONE);
	var x4 = difference (x2, x3);
	var x5 = rbind (subgrid, I);
	var x6 = apply (x5, x4);
	var x7 = rbind (colorcount, x1);
	var x8 = argmax (x6, x7);
	var x9 = mostcolor (x8);
	var O = canvas (x9, UNITY);
	return O;
};
export var solve_d8c310e9 = function (I) {
	var x1 = objects (I, F, F, T);
	var x2 = first (x1);
	var x3 = hperiod (x2);
	var x4 = multiply (x3, THREE);
	var x5 = tojvec (x3);
	var x6 = tojvec (x4);
	var x7 = shift (x2, x5);
	var x8 = shift (x2, x6);
	var x9 = paint (I, x7);
	var O = paint (x9, x8);
	return O;
};
export var solve_a3325580 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = valmax (x1, size);
	var x3 = sizefilter (x1, x2);
	var x4 = order (x3, leftmost);
	var x5 = apply (color, x4);
	var x6 = astuple (ONE, x2);
	var x7 = rbind (canvas, x6);
	var x8 = apply (x7, x5);
	var x9 = merge (x8);
	var O = dmirror (x9);
	return O;
};
export var solve_8eb1be9a = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = first (x1);
	var x3 = interval (NEG_TWO, FOUR, ONE);
	var x4 = lbind (shift, x2);
	var x5 = height (x2);
	var x6 = rbind (multiply, x5);
	var x7 = apply (x6, x3);
	var x8 = apply (toivec, x7);
	var x9 = mapply (x4, x8);
	var O = paint (I, x9);
	return O;
};
export var solve_321b1fc6 = function (I) {
	var x1 = objects (I, F, F, T);
	var x2 = colorfilter (x1, EIGHT);
	var x3 = difference (x1, x2);
	var x4 = first (x3);
	var x5 = cover (I, x4);
	var x6 = normalize (x4);
	var x7 = lbind (shift, x6);
	var x8 = apply (ulcorner, x2);
	var x9 = mapply (x7, x8);
	var O = paint (x5, x9);
	return O;
};
export var solve_1caeab9d = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = ofcolor (I, ONE);
	var x3 = lowermost (x2);
	var x4 = lbind (subtract, x3);
	var x5 = chain (toivec, x4, lowermost);
	var x6 = fork (shift, identity, x5);
	var x7 = merge (x1);
	var x8 = cover (I, x7);
	var x9 = mapply (x6, x1);
	var O = paint (x8, x9);
	return O;
};
export var solve_77fdfe62 = function (I) {
	var x1 = ofcolor (I, EIGHT);
	var x2 = subgrid (x1, I);
	var x3 = py_replace (I, EIGHT, ZERO);
	var x4 = py_replace (x3, ONE, ZERO);
	var x5 = compress (x4);
	var x6 = width (x2);
	var x7 = halve (x6);
	var x8 = upscale (x5, x7);
	var x9 = ofcolor (x2, ZERO);
	var O = fill (x8, ZERO, x9);
	return O;
};
export var solve_c0f76784 = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = colorfilter (x1, ZERO);
	var x3 = sfilter (x2, square);
	var x4 = sizefilter (x3, ONE);
	var x5 = merge (x4);
	var x6 = argmax (x3, size);
	var x7 = merge (x3);
	var x8 = fill (I, SEVEN, x7);
	var x9 = fill (x8, EIGHT, x6);
	var O = fill (x9, SIX, x5);
	return O;
};
export var solve_1b60fb0c = function (I) {
	var x1 = rot90 (I);
	var x2 = ofcolor (I, ONE);
	var x3 = ofcolor (x1, ONE);
	var x4 = neighbors (ORIGIN);
	var x5 = mapply (neighbors, x4);
	var x6 = lbind (shift, x3);
	var x7 = apply (x6, x5);
	var x8 = lbind (intersection, x2);
	var x9 = argmax (x7, x8);
	var O = underfill (I, TWO, x9);
	return O;
};
export var solve_ddf7fa4f = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = sizefilter (x1, ONE);
	var x3 = colorfilter (x1, FIVE);
	var x4 = product (x2, x3);
	var x5 = fork (vmatching, first, last);
	var x6 = sfilter (x4, x5);
	var x7 = compose (color, first);
	var x8 = fork (recolor, x7, last);
	var x9 = mapply (x8, x6);
	var O = paint (I, x9);
	return O;
};
export var solve_47c1f68c = function (I) {
	var x1 = leastcolor (I);
	var x2 = vmirror (I);
	var x3 = objects (I, T, T, T);
	var x4 = merge (x3);
	var x5 = mostcolor (x4);
	var x6 = cellwise (I, x2, x1);
	var x7 = hmirror (x6);
	var x8 = cellwise (x6, x7, x1);
	var x9 = compress (x8);
	var O = py_replace (x9, x1, x5);
	return O;
};
export var solve_6c434453 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = sizefilter (x1, EIGHT);
	var x3 = dneighbors (UNITY);
	var x4 = insert (UNITY, x3);
	var x5 = merge (x2);
	var x6 = cover (I, x5);
	var x7 = apply (ulcorner, x2);
	var x8 = lbind (shift, x4);
	var x9 = mapply (x8, x7);
	var O = fill (x6, TWO, x9);
	return O;
};
export var solve_23581191 = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = fork (combine, vfrontier, hfrontier);
	var x3 = compose (x2, center);
	var x4 = fork (recolor, color, x3);
	var x5 = mapply (x4, x1);
	var x6 = paint (I, x5);
	var x7 = fork (intersection, first, last);
	var x8 = apply (x3, x1);
	var x9 = x7 (x8);
	var O = fill (x6, TWO, x9);
	return O;
};
export var solve_c8cbb738 = function (I) {
	var x1 = mostcolor (I);
	var x2 = fgpartition (I);
	var x3 = valmax (x2, shape);
	var x4 = canvas (x1, x3);
	var x5 = apply (normalize, x2);
	var x6 = lbind (subtract, x3);
	var x7 = chain (halve, x6, shape);
	var x8 = fork (shift, identity, x7);
	var x9 = mapply (x8, x5);
	var O = paint (x4, x9);
	return O;
};
export var solve_3eda0437 = function (I) {
	var x1 = interval (TWO, TEN, ONE);
	var x2 = prapply (astuple, x1, x1);
	var x3 = lbind (canvas, ZERO);
	var x4 = lbind (occurrences, I);
	var x5 = lbind (lbind, shift);
	var x6 = fork (apply, x5, x4);
	var x7 = chain (x6, asobject, x3);
	var x8 = mapply (x7, x2);
	var x9 = argmax (x8, size);
	var O = fill (I, SIX, x9);
	return O;
};
export var solve_dc0a314f = function (I) {
	var x1 = ofcolor (I, THREE);
	var x2 = py_replace (I, THREE, ZERO);
	var x3 = dmirror (x2);
	var x4 = papply (pair, x2, x3);
	var x5 = lbind (apply, maximum);
	var x6 = apply (x5, x4);
	var x7 = cmirror (x6);
	var x8 = papply (pair, x6, x7);
	var x9 = apply (x5, x8);
	var O = subgrid (x1, x9);
	return O;
};
export var solve_d4469b4b = function (I) {
	var x1 = palette (I);
	var x2 = other (x1, ZERO);
	var x3 = equality (x2, ONE);
	var x4 = equality (x2, TWO);
	var x5 = branch (x3, UNITY, TWO_BY_TWO);
	var x6 = branch (x4, RIGHT, x5);
	var x7 = fork (combine, vfrontier, hfrontier);
	var x8 = x7 (x6);
	var x9 = canvas (ZERO, THREE_BY_THREE);
	var O = fill (x9, FIVE, x8);
	return O;
};
export var solve_6ecd11f4 = function (I) {
	var x1 = objects (I, F, T, T);
	var x2 = argmax (x1, size);
	var x3 = argmin (x1, size);
	var x4 = subgrid (x2, I);
	var x5 = subgrid (x3, I);
	var x6 = width (x4);
	var x7 = width (x5);
	var x8 = divide (x6, x7);
	var x9 = downscale (x4, x8);
	var x10 = ofcolor (x9, ZERO);
	var O = fill (x5, ZERO, x10);
	return O;
};
export var solve_760b3cac = function (I) {
	var x1 = ofcolor (I, FOUR);
	var x2 = ofcolor (I, EIGHT);
	var x3 = ulcorner (x1);
	var x4 = index (I, x3);
	var x5 = equality (x4, FOUR);
	var x6 = branch (x5, NEG_ONE, ONE);
	var x7 = multiply (x6, THREE);
	var x8 = tojvec (x7);
	var x9 = vmirror (x2);
	var x10 = shift (x9, x8);
	var O = fill (I, EIGHT, x10);
	return O;
};
export var solve_c444b776 = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = colorfilter (x1, ZERO);
	var x3 = argmin (x2, size);
	var x4 = backdrop (x3);
	var x5 = toobject (x4, I);
	var x6 = normalize (x5);
	var x7 = lbind (shift, x6);
	var x8 = compose (x7, ulcorner);
	var x9 = remove (x3, x2);
	var x10 = mapply (x8, x9);
	var O = paint (I, x10);
	return O;
};
export var solve_d4a91cb9 = function (I) {
	var x1 = ofcolor (I, EIGHT);
	var x2 = ofcolor (I, TWO);
	var x3 = first (x1);
	var x4 = first (x2);
	var x5 = last (x3);
	var x6 = first (x4);
	var x7 = astuple (x6, x5);
	var x8 = connect (x7, x3);
	var x9 = connect (x7, x4);
	var x10 = combine (x8, x9);
	var O = underfill (I, FOUR, x10);
	return O;
};
export var solve_eb281b96 = function (I) {
	var x1 = height (I);
	var x2 = width (I);
	var x3 = decrement (x1);
	var x4 = astuple (x3, x2);
	var x5 = crop (I, ORIGIN, x4);
	var x6 = hmirror (x5);
	var x7 = vconcat (I, x6);
	var x8 = double (x3);
	var x9 = astuple (x8, x2);
	var x10 = crop (x7, DOWN, x9);
	var O = vconcat (x7, x10);
	return O;
};
export var solve_ff28f65a = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = colorfilter (x1, TWO);
	var x3 = size (x2);
	var x4 = double (x3);
	var x5 = interval (ZERO, x4, TWO);
	var x6 = apply (tojvec, x5);
	var x7 = astuple (ONE, NINE);
	var x8 = canvas (ZERO, x7);
	var x9 = fill (x8, ONE, x6);
	var x10 = hsplit (x9, THREE);
	var O = merge (x10);
	return O;
};
export var solve_7e0986d6 = function (I) {
	var x1 = leastcolor (I);
	var x2 = ofcolor (I, x1);
	var x3 = py_replace (I, x1, ZERO);
	var x4 = leastcolor (x3);
	var x5 = rbind (colorcount, x4);
	var x6 = rbind (greater, ONE);
	var x7 = compose (x6, x5);
	var x8 = rbind (toobject, x3);
	var x9 = chain (x7, x8, dneighbors);
	var x10 = sfilter (x2, x9);
	var O = fill (x3, x4, x10);
	return O;
};
export var solve_09629e4f = function (I) {
	var x1 = objects (I, F, T, T);
	var x2 = rbind (subgrid, I);
	var x3 = apply (x2, x1);
	var x4 = argmin (x3, numcolors);
	var x5 = upscale (x4, FOUR);
	var x6 = ofcolor (I, FIVE);
	var x7 = shift (x6, UNITY);
	var x8 = fill (x5, FIVE, x7);
	var x9 = shape (x8);
	var x10 = decrement (x9);
	var O = crop (x8, UNITY, x10);
	return O;
};
export var solve_a85d4709 = function (I) {
	var x1 = ofcolor (I, FIVE);
	var x2 = lbind (matcher, last);
	var x3 = lbind (sfilter, x1);
	var x4 = lbind (mapply, hfrontier);
	var x5 = chain (x4, x3, x2);
	var x6 = x5 (ZERO);
	var x7 = x5 (TWO);
	var x8 = x5 (ONE);
	var x9 = fill (I, TWO, x6);
	var x10 = fill (x9, THREE, x7);
	var O = fill (x10, FOUR, x8);
	return O;
};
export var solve_feca6190 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = size (x1);
	var x3 = multiply (x2, FIVE);
	var x4 = astuple (x3, x3);
	var x5 = canvas (ZERO, x4);
	var x6 = rbind (shoot, UNITY);
	var x7 = compose (x6, center);
	var x8 = fork (recolor, color, x7);
	var x9 = mapply (x8, x1);
	var x10 = paint (x5, x9);
	var O = hmirror (x10);
	return O;
};
export var solve_a68b268e = function (I) {
	var x1 = tophalf (I);
	var x2 = bottomhalf (I);
	var x3 = lefthalf (x1);
	var x4 = righthalf (x1);
	var x5 = lefthalf (x2);
	var x6 = righthalf (x2);
	var x7 = ofcolor (x4, FOUR);
	var x8 = ofcolor (x3, SEVEN);
	var x9 = ofcolor (x5, EIGHT);
	var x10 = fill (x6, EIGHT, x9);
	var x11 = fill (x10, FOUR, x7);
	var O = fill (x11, SEVEN, x8);
	return O;
};
export var solve_beb8660c = function (I) {
	var x1 = shape (I);
	var x2 = objects (I, T, F, T);
	var x3 = compose (invert, size);
	var x4 = order (x2, x3);
	var x5 = apply (normalize, x4);
	var x6 = size (x5);
	var x7 = interval (ZERO, x6, ONE);
	var x8 = apply (toivec, x7);
	var x9 = mpapply (shift, x5, x8);
	var x10 = canvas (ZERO, x1);
	var x11 = paint (x10, x9);
	var O = rot180 (x11);
	return O;
};
export var solve_913fb3ed = function (I) {
	var x1 = lbind (ofcolor, I);
	var x2 = lbind (mapply, neighbors);
	var x3 = chain (x2, x1, last);
	var x4 = fork (recolor, first, x3);
	var x5 = astuple (SIX, THREE);
	var x6 = astuple (FOUR, EIGHT);
	var x7 = astuple (ONE, TWO);
	var x8 = initset (x5);
	var x9 = insert (x6, x8);
	var x10 = insert (x7, x9);
	var x11 = mapply (x4, x10);
	var O = paint (I, x11);
	return O;
};
export var solve_0962bcdd = function (I) {
	var x1 = leastcolor (I);
	var x2 = py_replace (I, ZERO, x1);
	var x3 = leastcolor (x2);
	var x4 = ofcolor (I, x3);
	var x5 = mapply (dneighbors, x4);
	var x6 = fill (I, x3, x5);
	var x7 = objects (x6, F, T, T);
	var x8 = fork (connect, ulcorner, lrcorner);
	var x9 = fork (connect, llcorner, urcorner);
	var x10 = fork (combine, x8, x9);
	var x11 = mapply (x10, x7);
	var O = fill (x6, x1, x11);
	return O;
};
export var solve_3631a71a = function (I) {
	var x1 = shape (I);
	var x2 = py_replace (I, NINE, ZERO);
	var x3 = lbind (apply, maximum);
	var x4 = dmirror (x2);
	var x5 = papply (pair, x2, x4);
	var x6 = apply (x3, x5);
	var x7 = subtract (x1, TWO_BY_TWO);
	var x8 = crop (x6, TWO_BY_TWO, x7);
	var x9 = vmirror (x8);
	var x10 = objects (x9, T, F, T);
	var x11 = merge (x10);
	var x12 = shift (x11, TWO_BY_TWO);
	var O = paint (x6, x12);
	return O;
};
export var solve_05269061 = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = neighbors (ORIGIN);
	var x3 = mapply (neighbors, x2);
	var x4 = rbind (multiply, THREE);
	var x5 = apply (x4, x3);
	var x6 = merge (x1);
	var x7 = lbind (shift, x6);
	var x8 = mapply (x7, x5);
	var x9 = shift (x8, UP_RIGHT);
	var x10 = shift (x8, DOWN_LEFT);
	var x11 = paint (I, x8);
	var x12 = paint (x11, x9);
	var O = paint (x12, x10);
	return O;
};
export var solve_95990924 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = apply (ulcorner, x1);
	var x3 = apply (urcorner, x1);
	var x4 = apply (llcorner, x1);
	var x5 = apply (lrcorner, x1);
	var x6 = shift (x2, NEG_UNITY);
	var x7 = shift (x3, UP_RIGHT);
	var x8 = shift (x4, DOWN_LEFT);
	var x9 = shift (x5, UNITY);
	var x10 = fill (I, ONE, x6);
	var x11 = fill (x10, TWO, x7);
	var x12 = fill (x11, THREE, x8);
	var O = fill (x12, FOUR, x9);
	return O;
};
export var solve_e509e548 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = rbind (subgrid, I);
	var x3 = chain (palette, trim, x2);
	var x4 = lbind (contained, THREE);
	var x5 = compose (x4, x3);
	var x6 = fork (add, height, width);
	var x7 = compose (decrement, x6);
	var x8 = fork (equality, size, x7);
	var x9 = mfilter (x1, x5);
	var x10 = mfilter (x1, x8);
	var x11 = py_replace (I, THREE, SIX);
	var x12 = fill (x11, TWO, x9);
	var O = fill (x12, ONE, x10);
	return O;
};
export var solve_d43fd935 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = ofcolor (I, THREE);
	var x3 = sizefilter (x1, ONE);
	var x4 = rbind (vmatching, x2);
	var x5 = rbind (hmatching, x2);
	var x6 = fork (either, x4, x5);
	var x7 = sfilter (x3, x6);
	var x8 = rbind (gravitate, x2);
	var x9 = fork (add, center, x8);
	var x10 = fork (connect, center, x9);
	var x11 = fork (recolor, color, x10);
	var x12 = mapply (x11, x7);
	var O = paint (I, x12);
	return O;
};
export var solve_db3e9e38 = function (I) {
	var x1 = ofcolor (I, SEVEN);
	var x2 = lrcorner (x1);
	var x3 = shoot (x2, UP_RIGHT);
	var x4 = shoot (x2, NEG_UNITY);
	var x5 = combine (x3, x4);
	var x6 = rbind (shoot, UP);
	var x7 = mapply (x6, x5);
	var x8 = last (x2);
	var x9 = rbind (subtract, x8);
	var x10 = chain (even, x9, last);
	var x11 = fill (I, EIGHT, x7);
	var x12 = sfilter (x7, x10);
	var O = fill (x11, SEVEN, x12);
	return O;
};
export var solve_e73095fd = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = colorfilter (x1, ZERO);
	var x3 = fork (equality, toindices, backdrop);
	var x4 = sfilter (x2, x3);
	var x5 = lbind (mapply, dneighbors);
	var x6 = chain (x5, corners, outbox);
	var x7 = fork (difference, x6, outbox);
	var x8 = ofcolor (I, FIVE);
	var x9 = rbind (intersection, x8);
	var x10 = matcher (size, ZERO);
	var x11 = chain (x10, x9, x7);
	var x12 = mfilter (x4, x11);
	var O = fill (I, FOUR, x12);
	return O;
};
export var solve_1bfc4729 = function (I) {
	var x1 = asindices (I);
	var x2 = tophalf (I);
	var x3 = bottomhalf (I);
	var x4 = leastcolor (x2);
	var x5 = leastcolor (x3);
	var x6 = ofcolor (x2, x4);
	var x7 = first (x6);
	var x8 = hfrontier (x7);
	var x9 = box (x1);
	var x10 = combine (x8, x9);
	var x11 = fill (x2, x4, x10);
	var x12 = hmirror (x11);
	var x13 = py_replace (x12, x4, x5);
	var O = vconcat (x11, x13);
	return O;
};
export var solve_93b581b8 = function (I) {
	var x1 = objects (I, F, F, T);
	var x2 = first (x1);
	var x3 = dmirror (x2);
	var x4 = cmirror (x3);
	var x5 = upscale (x4, THREE);
	var x6 = astuple (NEG_TWO, NEG_TWO);
	var x7 = shift (x5, x6);
	var x8 = underpaint (I, x7);
	var x9 = toindices (x2);
	var x10 = mapply (vfrontier, x9);
	var x11 = mapply (hfrontier, x9);
	var x12 = combine (x10, x11);
	var x13 = fill (x8, ZERO, x12);
	var O = paint (x13, x2);
	return O;
};
export var solve_9edfc990 = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = colorfilter (x1, ZERO);
	var x3 = ofcolor (I, ONE);
	var x4 = rbind (adjacent, x3);
	var x5 = mfilter (x2, x4);
	var x6 = recolor (ONE, x5);
	var x7 = paint (I, x6);
	var x8 = add (NINE, FOUR);
	var x9 = astuple (SIX, x8);
	var x10 = initset (x9);
	var x11 = fill (x7, ZERO, x10);
	var x12 = index (x7, x9);
	var x13 = equality (x12, ONE);
	var O = branch (x13, x11, x7);
	return O;
};
export var solve_a65b410d = function (I) {
	var x1 = ofcolor (I, TWO);
	var x2 = urcorner (x1);
	var x3 = add (x2, UP_RIGHT);
	var x4 = add (x2, DOWN_LEFT);
	var x5 = shoot (x3, UP_RIGHT);
	var x6 = shoot (x4, DOWN_LEFT);
	var x7 = fill (I, THREE, x5);
	var x8 = fill (x7, ONE, x6);
	var x9 = objects (x8, T, F, T);
	var x10 = rbind (shoot, LEFT);
	var x11 = compose (x10, urcorner);
	var x12 = fork (recolor, color, x11);
	var x13 = mapply (x12, x9);
	var O = paint (x8, x13);
	return O;
};
export var solve_7447852a = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = colorfilter (x1, ZERO);
	var x3 = compose (last, center);
	var x4 = order (x2, x3);
	var x5 = size (x4);
	var x6 = interval (ZERO, x5, THREE);
	var x7 = rbind (contained, x6);
	var x8 = compose (x7, last);
	var x9 = interval (ZERO, x5, ONE);
	var x10 = pair (x4, x9);
	var x11 = sfilter (x10, x8);
	var x12 = mapply (first, x11);
	var x13 = recolor (FOUR, x12);
	var O = paint (I, x13);
	return O;
};
export var solve_97999447 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = apply (toindices, x1);
	var x3 = rbind (shoot, RIGHT);
	var x4 = compose (x3, center);
	var x5 = fork (recolor, color, x4);
	var x6 = mapply (x5, x1);
	var x7 = paint (I, x6);
	var x8 = interval (ZERO, FIVE, ONE);
	var x9 = apply (double, x8);
	var x10 = apply (increment, x9);
	var x11 = apply (tojvec, x10);
	var x12 = prapply (shift, x2, x11);
	var x13 = merge (x12);
	var O = fill (x7, FIVE, x13);
	return O;
};
export var solve_91714a58 = function (I) {
	var x1 = shape (I);
	var x2 = asindices (I);
	var x3 = objects (I, T, F, T);
	var x4 = argmax (x3, size);
	var x5 = mostcolor (x4);
	var x6 = canvas (ZERO, x1);
	var x7 = paint (x6, x4);
	var x8 = rbind (toobject, x7);
	var x9 = rbind (colorcount, x5);
	var x10 = chain (x9, x8, neighbors);
	var x11 = lbind (greater, THREE);
	var x12 = compose (x11, x10);
	var x13 = sfilter (x2, x12);
	var O = fill (x7, ZERO, x13);
	return O;
};
export var solve_a61ba2ce = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = lbind (index, I);
	var x3 = matcher (x2, ZERO);
	var x4 = lbind (extract, x1);
	var x5 = rbind (subgrid, I);
	var x6 = lbind (compose, x3);
	var x7 = chain (x5, x4, x6);
	var x8 = x7 (ulcorner);
	var x9 = x7 (urcorner);
	var x10 = x7 (llcorner);
	var x11 = x7 (lrcorner);
	var x12 = hconcat (x11, x10);
	var x13 = hconcat (x9, x8);
	var O = vconcat (x12, x13);
	return O;
};
export var solve_8e1813be = function (I) {
	var x1 = py_replace (I, FIVE, ZERO);
	var x2 = objects (x1, T, T, T);
	var x3 = first (x2);
	var x4 = vline (x3);
	var x5 = branch (x4, dmirror, identity);
	var x6 = x5 (x1);
	var x7 = objects (x6, T, T, T);
	var x8 = order (x7, uppermost);
	var x9 = apply (color, x8);
	var x10 = dedupe (x9);
	var x11 = size (x10);
	var x12 = rbind (repeat, x11);
	var x13 = apply (x12, x10);
	var O = x5 (x13);
	return O;
};
export var solve_bc1d5164 = function (I) {
	var x1 = leastcolor (I);
	var x2 = crop (I, ORIGIN, THREE_BY_THREE);
	var x3 = crop (I, TWO_BY_ZERO, THREE_BY_THREE);
	var x4 = tojvec (FOUR);
	var x5 = crop (I, x4, THREE_BY_THREE);
	var x6 = astuple (TWO, FOUR);
	var x7 = crop (I, x6, THREE_BY_THREE);
	var x8 = canvas (ZERO, THREE_BY_THREE);
	var x9 = rbind (ofcolor, x1);
	var x10 = astuple (x2, x3);
	var x11 = astuple (x5, x7);
	var x12 = combine (x10, x11);
	var x13 = mapply (x9, x12);
	var O = fill (x8, x1, x13);
	return O;
};
export var solve_ce602527 = function (I) {
	var x1 = vmirror (I);
	var x2 = fgpartition (x1);
	var x3 = order (x2, size);
	var x4 = last (x3);
	var x5 = remove (x4, x3);
	var x6 = compose (toindices, normalize);
	var x7 = rbind (upscale, TWO);
	var x8 = chain (toindices, x7, normalize);
	var x9 = x6 (x4);
	var x10 = rbind (intersection, x9);
	var x11 = chain (size, x10, x8);
	var x12 = argmax (x5, x11);
	var x13 = subgrid (x12, x1);
	var O = vmirror (x13);
	return O;
};
export var solve_5c2c9af4 = function (I) {
	var x1 = leastcolor (I);
	var x2 = ofcolor (I, x1);
	var x3 = center (x2);
	var x4 = ulcorner (x2);
	var x5 = subtract (x3, x4);
	var x6 = multiply (NEG_ONE, NINE);
	var x7 = interval (ZERO, NINE, ONE);
	var x8 = interval (ZERO, x6, NEG_ONE);
	var x9 = lbind (multiply, x5);
	var x10 = apply (x9, x7);
	var x11 = apply (x9, x8);
	var x12 = pair (x10, x11);
	var x13 = mapply (box, x12);
	var x14 = shift (x13, x3);
	var O = fill (I, x1, x14);
	return O;
};
export var solve_75b8110e = function (I) {
	var x1 = lefthalf (I);
	var x2 = righthalf (I);
	var x3 = tophalf (x1);
	var x4 = bottomhalf (x1);
	var x5 = tophalf (x2);
	var x6 = bottomhalf (x2);
	var x7 = rbind (ofcolor, ZERO);
	var x8 = fork (difference, asindices, x7);
	var x9 = fork (toobject, x8, identity);
	var x10 = x9 (x5);
	var x11 = x9 (x4);
	var x12 = x9 (x6);
	var x13 = paint (x3, x12);
	var x14 = paint (x13, x11);
	var O = paint (x14, x10);
	return O;
};
export var solve_941d9a10 = function (I) {
	var x1 = shape (I);
	var x2 = objects (I, T, F, F);
	var x3 = colorfilter (x2, ZERO);
	var x4 = apply (toindices, x3);
	var x5 = lbind (lbind, contained);
	var x6 = lbind (extract, x4);
	var x7 = compose (x6, x5);
	var x8 = decrement (x1);
	var x9 = astuple (FIVE, FIVE);
	var x10 = x7 (ORIGIN);
	var x11 = x7 (x8);
	var x12 = x7 (x9);
	var x13 = fill (I, ONE, x10);
	var x14 = fill (x13, THREE, x11);
	var O = fill (x14, TWO, x12);
	return O;
};
export var solve_c3f564a4 = function (I) {
	var x1 = asindices (I);
	var x2 = dmirror (I);
	var x3 = invert (NINE);
	var x4 = papply (pair, I, x2);
	var x5 = lbind (apply, maximum);
	var x6 = apply (x5, x4);
	var x7 = ofcolor (x6, ZERO);
	var x8 = difference (x1, x7);
	var x9 = toobject (x8, x6);
	var x10 = interval (x3, NINE, ONE);
	var x11 = interval (NINE, x3, NEG_ONE);
	var x12 = pair (x10, x11);
	var x13 = lbind (shift, x9);
	var x14 = mapply (x13, x12);
	var O = paint (x6, x14);
	return O;
};
export var solve_1a07d186 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = sizefilter (x1, ONE);
	var x3 = difference (x1, x2);
	var x4 = apply (color, x3);
	var x5 = rbind (contained, x4);
	var x6 = compose (x5, color);
	var x7 = sfilter (x2, x6);
	var x8 = lbind (colorfilter, x3);
	var x9 = chain (first, x8, color);
	var x10 = fork (gravitate, identity, x9);
	var x11 = fork (shift, identity, x10);
	var x12 = mapply (x11, x7);
	var x13 = merge (x2);
	var x14 = cover (I, x13);
	var O = paint (x14, x12);
	return O;
};
export var solve_d687bc17 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = sizefilter (x1, ONE);
	var x3 = difference (x1, x2);
	var x4 = apply (color, x3);
	var x5 = rbind (contained, x4);
	var x6 = compose (x5, color);
	var x7 = sfilter (x2, x6);
	var x8 = lbind (colorfilter, x3);
	var x9 = chain (first, x8, color);
	var x10 = fork (gravitate, identity, x9);
	var x11 = fork (shift, identity, x10);
	var x12 = merge (x2);
	var x13 = mapply (x11, x7);
	var x14 = cover (I, x12);
	var O = paint (x14, x13);
	return O;
};
export var solve_9af7a82c = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = order (x1, size);
	var x3 = valmax (x1, size);
	var x4 = rbind (astuple, ONE);
	var x5 = lbind (subtract, x3);
	var x6 = compose (x4, size);
	var x7 = chain (x4, x5, size);
	var x8 = fork (canvas, color, x6);
	var x9 = lbind (canvas, ZERO);
	var x10 = compose (x9, x7);
	var x11 = fork (vconcat, x8, x10);
	var x12 = compose (cmirror, x11);
	var x13 = apply (x12, x2);
	var x14 = merge (x13);
	var O = cmirror (x14);
	return O;
};
export var solve_6e19193c = function (I) {
	var x1 = leastcolor (I);
	var x2 = objects (I, T, F, T);
	var x3 = rbind (toobject, I);
	var x4 = compose (first, delta);
	var x5 = rbind (colorcount, x1);
	var x6 = matcher (x5, TWO);
	var x7 = chain (x6, x3, dneighbors);
	var x8 = rbind (sfilter, x7);
	var x9 = chain (first, x8, toindices);
	var x10 = fork (subtract, x4, x9);
	var x11 = fork (shoot, x4, x10);
	var x12 = mapply (x11, x2);
	var x13 = fill (I, x1, x12);
	var x14 = mapply (delta, x2);
	var O = fill (x13, ZERO, x14);
	return O;
};
export var solve_ef135b50 = function (I) {
	var x1 = ofcolor (I, TWO);
	var x2 = ofcolor (I, ZERO);
	var x3 = product (x1, x1);
	var x4 = power (first, TWO);
	var x5 = compose (first, last);
	var x6 = fork (equality, x4, x5);
	var x7 = sfilter (x3, x6);
	var x8 = fork (connect, first, last);
	var x9 = mapply (x8, x7);
	var x10 = intersection (x9, x2);
	var x11 = fill (I, NINE, x10);
	var x12 = trim (x11);
	var x13 = asobject (x12);
	var x14 = shift (x13, UNITY);
	var O = paint (I, x14);
	return O;
};
export var solve_cbded52d = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = sizefilter (x1, ONE);
	var x3 = product (x2, x2);
	var x4 = fork (vmatching, first, last);
	var x5 = fork (hmatching, first, last);
	var x6 = fork (either, x4, x5);
	var x7 = sfilter (x3, x6);
	var x8 = compose (center, first);
	var x9 = compose (center, last);
	var x10 = fork (connect, x8, x9);
	var x11 = chain (initset, center, x10);
	var x12 = compose (color, first);
	var x13 = fork (recolor, x12, x11);
	var x14 = mapply (x13, x7);
	var O = paint (I, x14);
	return O;
};
export var solve_8a004b2b = function (I) {
	var x1 = objects (I, F, T, T);
	var x2 = ofcolor (I, FOUR);
	var x3 = subgrid (x2, I);
	var x4 = argmax (x1, lowermost);
	var x5 = normalize (x4);
	var x6 = py_replace (x3, FOUR, ZERO);
	var x7 = objects (x6, T, F, T);
	var x8 = merge (x7);
	var x9 = width (x8);
	var x10 = ulcorner (x8);
	var x11 = width (x4);
	var x12 = divide (x9, x11);
	var x13 = upscale (x5, x12);
	var x14 = shift (x13, x10);
	var O = paint (x3, x14);
	return O;
};
export var solve_e26a3af2 = function (I) {
	var x1 = rot90 (I);
	var x2 = apply (mostcommon, I);
	var x3 = apply (mostcommon, x1);
	var x4 = repeat (x2, ONE);
	var x5 = repeat (x3, ONE);
	var x6 = compose (size, dedupe);
	var x7 = x6 (x2);
	var x8 = x6 (x3);
	var x9 = greater (x8, x7);
	var x10 = branch (x9, height, width);
	var x11 = x10 (I);
	var x12 = rot90 (x4);
	var x13 = branch (x9, x5, x12);
	var x14 = branch (x9, vupscale, hupscale);
	var O = x14 (x13, x11);
	return O;
};
export var solve_6cf79266 = function (I) {
	var x1 = ofcolor (I, ZERO);
	var x2 = astuple (ZERO, ORIGIN);
	var x3 = initset (x2);
	var x4 = upscale (x3, THREE);
	var x5 = toindices (x4);
	var x6 = lbind (shift, x5);
	var x7 = rbind (difference, x1);
	var x8 = chain (size, x7, x6);
	var x9 = matcher (x8, ZERO);
	var x10 = lbind (add, NEG_UNITY);
	var x11 = chain (flip, x9, x10);
	var x12 = fork (both, x9, x11);
	var x13 = sfilter (x1, x12);
	var x14 = mapply (x6, x13);
	var O = fill (I, ONE, x14);
	return O;
};
export var solve_a87f7484 = function (I) {
	var x1 = palette (I);
	var x2 = dmirror (I);
	var x3 = portrait (I);
	var x4 = branch (x3, x2, I);
	var x5 = size (x1);
	var x6 = decrement (x5);
	var x7 = hsplit (x4, x6);
	var x8 = rbind (ofcolor, ZERO);
	var x9 = apply (x8, x7);
	var x10 = mostcommon (x9);
	var x11 = matcher (x8, x10);
	var x12 = compose (flip, x11);
	var x13 = extract (x7, x12);
	var x14 = dmirror (x13);
	var O = branch (x3, x14, x13);
	return O;
};
export var solve_4093f84a = function (I) {
	var x1 = leastcolor (I);
	var x2 = py_replace (I, x1, FIVE);
	var x3 = rot270 (x2);
	var x4 = ofcolor (I, FIVE);
	var x5 = portrait (x4);
	var x6 = branch (x5, x2, x3);
	var x7 = lefthalf (x6);
	var x8 = righthalf (x6);
	var x9 = rbind (order, identity);
	var x10 = rbind (order, invert);
	var x11 = apply (x9, x7);
	var x12 = apply (x10, x8);
	var x13 = hconcat (x11, x12);
	var x14 = rot90 (x13);
	var O = branch (x5, x13, x14);
	return O;
};
export var solve_ba26e723 = function (I) {
	var x1 = width (I);
	var x2 = hsplit (I, x1);
	var x3 = interval (ZERO, x1, ONE);
	var x4 = rbind (divide, THREE);
	var x5 = rbind (multiply, THREE);
	var x6 = compose (x5, x4);
	var x7 = fork (equality, identity, x6);
	var x8 = apply (x7, x3);
	var x9 = rbind (ofcolor, FOUR);
	var x10 = apply (x9, x2);
	var x11 = apply (tojvec, x3);
	var x12 = papply (shift, x10, x11);
	var x13 = pair (x8, x12);
	var x14 = sfilter (x13, first);
	var x15 = mapply (last, x14);
	var O = fill (I, SIX, x15);
	return O;
};
export var solve_4612dd53 = function (I) {
	var x1 = ofcolor (I, ONE);
	var x2 = box (x1);
	var x3 = fill (I, TWO, x2);
	var x4 = subgrid (x1, x3);
	var x5 = ofcolor (x4, ONE);
	var x6 = mapply (vfrontier, x5);
	var x7 = mapply (hfrontier, x5);
	var x8 = size (x6);
	var x9 = size (x7);
	var x10 = greater (x8, x9);
	var x11 = branch (x10, x7, x6);
	var x12 = fill (x4, TWO, x11);
	var x13 = ofcolor (x12, TWO);
	var x14 = ulcorner (x1);
	var x15 = shift (x13, x14);
	var O = underfill (I, TWO, x15);
	return O;
};
export var solve_29c11459 = function (I) {
	var x1 = lefthalf (I);
	var x2 = righthalf (I);
	var x3 = objects (x2, T, F, T);
	var x4 = objects (x1, T, F, T);
	var x5 = compose (hfrontier, center);
	var x6 = fork (recolor, color, x5);
	var x7 = mapply (x6, x4);
	var x8 = paint (x1, x7);
	var x9 = mapply (x6, x3);
	var x10 = paint (I, x9);
	var x11 = objects (x8, T, F, T);
	var x12 = apply (urcorner, x11);
	var x13 = shift (x12, RIGHT);
	var x14 = merge (x11);
	var x15 = paint (x10, x14);
	var O = fill (x15, FIVE, x13);
	return O;
};
export var solve_963e52fc = function (I) {
	var x1 = width (I);
	var x2 = asobject (I);
	var x3 = hperiod (x2);
	var x4 = height (x2);
	var x5 = astuple (x4, x3);
	var x6 = ulcorner (x2);
	var x7 = crop (I, x6, x5);
	var x8 = rot90 (x7);
	var x9 = double (x1);
	var x10 = divide (x9, x3);
	var x11 = increment (x10);
	var x12 = repeat (x8, x11);
	var x13 = merge (x12);
	var x14 = rot270 (x13);
	var x15 = astuple (x4, x9);
	var O = crop (x14, ORIGIN, x15);
	return O;
};
export var solve_ae3edfdc = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = py_replace (I, THREE, ZERO);
	var x3 = py_replace (x2, SEVEN, ZERO);
	var x4 = lbind (colorfilter, x1);
	var x5 = lbind (rbind, gravitate);
	var x6 = chain (x5, first, x4);
	var x7 = x6 (TWO);
	var x8 = x6 (ONE);
	var x9 = x4 (THREE);
	var x10 = x4 (SEVEN);
	var x11 = fork (shift, identity, x7);
	var x12 = fork (shift, identity, x8);
	var x13 = mapply (x11, x9);
	var x14 = mapply (x12, x10);
	var x15 = paint (x3, x13);
	var O = paint (x15, x14);
	return O;
};
export var solve_1f0c79e5 = function (I) {
	var x1 = ofcolor (I, TWO);
	var x2 = py_replace (I, TWO, ZERO);
	var x3 = leastcolor (x2);
	var x4 = ofcolor (x2, x3);
	var x5 = combine (x1, x4);
	var x6 = recolor (x3, x5);
	var x7 = compose (decrement, double);
	var x8 = ulcorner (x5);
	var x9 = invert (x8);
	var x10 = shift (x1, x9);
	var x11 = apply (x7, x10);
	var x12 = interval (ZERO, NINE, ONE);
	var x13 = prapply (multiply, x11, x12);
	var x14 = lbind (shift, x6);
	var x15 = mapply (x14, x13);
	var O = paint (I, x15);
	return O;
};
export var solve_56dc2b01 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = colorfilter (x1, THREE);
	var x3 = first (x2);
	var x4 = ofcolor (I, TWO);
	var x5 = gravitate (x3, x4);
	var x6 = first (x5);
	var x7 = equality (x6, ZERO);
	var x8 = branch (x7, width, height);
	var x9 = x8 (x3);
	var x10 = gravitate (x4, x3);
	var x11 = sign (x10);
	var x12 = multiply (x11, x9);
	var x13 = crement (x12);
	var x14 = recolor (EIGHT, x4);
	var x15 = shift (x14, x13);
	var x16 = paint (I, x15);
	var O = move (x16, x3, x5);
	return O;
};
export var solve_e48d4e1a = function (I) {
	var x1 = shape (I);
	var x2 = ofcolor (I, FIVE);
	var x3 = fill (I, ZERO, x2);
	var x4 = leastcolor (x3);
	var x5 = size (x2);
	var x6 = ofcolor (I, x4);
	var x7 = rbind (toobject, I);
	var x8 = rbind (colorcount, x4);
	var x9 = chain (x8, x7, dneighbors);
	var x10 = matcher (x9, FOUR);
	var x11 = extract (x6, x10);
	var x12 = multiply (DOWN_LEFT, x5);
	var x13 = add (x12, x11);
	var x14 = canvas (ZERO, x1);
	var x15 = fork (combine, vfrontier, hfrontier);
	var x16 = x15 (x13);
	var O = fill (x14, x4, x16);
	return O;
};
export var solve_6773b310 = function (I) {
	var x1 = compress (I);
	var x2 = neighbors (ORIGIN);
	var x3 = insert (ORIGIN, x2);
	var x4 = rbind (multiply, THREE);
	var x5 = apply (x4, x3);
	var x6 = astuple (FOUR, FOUR);
	var x7 = shift (x5, x6);
	var x8 = fork (insert, identity, neighbors);
	var x9 = apply (x8, x7);
	var x10 = rbind (toobject, x1);
	var x11 = apply (x10, x9);
	var x12 = rbind (colorcount, SIX);
	var x13 = matcher (x12, TWO);
	var x14 = mfilter (x11, x13);
	var x15 = fill (x1, ONE, x14);
	var x16 = py_replace (x15, SIX, ZERO);
	var O = downscale (x16, THREE);
	return O;
};
export var solve_780d0b14 = function (I) {
	var x1 = asindices (I);
	var x2 = objects (I, T, T, T);
	var x3 = rbind (greater, TWO);
	var x4 = compose (x3, size);
	var x5 = sfilter (x2, x4);
	var x6 = totuple (x5);
	var x7 = apply (color, x6);
	var x8 = apply (center, x6);
	var x9 = pair (x7, x8);
	var x10 = fill (I, ZERO, x1);
	var x11 = paint (x10, x9);
	var x12 = rbind (greater, ONE);
	var x13 = compose (dedupe, totuple);
	var x14 = chain (x12, size, x13);
	var x15 = sfilter (x11, x14);
	var x16 = rot90 (x15);
	var x17 = sfilter (x16, x14);
	var O = rot270 (x17);
	return O;
};
export var solve_2204b7a8 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = lbind (sfilter, x1);
	var x3 = compose (size, x2);
	var x4 = x3 (vline);
	var x5 = x3 (hline);
	var x6 = greater (x4, x5);
	var x7 = branch (x6, lefthalf, tophalf);
	var x8 = branch (x6, righthalf, bottomhalf);
	var x9 = branch (x6, hconcat, vconcat);
	var x10 = x7 (I);
	var x11 = x8 (I);
	var x12 = index (x10, ORIGIN);
	var x13 = shape (x11);
	var x14 = decrement (x13);
	var x15 = index (x11, x14);
	var x16 = py_replace (x10, THREE, x12);
	var x17 = py_replace (x11, THREE, x15);
	var O = x9 (x16, x17);
	return O;
};
export var solve_d9f24cd1 = function (I) {
	var x1 = ofcolor (I, TWO);
	var x2 = ofcolor (I, FIVE);
	var x3 = prapply (connect, x1, x2);
	var x4 = mfilter (x3, vline);
	var x5 = underfill (I, TWO, x4);
	var x6 = matcher (numcolors, TWO);
	var x7 = objects (x5, F, F, T);
	var x8 = sfilter (x7, x6);
	var x9 = difference (x7, x8);
	var x10 = colorfilter (x9, TWO);
	var x11 = mapply (toindices, x10);
	var x12 = apply (urcorner, x8);
	var x13 = shift (x12, UNITY);
	var x14 = rbind (shoot, UP);
	var x15 = mapply (x14, x13);
	var x16 = fill (x5, TWO, x15);
	var x17 = mapply (vfrontier, x11);
	var O = fill (x16, TWO, x17);
	return O;
};
export var solve_b782dc8a = function (I) {
	var x1 = leastcolor (I);
	var x2 = objects (I, T, F, F);
	var x3 = ofcolor (I, x1);
	var x4 = first (x3);
	var x5 = dneighbors (x4);
	var x6 = toobject (x5, I);
	var x7 = mostcolor (x6);
	var x8 = ofcolor (I, x7);
	var x9 = colorfilter (x2, ZERO);
	var x10 = rbind (adjacent, x8);
	var x11 = mfilter (x9, x10);
	var x12 = toindices (x11);
	var x13 = rbind (manhattan, x3);
	var x14 = chain (even, x13, initset);
	var x15 = sfilter (x12, x14);
	var x16 = difference (x12, x15);
	var x17 = fill (I, x1, x15);
	var O = fill (x17, x7, x16);
	return O;
};
export var solve_673ef223 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = ofcolor (I, EIGHT);
	var x3 = py_replace (I, EIGHT, FOUR);
	var x4 = colorfilter (x1, TWO);
	var x5 = argmin (x1, uppermost);
	var x6 = apply (uppermost, x4);
	var x7 = fork (subtract, maximum, minimum);
	var x8 = x7 (x6);
	var x9 = toivec (x8);
	var x10 = leftmost (x5);
	var x11 = equality (x10, ZERO);
	var x12 = branch (x11, LEFT, RIGHT);
	var x13 = rbind (shoot, x12);
	var x14 = mapply (x13, x2);
	var x15 = underfill (x3, EIGHT, x14);
	var x16 = shift (x2, x9);
	var x17 = mapply (hfrontier, x16);
	var O = underfill (x15, EIGHT, x17);
	return O;
};
export var solve_f5b8619d = function (I) {
	var x1 = leastcolor (I);
	var x2 = width (I);
	var x3 = height (I);
	var x4 = righthalf (I);
	var x5 = halve (x2);
	var x6 = even (x2);
	var x7 = branch (x6, identity, increment);
	var x8 = x7 (x5);
	var x9 = astuple (x3, x8);
	var x10 = crop (I, ORIGIN, x9);
	var x11 = vconcat (x10, x10);
	var x12 = vconcat (x4, x4);
	var x13 = hconcat (x12, x11);
	var x14 = hconcat (x11, x13);
	var x15 = hconcat (x14, x12);
	var x16 = ofcolor (x15, x1);
	var x17 = mapply (vfrontier, x16);
	var O = underfill (x15, EIGHT, x17);
	return O;
};
export var solve_f8c80d96 = function (I) {
	var x1 = leastcolor (I);
	var x2 = objects (I, T, F, F);
	var x3 = colorfilter (x2, x1);
	var x4 = argmax (x3, size);
	var x5 = argmin (x2, width);
	var x6 = size (x5);
	var x7 = equality (x6, ONE);
	var x8 = branch (x7, identity, outbox);
	var x9 = chain (outbox, outbox, x8);
	var x10 = power (x9, TWO);
	var x11 = power (x9, THREE);
	var x12 = x9 (x4);
	var x13 = x10 (x4);
	var x14 = x11 (x4);
	var x15 = fill (I, x1, x12);
	var x16 = fill (x15, x1, x13);
	var x17 = fill (x16, x1, x14);
	var O = py_replace (x17, ZERO, FIVE);
	return O;
};
export var solve_ecdecbb3 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = colorfilter (x1, TWO);
	var x3 = colorfilter (x1, EIGHT);
	var x4 = product (x2, x3);
	var x5 = fork (gravitate, first, last);
	var x6 = compose (crement, x5);
	var x7 = compose (center, first);
	var x8 = fork (add, x7, x6);
	var x9 = fork (connect, x7, x8);
	var x10 = apply (x9, x4);
	var x11 = lbind (greater, EIGHT);
	var x12 = compose (x11, size);
	var x13 = mfilter (x10, x12);
	var x14 = fill (I, TWO, x13);
	var x15 = apply (x8, x4);
	var x16 = intersection (x13, x15);
	var x17 = mapply (neighbors, x16);
	var O = fill (x14, EIGHT, x17);
	return O;
};
export var solve_e5062a87 = function (I) {
	var x1 = ofcolor (I, TWO);
	var x2 = recolor (ZERO, x1);
	var x3 = normalize (x2);
	var x4 = occurrences (I, x2);
	var x5 = lbind (shift, x3);
	var x6 = apply (x5, x4);
	var x7 = astuple (ONE, THREE);
	var x8 = astuple (FIVE, ONE);
	var x9 = astuple (TWO, SIX);
	var x10 = initset (x7);
	var x11 = insert (x8, x10);
	var x12 = insert (x9, x11);
	var x13 = rbind (contained, x12);
	var x14 = chain (flip, x13, ulcorner);
	var x15 = sfilter (x6, x14);
	var x16 = merge (x15);
	var x17 = recolor (TWO, x16);
	var O = paint (I, x17);
	return O;
};
export var solve_a8d7556c = function (I) {
	var x1 = initset (ORIGIN);
	var x2 = recolor (ZERO, x1);
	var x3 = upscale (x2, TWO);
	var x4 = occurrences (I, x3);
	var x5 = lbind (shift, x3);
	var x6 = mapply (x5, x4);
	var x7 = fill (I, TWO, x6);
	var x8 = add (SIX, SIX);
	var x9 = astuple (EIGHT, x8);
	var x10 = index (x7, x9);
	var x11 = equality (x10, TWO);
	var x12 = initset (x9);
	var x13 = add (x9, DOWN);
	var x14 = insert (x13, x12);
	var x15 = toobject (x14, x7);
	var x16 = toobject (x14, I);
	var x17 = branch (x11, x16, x15);
	var O = paint (x7, x17);
	return O;
};
export var solve_4938f0c2 = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = ofcolor (I, TWO);
	var x3 = vmirror (x2);
	var x4 = height (x2);
	var x5 = width (x2);
	var x6 = toivec (x4);
	var x7 = tojvec (x5);
	var x8 = add (x7, ZERO_BY_TWO);
	var x9 = add (x6, TWO_BY_ZERO);
	var x10 = shift (x3, x8);
	var x11 = fill (I, TWO, x10);
	var x12 = ofcolor (x11, TWO);
	var x13 = hmirror (x12);
	var x14 = shift (x13, x9);
	var x15 = fill (x11, TWO, x14);
	var x16 = size (x1);
	var x17 = greater (x16, FOUR);
	var O = branch (x17, I, x15);
	return O;
};
export var solve_834ec97d = function (I) {
	var x1 = asindices (I);
	var x2 = objects (I, T, F, T);
	var x3 = first (x2);
	var x4 = shift (x3, DOWN);
	var x5 = fill (I, ZERO, x3);
	var x6 = paint (x5, x4);
	var x7 = uppermost (x4);
	var x8 = leftmost (x4);
	var x9 = subtract (x8, TEN);
	var x10 = add (x8, TEN);
	var x11 = interval (x9, x10, TWO);
	var x12 = lbind (greater, x7);
	var x13 = compose (x12, first);
	var x14 = rbind (contained, x11);
	var x15 = compose (x14, last);
	var x16 = sfilter (x1, x13);
	var x17 = sfilter (x16, x15);
	var O = fill (x6, FOUR, x17);
	return O;
};
export var solve_846bdb03 = function (I) {
	var x1 = objects (I, F, F, T);
	var x2 = rbind (colorcount, FOUR);
	var x3 = matcher (x2, ZERO);
	var x4 = extract (x1, x3);
	var x5 = remove (x4, x1);
	var x6 = merge (x5);
	var x7 = subgrid (x6, I);
	var x8 = index (x7, DOWN);
	var x9 = subgrid (x4, I);
	var x10 = lefthalf (x9);
	var x11 = palette (x10);
	var x12 = other (x11, ZERO);
	var x13 = equality (x8, x12);
	var x14 = branch (x13, identity, vmirror);
	var x15 = x14 (x4);
	var x16 = normalize (x15);
	var x17 = shift (x16, UNITY);
	var O = paint (x7, x17);
	return O;
};
export var solve_90f3ed37 = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = order (x1, uppermost);
	var x3 = first (x2);
	var x4 = remove (x3, x2);
	var x5 = normalize (x3);
	var x6 = lbind (shift, x5);
	var x7 = compose (x6, ulcorner);
	var x8 = interval (TWO, NEG_ONE, NEG_ONE);
	var x9 = apply (tojvec, x8);
	var x10 = rbind (apply, x9);
	var x11 = lbind (compose, size);
	var x12 = lbind (lbind, intersection);
	var x13 = compose (x11, x12);
	var x14 = lbind (lbind, shift);
	var x15 = chain (x10, x14, x7);
	var x16 = fork (argmax, x15, x13);
	var x17 = mapply (x16, x4);
	var O = underfill (I, ONE, x17);
	return O;
};
export var solve_8403a5d5 = function (I) {
	var x1 = asindices (I);
	var x2 = objects (I, T, F, T);
	var x3 = first (x2);
	var x4 = color (x3);
	var x5 = leftmost (x3);
	var x6 = interval (x5, TEN, TWO);
	var x7 = rbind (contained, x6);
	var x8 = compose (x7, last);
	var x9 = sfilter (x1, x8);
	var x10 = increment (x5);
	var x11 = add (x5, THREE);
	var x12 = interval (x10, TEN, FOUR);
	var x13 = interval (x11, TEN, FOUR);
	var x14 = lbind (astuple, NINE);
	var x15 = apply (tojvec, x12);
	var x16 = apply (x14, x13);
	var x17 = fill (I, x4, x9);
	var x18 = fill (x17, FIVE, x15);
	var O = fill (x18, FIVE, x16);
	return O;
};
export var solve_91413438 = function (I) {
	var x1 = colorcount (I, ZERO);
	var x2 = subtract (NINE, x1);
	var x3 = multiply (x1, THREE);
	var x4 = multiply (x3, x1);
	var x5 = subtract (x4, THREE);
	var x6 = astuple (THREE, x5);
	var x7 = canvas (ZERO, x6);
	var x8 = hconcat (I, x7);
	var x9 = objects (x8, T, T, T);
	var x10 = first (x9);
	var x11 = lbind (shift, x10);
	var x12 = compose (x11, tojvec);
	var x13 = interval (ZERO, x2, ONE);
	var x14 = rbind (multiply, THREE);
	var x15 = apply (x14, x13);
	var x16 = mapply (x12, x15);
	var x17 = paint (x8, x16);
	var x18 = hsplit (x17, x1);
	var O = merge (x18);
	return O;
};
export var solve_539a4f51 = function (I) {
	var x1 = shape (I);
	var x2 = index (I, ORIGIN);
	var x3 = colorcount (I, ZERO);
	var x4 = decrement (x1);
	var x5 = positive (x3);
	var x6 = branch (x5, x4, x1);
	var x7 = crop (I, ORIGIN, x6);
	var x8 = width (x7);
	var x9 = astuple (ONE, x8);
	var x10 = crop (x7, ORIGIN, x9);
	var x11 = vupscale (x10, x8);
	var x12 = dmirror (x11);
	var x13 = hconcat (x7, x11);
	var x14 = hconcat (x12, x7);
	var x15 = vconcat (x13, x14);
	var x16 = asobject (x15);
	var x17 = multiply (UNITY, TEN);
	var x18 = canvas (x2, x17);
	var O = paint (x18, x16);
	return O;
};
export var solve_5daaa586 = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = colorfilter (x1, ZERO);
	var x3 = rbind (bordering, I);
	var x4 = compose (flip, x3);
	var x5 = extract (x2, x4);
	var x6 = outbox (x5);
	var x7 = subgrid (x6, I);
	var x8 = fgpartition (x7);
	var x9 = argmax (x8, size);
	var x10 = color (x9);
	var x11 = toindices (x9);
	var x12 = prapply (connect, x11, x11);
	var x13 = mfilter (x12, vline);
	var x14 = mfilter (x12, hline);
	var x15 = size (x13);
	var x16 = size (x14);
	var x17 = greater (x15, x16);
	var x18 = branch (x17, x13, x14);
	var O = fill (x7, x10, x18);
	return O;
};
export var solve_3bdb4ada = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = totuple (x1);
	var x3 = compose (increment, ulcorner);
	var x4 = compose (decrement, lrcorner);
	var x5 = apply (x3, x2);
	var x6 = apply (x4, x2);
	var x7 = papply (connect, x5, x6);
	var x8 = apply (last, x5);
	var x9 = compose (last, first);
	var x10 = power (last, TWO);
	var x11 = fork (subtract, x9, x10);
	var x12 = compose (even, x11);
	var x13 = lbind (rbind, astuple);
	var x14 = lbind (compose, x12);
	var x15 = compose (x14, x13);
	var x16 = fork (sfilter, first, x15);
	var x17 = pair (x7, x8);
	var x18 = mapply (x16, x17);
	var O = fill (I, ZERO, x18);
	return O;
};
export var solve_ec883f72 = function (I) {
	var x1 = palette (I);
	var x2 = objects (I, T, T, T);
	var x3 = fork (multiply, height, width);
	var x4 = argmax (x2, x3);
	var x5 = color (x4);
	var x6 = remove (ZERO, x1);
	var x7 = other (x6, x5);
	var x8 = lrcorner (x4);
	var x9 = llcorner (x4);
	var x10 = urcorner (x4);
	var x11 = ulcorner (x4);
	var x12 = shoot (x8, UNITY);
	var x13 = shoot (x9, DOWN_LEFT);
	var x14 = shoot (x10, UP_RIGHT);
	var x15 = shoot (x11, NEG_UNITY);
	var x16 = combine (x12, x13);
	var x17 = combine (x14, x15);
	var x18 = combine (x16, x17);
	var O = underfill (I, x7, x18);
	return O;
};
export var solve_2bee17df = function (I) {
	var x1 = height (I);
	var x2 = rot90 (I);
	var x3 = subtract (x1, TWO);
	var x4 = interval (ZERO, x1, ONE);
	var x5 = rbind (colorcount, ZERO);
	var x6 = matcher (x5, x3);
	var x7 = rbind (vsplit, x1);
	var x8 = lbind (apply, x6);
	var x9 = compose (x8, x7);
	var x10 = x9 (I);
	var x11 = pair (x4, x10);
	var x12 = sfilter (x11, last);
	var x13 = mapply (hfrontier, x12);
	var x14 = x9 (x2);
	var x15 = pair (x14, x4);
	var x16 = sfilter (x15, first);
	var x17 = mapply (vfrontier, x16);
	var x18 = astuple (x13, x17);
	var x19 = merge (x18);
	var O = underfill (I, THREE, x19);
	return O;
};
export var solve_e8dc4411 = function (I) {
	var x1 = leastcolor (I);
	var x2 = ofcolor (I, ZERO);
	var x3 = ofcolor (I, x1);
	var x4 = position (x2, x3);
	var x5 = fork (connect, ulcorner, lrcorner);
	var x6 = x5 (x2);
	var x7 = intersection (x2, x6);
	var x8 = equality (x6, x7);
	var x9 = fork (subtract, identity, crement);
	var x10 = fork (add, identity, x9);
	var x11 = branch (x8, identity, x10);
	var x12 = shape (x2);
	var x13 = multiply (x12, x4);
	var x14 = apply (x11, x13);
	var x15 = interval (ONE, FIVE, ONE);
	var x16 = lbind (multiply, x14);
	var x17 = apply (x16, x15);
	var x18 = lbind (shift, x2);
	var x19 = mapply (x18, x17);
	var O = fill (I, x1, x19);
	return O;
};
export var solve_e40b9e2f = function (I) {
	var x1 = objects (I, F, T, T);
	var x2 = neighbors (ORIGIN);
	var x3 = mapply (neighbors, x2);
	var x4 = first (x1);
	var x5 = lbind (intersection, x4);
	var x6 = compose (hmirror, vmirror);
	var x7 = x6 (x4);
	var x8 = lbind (shift, x7);
	var x9 = apply (x8, x3);
	var x10 = argmax (x9, x5);
	var x11 = paint (I, x10);
	var x12 = objects (x11, F, T, T);
	var x13 = first (x12);
	var x14 = compose (size, x5);
	var x15 = compose (vmirror, dmirror);
	var x16 = x15 (x13);
	var x17 = lbind (shift, x16);
	var x18 = apply (x17, x3);
	var x19 = argmax (x18, x14);
	var O = paint (x11, x19);
	return O;
};
export var solve_29623171 = function (I) {
	var x1 = leastcolor (I);
	var x2 = interval (ZERO, NINE, FOUR);
	var x3 = product (x2, x2);
	var x4 = rbind (add, THREE);
	var x5 = rbind (interval, ONE);
	var x6 = fork (x5, identity, x4);
	var x7 = compose (x6, first);
	var x8 = compose (x6, last);
	var x9 = fork (product, x7, x8);
	var x10 = rbind (colorcount, x1);
	var x11 = rbind (toobject, I);
	var x12 = compose (x10, x11);
	var x13 = apply (x9, x3);
	var x14 = valmax (x13, x12);
	var x15 = matcher (x12, x14);
	var x16 = compose (flip, x15);
	var x17 = mfilter (x13, x15);
	var x18 = mfilter (x13, x16);
	var x19 = fill (I, x1, x17);
	var O = fill (x19, ZERO, x18);
	return O;
};
export var solve_a2fd1cf0 = function (I) {
	var x1 = ofcolor (I, TWO);
	var x2 = ofcolor (I, THREE);
	var x3 = uppermost (x1);
	var x4 = leftmost (x1);
	var x5 = uppermost (x2);
	var x6 = leftmost (x2);
	var x7 = astuple (x3, x5);
	var x8 = minimum (x7);
	var x9 = maximum (x7);
	var x10 = astuple (x8, x6);
	var x11 = astuple (x9, x6);
	var x12 = connect (x10, x11);
	var x13 = astuple (x4, x6);
	var x14 = minimum (x13);
	var x15 = maximum (x13);
	var x16 = astuple (x3, x14);
	var x17 = astuple (x3, x15);
	var x18 = connect (x16, x17);
	var x19 = combine (x12, x18);
	var O = underfill (I, EIGHT, x19);
	return O;
};
export var solve_b0c4d837 = function (I) {
	var x1 = ofcolor (I, FIVE);
	var x2 = ofcolor (I, EIGHT);
	var x3 = height (x1);
	var x4 = decrement (x3);
	var x5 = height (x2);
	var x6 = subtract (x4, x5);
	var x7 = astuple (ONE, x6);
	var x8 = canvas (EIGHT, x7);
	var x9 = subtract (SIX, x6);
	var x10 = astuple (ONE, x9);
	var x11 = canvas (ZERO, x10);
	var x12 = hconcat (x8, x11);
	var x13 = hsplit (x12, TWO);
	var x14 = first (x13);
	var x15 = last (x13);
	var x16 = vmirror (x15);
	var x17 = vconcat (x14, x16);
	var x18 = astuple (ONE, THREE);
	var x19 = canvas (ZERO, x18);
	var O = vconcat (x17, x19);
	return O;
};
export var solve_8731374e = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = argmax (x1, size);
	var x3 = subgrid (x2, I);
	var x4 = height (x3);
	var x5 = width (x3);
	var x6 = vsplit (x3, x4);
	var x7 = lbind (greater, FOUR);
	var x8 = compose (x7, numcolors);
	var x9 = sfilter (x6, x8);
	var x10 = merge (x9);
	var x11 = rot90 (x10);
	var x12 = vsplit (x11, x5);
	var x13 = sfilter (x12, x8);
	var x14 = merge (x13);
	var x15 = rot270 (x14);
	var x16 = leastcolor (x15);
	var x17 = ofcolor (x15, x16);
	var x18 = fork (combine, vfrontier, hfrontier);
	var x19 = mapply (x18, x17);
	var O = fill (x15, x16, x19);
	return O;
};
export var solve_272f95fa = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = colorfilter (x1, ZERO);
	var x3 = apply (toindices, x2);
	var x4 = rbind (bordering, I);
	var x5 = compose (flip, x4);
	var x6 = extract (x3, x5);
	var x7 = remove (x6, x3);
	var x8 = lbind (vmatching, x6);
	var x9 = lbind (hmatching, x6);
	var x10 = sfilter (x7, x8);
	var x11 = sfilter (x7, x9);
	var x12 = argmin (x10, uppermost);
	var x13 = argmax (x10, uppermost);
	var x14 = argmin (x11, leftmost);
	var x15 = argmax (x11, leftmost);
	var x16 = fill (I, SIX, x6);
	var x17 = fill (x16, TWO, x12);
	var x18 = fill (x17, ONE, x13);
	var x19 = fill (x18, FOUR, x14);
	var O = fill (x19, THREE, x15);
	return O;
};
export var solve_db93a21d = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = ofcolor (I, NINE);
	var x3 = colorfilter (x1, NINE);
	var x4 = rbind (shoot, DOWN);
	var x5 = mapply (x4, x2);
	var x6 = underfill (I, ONE, x5);
	var x7 = compose (halve, width);
	var x8 = rbind (greater, ONE);
	var x9 = compose (x8, x7);
	var x10 = matcher (x7, THREE);
	var x11 = power (outbox, TWO);
	var x12 = power (outbox, THREE);
	var x13 = mapply (outbox, x3);
	var x14 = sfilter (x3, x9);
	var x15 = sfilter (x3, x10);
	var x16 = mapply (x11, x14);
	var x17 = mapply (x12, x15);
	var x18 = fill (x6, THREE, x13);
	var x19 = fill (x18, THREE, x16);
	var O = fill (x19, THREE, x17);
	return O;
};
export var solve_53b68214 = function (I) {
	var x1 = width (I);
	var x2 = objects (I, T, T, T);
	var x3 = first (x2);
	var x4 = vperiod (x3);
	var x5 = toivec (x4);
	var x6 = interval (ZERO, NINE, ONE);
	var x7 = lbind (multiply, x5);
	var x8 = apply (x7, x6);
	var x9 = lbind (shift, x3);
	var x10 = mapply (x9, x8);
	var x11 = astuple (x1, x1);
	var x12 = portrait (x3);
	var x13 = shape (x3);
	var x14 = add (DOWN, x13);
	var x15 = decrement (x14);
	var x16 = shift (x3, x15);
	var x17 = branch (x12, x10, x16);
	var x18 = canvas (ZERO, x11);
	var x19 = paint (x18, x3);
	var O = paint (x19, x17);
	return O;
};
export var solve_d6ad076f = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = argmin (x1, size);
	var x3 = argmax (x1, size);
	var x4 = vmatching (x2, x3);
	var x5 = branch (x4, DOWN, RIGHT);
	var x6 = branch (x4, uppermost, leftmost);
	var x7 = valmax (x1, x6);
	var x8 = x6 (x2);
	var x9 = equality (x7, x8);
	var x10 = branch (x9, NEG_ONE, ONE);
	var x11 = multiply (x5, x10);
	var x12 = inbox (x2);
	var x13 = rbind (shoot, x11);
	var x14 = mapply (x13, x12);
	var x15 = underfill (I, EIGHT, x14);
	var x16 = objects (x15, T, F, T);
	var x17 = colorfilter (x16, EIGHT);
	var x18 = rbind (bordering, I);
	var x19 = mfilter (x17, x18);
	var O = cover (x15, x19);
	return O;
};
export var solve_6cdd2623 = function (I) {
	var x1 = leastcolor (I);
	var x2 = height (I);
	var x3 = width (I);
	var x4 = objects (I, T, F, T);
	var x5 = merge (x4);
	var x6 = cover (I, x5);
	var x7 = ofcolor (I, x1);
	var x8 = prapply (connect, x7, x7);
	var x9 = merge (x8);
	var x10 = decrement (x2);
	var x11 = decrement (x3);
	var x12 = lbind (greater, x10);
	var x13 = lbind (greater, x11);
	var x14 = fork (both, positive, x12);
	var x15 = compose (x14, first);
	var x16 = fork (both, positive, x13);
	var x17 = compose (x16, last);
	var x18 = fork (both, x15, x17);
	var x19 = sfilter (x9, x18);
	var x20 = fill (x6, x1, x19);
	var O = fill (x20, x1, x7);
	return O;
};
export var solve_a3df8b1e = function (I) {
	var x1 = shape (I);
	var x2 = ofcolor (I, ONE);
	var x3 = first (x2);
	var x4 = shoot (x3, UP_RIGHT);
	var x5 = fill (I, ONE, x4);
	var x6 = ofcolor (x5, ONE);
	var x7 = urcorner (x6);
	var x8 = shoot (x7, NEG_UNITY);
	var x9 = fill (x5, ONE, x8);
	var x10 = objects (x9, T, T, T);
	var x11 = first (x10);
	var x12 = subgrid (x11, x9);
	var x13 = shape (x12);
	var x14 = subtract (x13, DOWN);
	var x15 = crop (x12, DOWN, x14);
	var x16 = vconcat (x15, x15);
	var x17 = vconcat (x16, x16);
	var x18 = vconcat (x17, x17);
	var x19 = hmirror (x18);
	var x20 = crop (x19, ORIGIN, x1);
	var O = hmirror (x20);
	return O;
};
export var solve_8d510a79 = function (I) {
	var x1 = height (I);
	var x2 = halve (x1);
	var x3 = ofcolor (I, ONE);
	var x4 = ofcolor (I, TWO);
	var x5 = ofcolor (I, FIVE);
	var x6 = rbind (gravitate, x5);
	var x7 = compose (x6, initset);
	var x8 = fork (add, identity, x7);
	var x9 = fork (connect, identity, x8);
	var x10 = mapply (x9, x4);
	var x11 = fill (I, TWO, x10);
	var x12 = rbind (greater, x2);
	var x13 = compose (x12, first);
	var x14 = sfilter (x3, x13);
	var x15 = difference (x3, x14);
	var x16 = rbind (shoot, UP);
	var x17 = rbind (shoot, DOWN);
	var x18 = mapply (x16, x15);
	var x19 = mapply (x17, x14);
	var x20 = combine (x18, x19);
	var O = fill (x11, ONE, x20);
	return O;
};
export var solve_cdecee7f = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = astuple (ONE, THREE);
	var x3 = size (x1);
	var x4 = order (x1, leftmost);
	var x5 = apply (color, x4);
	var x6 = rbind (canvas, UNITY);
	var x7 = apply (x6, x5);
	var x8 = merge (x7);
	var x9 = dmirror (x8);
	var x10 = subtract (NINE, x3);
	var x11 = astuple (ONE, x10);
	var x12 = canvas (ZERO, x11);
	var x13 = hconcat (x9, x12);
	var x14 = hsplit (x13, THREE);
	var x15 = merge (x14);
	var x16 = crop (x15, ORIGIN, x2);
	var x17 = crop (x15, DOWN, x2);
	var x18 = crop (x15, TWO_BY_ZERO, x2);
	var x19 = vmirror (x17);
	var x20 = vconcat (x16, x19);
	var O = vconcat (x20, x18);
	return O;
};
export var solve_3345333e = function (I) {
	var x1 = mostcolor (I);
	var x2 = asindices (I);
	var x3 = objects (I, F, T, T);
	var x4 = first (x3);
	var x5 = mostcolor (x4);
	var x6 = matcher (first, x5);
	var x7 = sfilter (x4, x6);
	var x8 = toindices (x7);
	var x9 = ulcorner (x7);
	var x10 = difference (x2, x8);
	var x11 = fill (I, x1, x10);
	var x12 = subgrid (x7, x11);
	var x13 = vmirror (x12);
	var x14 = ofcolor (x13, x5);
	var x15 = last (x9);
	var x16 = even (x15);
	var x17 = invert (x16);
	var x18 = tojvec (x17);
	var x19 = add (x9, x18);
	var x20 = shift (x14, x19);
	var O = fill (x11, x5, x20);
	return O;
};
export var solve_b190f7f5 = function (I) {
	var x1 = dmirror (I);
	var x2 = portrait (I);
	var x3 = branch (x2, x1, I);
	var x4 = lefthalf (x3);
	var x5 = righthalf (x3);
	var x6 = palette (x4);
	var x7 = contained (EIGHT, x6);
	var x8 = branch (x7, x4, x5);
	var x9 = branch (x7, x5, x4);
	var x10 = width (x9);
	var x11 = upscale (x9, x10);
	var x12 = repeat (x8, x10);
	var x13 = merge (x12);
	var x14 = dmirror (x13);
	var x15 = repeat (x14, x10);
	var x16 = merge (x15);
	var x17 = dmirror (x16);
	var x18 = ofcolor (x17, ZERO);
	var x19 = fill (x11, ZERO, x18);
	var x20 = dmirror (x19);
	var O = branch (x2, x20, x19);
	return O;
};
export var solve_caa06a1f = function (I) {
	var x1 = asobject (I);
	var x2 = shape (I);
	var x3 = decrement (x2);
	var x4 = index (I, x3);
	var x5 = double (x2);
	var x6 = canvas (x4, x5);
	var x7 = paint (x6, x1);
	var x8 = objects (x7, F, F, T);
	var x9 = first (x8);
	var x10 = shift (x9, LEFT);
	var x11 = vperiod (x10);
	var x12 = hperiod (x10);
	var x13 = neighbors (ORIGIN);
	var x14 = lbind (mapply, neighbors);
	var x15 = power (x14, TWO);
	var x16 = x15 (x13);
	var x17 = astuple (x11, x12);
	var x18 = lbind (multiply, x17);
	var x19 = apply (x18, x16);
	var x20 = lbind (shift, x10);
	var x21 = mapply (x20, x19);
	var O = paint (I, x21);
	return O;
};
export var solve_e21d9049 = function (I) {
	var x1 = asindices (I);
	var x2 = leastcolor (I);
	var x3 = objects (I, T, F, T);
	var x4 = ofcolor (I, x2);
	var x5 = merge (x3);
	var x6 = shape (x5);
	var x7 = neighbors (ORIGIN);
	var x8 = lbind (mapply, neighbors);
	var x9 = power (x8, TWO);
	var x10 = x9 (x7);
	var x11 = lbind (multiply, x6);
	var x12 = lbind (shift, x5);
	var x13 = apply (x11, x10);
	var x14 = mapply (x12, x13);
	var x15 = lbind (hmatching, x4);
	var x16 = lbind (vmatching, x4);
	var x17 = fork (either, x15, x16);
	var x18 = compose (x17, initset);
	var x19 = paint (I, x14);
	var x20 = sfilter (x1, x18);
	var x21 = difference (x1, x20);
	var O = cover (x19, x21);
	return O;
};
export var solve_d89b689b = function (I) {
	var x1 = asindices (I);
	var x2 = ofcolor (I, EIGHT);
	var x3 = py_replace (I, EIGHT, ZERO);
	var x4 = vsplit (x3, TWO);
	var x5 = rbind (order, leftmost);
	var x6 = lbind (apply, color);
	var x7 = rbind (repeat, ONE);
	var x8 = chain (x7, x6, x5);
	var x9 = matcher (first, ZERO);
	var x10 = compose (flip, x9);
	var x11 = rbind (sfilter, x10);
	var x12 = lbind (apply, initset);
	var x13 = chain (x12, x11, asobject);
	var x14 = compose (x8, x13);
	var x15 = apply (x14, x4);
	var x16 = merge (x15);
	var x17 = ulcorner (x2);
	var x18 = asindices (x16);
	var x19 = toobject (x18, x16);
	var x20 = shift (x19, x17);
	var x21 = cover (I, x1);
	var O = paint (x21, x20);
	return O;
};
export var solve_746b3537 = function (I) {
	var x1 = rot90 (I);
	var x2 = objects (I, T, F, F);
	var x3 = sfilter (x2, vline);
	var x4 = compose (positive, size);
	var x5 = x4 (x3);
	var x6 = branch (x5, x1, I);
	var x7 = height (x6);
	var x8 = astuple (x7, ONE);
	var x9 = crop (x6, ORIGIN, x8);
	var x10 = objects (x9, T, F, F);
	var x11 = asindices (x9);
	var x12 = apply (center, x10);
	var x13 = difference (x11, x12);
	var x14 = fill (x9, ZERO, x13);
	var x15 = vsplit (x14, x7);
	var x16 = canvas (ZERO, UNITY);
	var x17 = rbind (equality, x16);
	var x18 = compose (flip, x17);
	var x19 = sfilter (x15, x18);
	var x20 = merge (x19);
	var x21 = dmirror (x20);
	var O = branch (x5, x21, x20);
	return O;
};
export var solve_63613498 = function (I) {
	var x1 = objects (I, F, F, T);
	var x2 = crop (I, ORIGIN, THREE_BY_THREE);
	var x3 = partition (x2);
	var x4 = colorfilter (x3, ZERO);
	var x5 = difference (x3, x4);
	var x6 = first (x5);
	var x7 = toindices (x6);
	var x8 = ulcorner (x7);
	var x9 = invert (x8);
	var x10 = shift (x7, x9);
	var x11 = totuple (x1);
	var x12 = apply (toindices, x11);
	var x13 = apply (normalize, x12);
	var x14 = pair (x11, x13);
	var x15 = matcher (last, x10);
	var x16 = sfilter (x14, x15);
	var x17 = matcher (first, x6);
	var x18 = compose (flip, x17);
	var x19 = extract (x16, x18);
	var x20 = first (x19);
	var x21 = recolor (FIVE, x20);
	var O = paint (I, x21);
	return O;
};
export var solve_06df4c85 = function (I) {
	var x1 = partition (I);
	var x2 = mostcolor (I);
	var x3 = ofcolor (I, x2);
	var x4 = colorfilter (x1, ZERO);
	var x5 = argmax (x1, size);
	var x6 = difference (x1, x4);
	var x7 = remove (x5, x6);
	var x8 = merge (x7);
	var x9 = product (x8, x8);
	var x10 = power (first, TWO);
	var x11 = compose (first, last);
	var x12 = fork (equality, x10, x11);
	var x13 = sfilter (x9, x12);
	var x14 = compose (last, first);
	var x15 = power (last, TWO);
	var x16 = fork (connect, x14, x15);
	var x17 = fork (recolor, color, x16);
	var x18 = apply (x17, x13);
	var x19 = fork (either, vline, hline);
	var x20 = mfilter (x18, x19);
	var x21 = paint (I, x20);
	var O = fill (x21, x2, x3);
	return O;
};
export var solve_f9012d9b = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = ofcolor (I, ZERO);
	var x3 = lbind (contained, ZERO);
	var x4 = chain (flip, x3, palette);
	var x5 = mfilter (x1, x4);
	var x6 = vsplit (I, TWO);
	var x7 = hsplit (I, TWO);
	var x8 = extract (x6, x4);
	var x9 = extract (x7, x4);
	var x10 = asobject (x8);
	var x11 = asobject (x9);
	var x12 = vperiod (x10);
	var x13 = hperiod (x11);
	var x14 = neighbors (ORIGIN);
	var x15 = mapply (neighbors, x14);
	var x16 = astuple (x12, x13);
	var x17 = rbind (multiply, x16);
	var x18 = apply (x17, x15);
	var x19 = lbind (shift, x5);
	var x20 = mapply (x19, x18);
	var x21 = paint (I, x20);
	var O = subgrid (x2, x21);
	return O;
};
export var solve_4522001f = function (I) {
	var x1 = objects (I, F, F, T);
	var x2 = first (x1);
	var x3 = toindices (x2);
	var x4 = contained (ZERO_BY_TWO, x3);
	var x5 = contained (TWO_BY_TWO, x3);
	var x6 = contained (TWO_BY_ZERO, x3);
	var x7 = astuple (NINE, NINE);
	var x8 = canvas (ZERO, x7);
	var x9 = astuple (THREE, ORIGIN);
	var x10 = initset (x9);
	var x11 = upscale (x10, TWO);
	var x12 = upscale (x11, TWO);
	var x13 = shape (x12);
	var x14 = shift (x12, x13);
	var x15 = combine (x12, x14);
	var x16 = paint (x8, x15);
	var x17 = rot90 (x16);
	var x18 = rot180 (x16);
	var x19 = rot270 (x16);
	var x20 = branch (x4, x17, x16);
	var x21 = branch (x5, x18, x20);
	var O = branch (x6, x19, x21);
	return O;
};
export var solve_a48eeaf7 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = ofcolor (I, TWO);
	var x3 = colorfilter (x1, FIVE);
	var x4 = rbind (gravitate, x2);
	var x5 = fork (shift, identity, x4);
	var x6 = mapply (x5, x3);
	var x7 = paint (I, x6);
	var x8 = rbind (vmatching, x2);
	var x9 = rbind (hmatching, x2);
	var x10 = fork (either, x8, x9);
	var x11 = sfilter (x3, x10);
	var x12 = merge (x3);
	var x13 = cover (x7, x12);
	var x14 = difference (x3, x11);
	var x15 = rbind (position, x2);
	var x16 = rbind (manhattan, x2);
	var x17 = compose (halve, x16);
	var x18 = fork (multiply, x17, x15);
	var x19 = fork (subtract, x18, x15);
	var x20 = fork (shift, identity, x19);
	var x21 = mapply (x20, x14);
	var O = paint (x13, x21);
	return O;
};
export var solve_eb5a1d5d = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = argmin (x1, size);
	var x3 = color (x2);
	var x4 = compose (invert, width);
	var x5 = order (x1, x4);
	var x6 = apply (color, x5);
	var x7 = size (x5);
	var x8 = double (x7);
	var x9 = decrement (x8);
	var x10 = interval (ZERO, x7, ONE);
	var x11 = pair (x10, x10);
	var x12 = decrement (x9);
	var x13 = interval (x12, ZERO, NEG_TWO);
	var x14 = papply (add, x13, x10);
	var x15 = order (x14, invert);
	var x16 = pair (x15, x15);
	var x17 = pair (x11, x16);
	var x18 = apply (box, x17);
	var x19 = mpapply (recolor, x6, x18);
	var x20 = astuple (x9, x9);
	var x21 = canvas (ZERO, x20);
	var x22 = paint (x21, x19);
	var O = py_replace (x22, ZERO, x3);
	return O;
};
export var solve_e179c5f4 = function (I) {
	var x1 = height (I);
	var x2 = ofcolor (I, ONE);
	var x3 = first (x2);
	var x4 = shoot (x3, UP_RIGHT);
	var x5 = fill (I, ONE, x4);
	var x6 = ofcolor (x5, ONE);
	var x7 = urcorner (x6);
	var x8 = shoot (x7, NEG_UNITY);
	var x9 = fill (x5, ONE, x8);
	var x10 = ofcolor (x9, ONE);
	var x11 = subgrid (x10, x9);
	var x12 = height (x11);
	var x13 = width (x11);
	var x14 = decrement (x12);
	var x15 = astuple (x14, x13);
	var x16 = ulcorner (x10);
	var x17 = crop (x9, x16, x15);
	var x18 = repeat (x17, NINE);
	var x19 = merge (x18);
	var x20 = astuple (x1, x13);
	var x21 = crop (x19, ORIGIN, x20);
	var x22 = hmirror (x21);
	var O = py_replace (x22, ZERO, EIGHT);
	return O;
};
export var solve_228f6490 = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = colorfilter (x1, ZERO);
	var x3 = rbind (bordering, I);
	var x4 = compose (flip, x3);
	var x5 = sfilter (x2, x4);
	var x6 = first (x5);
	var x7 = last (x5);
	var x8 = difference (x1, x2);
	var x9 = compose (normalize, toindices);
	var x10 = x9 (x6);
	var x11 = x9 (x7);
	var x12 = matcher (x9, x10);
	var x13 = matcher (x9, x11);
	var x14 = extract (x8, x12);
	var x15 = extract (x8, x13);
	var x16 = ulcorner (x6);
	var x17 = ulcorner (x7);
	var x18 = ulcorner (x14);
	var x19 = ulcorner (x15);
	var x20 = subtract (x16, x18);
	var x21 = subtract (x17, x19);
	var x22 = move (I, x14, x20);
	var O = move (x22, x15, x21);
	return O;
};
export var solve_995c5fa3 = function (I) {
	var x1 = hsplit (I, THREE);
	var x2 = astuple (TWO, ONE);
	var x3 = rbind (ofcolor, ZERO);
	var x4 = compose (ulcorner, x3);
	var x5 = compose (size, x3);
	var x6 = matcher (x5, ZERO);
	var x7 = matcher (x4, UNITY);
	var x8 = matcher (x4, DOWN);
	var x9 = matcher (x4, x2);
	var x10 = rbind (multiply, THREE);
	var x11 = power (double, TWO);
	var x12 = compose (double, x6);
	var x13 = chain (x11, double, x7);
	var x14 = compose (x10, x8);
	var x15 = compose (x11, x9);
	var x16 = fork (add, x12, x13);
	var x17 = fork (add, x14, x15);
	var x18 = fork (add, x16, x17);
	var x19 = rbind (canvas, UNITY);
	var x20 = compose (x19, x18);
	var x21 = apply (x20, x1);
	var x22 = merge (x21);
	var O = hupscale (x22, THREE);
	return O;
};
export var solve_d06dbe63 = function (I) {
	var x1 = ofcolor (I, EIGHT);
	var x2 = center (x1);
	var x3 = connect (ORIGIN, DOWN);
	var x4 = connect (ORIGIN, ZERO_BY_TWO);
	var x5 = combine (x3, x4);
	var x6 = subtract (x2, TWO_BY_ZERO);
	var x7 = shift (x5, x6);
	var x8 = astuple (NEG_TWO, TWO);
	var x9 = interval (ZERO, FIVE, ONE);
	var x10 = lbind (multiply, x8);
	var x11 = apply (x10, x9);
	var x12 = lbind (shift, x7);
	var x13 = mapply (x12, x11);
	var x14 = fill (I, FIVE, x13);
	var x15 = rot180 (x14);
	var x16 = ofcolor (x15, EIGHT);
	var x17 = center (x16);
	var x18 = subtract (x17, x6);
	var x19 = shift (x13, x18);
	var x20 = toivec (NEG_TWO);
	var x21 = shift (x19, x20);
	var x22 = fill (x15, FIVE, x21);
	var O = rot180 (x22);
	return O;
};
export var solve_36fdfd69 = function (I) {
	var x1 = upscale (I, TWO);
	var x2 = ofcolor (x1, TWO);
	var x3 = mapply (neighbors, x2);
	var x4 = difference (x3, x2);
	var x5 = fill (x1, FOUR, x4);
	var x6 = objects (x5, T, T, T);
	var x7 = colorfilter (x6, FOUR);
	var x8 = totuple (x7);
	var x9 = rbind (subgrid, x5);
	var x10 = apply (x9, x8);
	var x11 = apply (ulcorner, x8);
	var x12 = rbind (ofcolor, TWO);
	var x13 = apply (x12, x10);
	var x14 = papply (subgrid, x13, x10);
	var x15 = rbind (downscale, TWO);
	var x16 = apply (x15, x14);
	var x17 = apply (asindices, x16);
	var x18 = apply (increment, x11);
	var x19 = apply (halve, x18);
	var x20 = mpapply (shift, x17, x19);
	var x21 = fill (I, FOUR, x20);
	var x22 = ofcolor (I, TWO);
	var O = fill (x21, TWO, x22);
	return O;
};
export var solve_0a938d79 = function (I) {
	var x1 = portrait (I);
	var x2 = branch (x1, dmirror, identity);
	var x3 = x2 (I);
	var x4 = objects (x3, T, F, T);
	var x5 = argmin (x4, leftmost);
	var x6 = argmax (x4, leftmost);
	var x7 = color (x5);
	var x8 = color (x6);
	var x9 = leftmost (x5);
	var x10 = leftmost (x6);
	var x11 = subtract (x10, x9);
	var x12 = double (x11);
	var x13 = multiply (THREE, TEN);
	var x14 = interval (x9, x13, x12);
	var x15 = interval (x10, x13, x12);
	var x16 = compose (vfrontier, tojvec);
	var x17 = mapply (x16, x14);
	var x18 = mapply (x16, x15);
	var x19 = recolor (x7, x17);
	var x20 = recolor (x8, x18);
	var x21 = combine (x19, x20);
	var x22 = paint (x3, x21);
	var O = x2 (x22);
	return O;
};
export var solve_045e512c = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = apply (size, x1);
	var x3 = maximum (x2);
	var x4 = lbind (greater, x3);
	var x5 = compose (x4, size);
	var x6 = sfilter (x1, x5);
	var x7 = difference (x1, x6);
	var x8 = first (x7);
	var x9 = interval (ONE, FOUR, ONE);
	var x10 = lbind (multiply, FOUR);
	var x11 = apply (x10, x9);
	var x12 = product (x6, x11);
	var x13 = totuple (x12);
	var x14 = apply (first, x13);
	var x15 = apply (last, x13);
	var x16 = apply (color, x14);
	var x17 = lbind (position, x8);
	var x18 = apply (x17, x14);
	var x19 = papply (multiply, x18, x15);
	var x20 = rbind (recolor, x8);
	var x21 = apply (x20, x16);
	var x22 = mpapply (shift, x21, x19);
	var x23 = paint (I, x22);
	var O = paint (x23, x8);
	return O;
};
export var solve_82819916 = function (I) {
	var x1 = objects (I, F, F, T);
	var x2 = index (I, DOWN);
	var x3 = argmax (x1, size);
	var x4 = remove (x3, x1);
	var x5 = matcher (first, x2);
	var x6 = sfilter (x3, x5);
	var x7 = difference (x3, x6);
	var x8 = lbind (shift, x6);
	var x9 = lbind (shift, x7);
	var x10 = totuple (x4);
	var x11 = apply (ulcorner, x10);
	var x12 = apply (urcorner, x10);
	var x13 = apply (first, x11);
	var x14 = apply (decrement, x13);
	var x15 = apply (toivec, x14);
	var x16 = lbind (index, I);
	var x17 = apply (x16, x11);
	var x18 = apply (x16, x12);
	var x19 = apply (x8, x15);
	var x20 = apply (x9, x15);
	var x21 = mpapply (recolor, x17, x19);
	var x22 = mpapply (recolor, x18, x20);
	var x23 = paint (I, x21);
	var O = paint (x23, x22);
	return O;
};
export var solve_99fa7670 = function (I) {
	var x1 = shape (I);
	var x2 = objects (I, T, F, T);
	var x3 = rbind (shoot, RIGHT);
	var x4 = compose (x3, center);
	var x5 = fork (recolor, color, x4);
	var x6 = mapply (x5, x2);
	var x7 = paint (I, x6);
	var x8 = add (x1, DOWN_LEFT);
	var x9 = initset (x8);
	var x10 = recolor (ZERO, x9);
	var x11 = objects (x7, T, F, T);
	var x12 = insert (x10, x11);
	var x13 = order (x12, uppermost);
	var x14 = first (x13);
	var x15 = remove (x10, x13);
	var x16 = remove (x14, x13);
	var x17 = compose (lrcorner, first);
	var x18 = compose (lrcorner, last);
	var x19 = fork (connect, x17, x18);
	var x20 = compose (color, first);
	var x21 = fork (recolor, x20, x19);
	var x22 = pair (x15, x16);
	var x23 = mapply (x21, x22);
	var O = underpaint (x7, x23);
	return O;
};
export var solve_72322fa7 = function (I) {
	var x1 = objects (I, F, T, T);
	var x2 = matcher (numcolors, ONE);
	var x3 = sfilter (x1, x2);
	var x4 = difference (x1, x3);
	var x5 = lbind (matcher, first);
	var x6 = compose (x5, mostcolor);
	var x7 = fork (sfilter, identity, x6);
	var x8 = fork (difference, identity, x7);
	var x9 = lbind (occurrences, I);
	var x10 = compose (x9, x7);
	var x11 = compose (x9, x8);
	var x12 = compose (ulcorner, x8);
	var x13 = fork (subtract, ulcorner, x12);
	var x14 = lbind (rbind, add);
	var x15 = compose (x14, x13);
	var x16 = fork (apply, x15, x11);
	var x17 = lbind (lbind, shift);
	var x18 = compose (x17, normalize);
	var x19 = fork (mapply, x18, x10);
	var x20 = fork (mapply, x18, x16);
	var x21 = mapply (x19, x4);
	var x22 = mapply (x20, x4);
	var x23 = paint (I, x21);
	var O = paint (x23, x22);
	return O;
};
export var solve_855e0971 = function (I) {
	var x1 = rot90 (I);
	var x2 = objects (I, T, F, F);
	var x3 = sizefilter (x2, ONE);
	var x4 = difference (x2, x3);
	var x5 = first (x4);
	var x6 = portrait (x5);
	var x7 = branch (x6, x1, I);
	var x8 = objects (x7, T, F, F);
	var x9 = sizefilter (x8, ONE);
	var x10 = difference (x8, x9);
	var x11 = rbind (subgrid, x7);
	var x12 = rbind (ofcolor, ZERO);
	var x13 = lbind (mapply, vfrontier);
	var x14 = chain (x13, x12, x11);
	var x15 = lbind (recolor, ZERO);
	var x16 = compose (x15, x14);
	var x17 = fork (paint, x11, x16);
	var x18 = fork (toobject, asindices, identity);
	var x19 = compose (x18, x17);
	var x20 = fork (shift, x19, ulcorner);
	var x21 = mapply (x20, x10);
	var x22 = paint (x7, x21);
	var x23 = rot270 (x22);
	var O = branch (x6, x23, x22);
	return O;
};
export var solve_a78176bb = function (I) {
	var x1 = palette (I);
	var x2 = objects (I, T, F, T);
	var x3 = remove (ZERO, x1);
	var x4 = other (x3, FIVE);
	var x5 = colorfilter (x2, FIVE);
	var x6 = lbind (index, I);
	var x7 = compose (x6, urcorner);
	var x8 = matcher (x7, FIVE);
	var x9 = sfilter (x5, x8);
	var x10 = difference (x5, x9);
	var x11 = apply (urcorner, x9);
	var x12 = apply (llcorner, x10);
	var x13 = rbind (add, UP_RIGHT);
	var x14 = rbind (add, DOWN_LEFT);
	var x15 = apply (x13, x11);
	var x16 = apply (x14, x12);
	var x17 = rbind (shoot, UNITY);
	var x18 = rbind (shoot, NEG_UNITY);
	var x19 = fork (combine, x17, x18);
	var x20 = mapply (x19, x15);
	var x21 = mapply (x19, x16);
	var x22 = combine (x20, x21);
	var x23 = fill (I, x4, x22);
	var O = py_replace (x23, FIVE, ZERO);
	return O;
};
export var solve_952a094c = function (I) {
	var x1 = objects (I, F, F, T);
	var x2 = first (x1);
	var x3 = inbox (x2);
	var x4 = toobject (x3, I);
	var x5 = dmirror (x4);
	var x6 = cmirror (x5);
	var x7 = paint (I, x6);
	var x8 = lbind (index, x7);
	var x9 = fork (astuple, x8, identity);
	var x10 = compose (initset, x9);
	var x11 = double (NEG_UNITY);
	var x12 = astuple (NEG_TWO, TWO);
	var x13 = astuple (TWO, NEG_TWO);
	var x14 = ulcorner (x3);
	var x15 = urcorner (x3);
	var x16 = llcorner (x3);
	var x17 = lrcorner (x3);
	var x18 = x10 (x14);
	var x19 = move (x7, x18, x11);
	var x20 = x10 (x15);
	var x21 = move (x19, x20, x12);
	var x22 = x10 (x16);
	var x23 = move (x21, x22, x13);
	var x24 = x10 (x17);
	var O = move (x23, x24, TWO_BY_TWO);
	return O;
};
export var solve_6d58a25d = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = argmax (x1, size);
	var x3 = remove (x2, x1);
	var x4 = first (x3);
	var x5 = color (x4);
	var x6 = leftmost (x2);
	var x7 = rightmost (x2);
	var x8 = center (x2);
	var x9 = first (x8);
	var x10 = rbind (greater, x6);
	var x11 = compose (x10, leftmost);
	var x12 = lbind (greater, x7);
	var x13 = compose (x12, rightmost);
	var x14 = rbind (greater, x9);
	var x15 = compose (x14, lowermost);
	var x16 = fork (both, x11, x13);
	var x17 = fork (both, x16, x15);
	var x18 = sfilter (x3, x17);
	var x19 = mapply (toindices, x18);
	var x20 = apply (last, x19);
	var x21 = lbind (astuple, x9);
	var x22 = apply (x21, x20);
	var x23 = rbind (shoot, DOWN);
	var x24 = mapply (x23, x22);
	var O = underfill (I, x5, x24);
	return O;
};
export var solve_6aa20dc0 = function (I) {
	var x1 = objects (I, F, T, T);
	var x2 = argmax (x1, numcolors);
	var x3 = normalize (x2);
	var x4 = lbind (matcher, first);
	var x5 = compose (x4, mostcolor);
	var x6 = fork (sfilter, identity, x5);
	var x7 = fork (difference, identity, x6);
	var x8 = lbind (rbind, upscale);
	var x9 = interval (ONE, FOUR, ONE);
	var x10 = apply (x8, x9);
	var x11 = initset (identity);
	var x12 = insert (vmirror, x11);
	var x13 = insert (hmirror, x12);
	var x14 = insert (cmirror, x13);
	var x15 = insert (dmirror, x14);
	var x16 = fork (compose, first, last);
	var x17 = lbind (occurrences, I);
	var x18 = lbind (lbind, shift);
	var x19 = compose (x17, x7);
	var x20 = product (x15, x10);
	var x21 = apply (x16, x20);
	var x22 = rapply (x21, x3);
	var x23 = fork (mapply, x18, x19);
	var x24 = mapply (x23, x22);
	var O = paint (I, x24);
	return O;
};
export var solve_e6721834 = function (I) {
	var x1 = portrait (I);
	var x2 = branch (x1, vsplit, hsplit);
	var x3 = x2 (I, TWO);
	var x4 = order (x3, numcolors);
	var x5 = first (x4);
	var x6 = last (x4);
	var x7 = objects (x6, F, F, T);
	var x8 = merge (x7);
	var x9 = mostcolor (x8);
	var x10 = matcher (first, x9);
	var x11 = compose (flip, x10);
	var x12 = rbind (sfilter, x11);
	var x13 = lbind (occurrences, x5);
	var x14 = compose (x13, x12);
	var x15 = chain (positive, size, x14);
	var x16 = sfilter (x7, x15);
	var x17 = chain (first, x13, x12);
	var x18 = compose (ulcorner, x12);
	var x19 = fork (subtract, x17, x18);
	var x20 = fork (shift, identity, x19);
	var x21 = apply (x20, x16);
	var x22 = compose (decrement, width);
	var x23 = chain (positive, decrement, x22);
	var x24 = mfilter (x21, x23);
	var O = paint (x5, x24);
	return O;
};
export var solve_447fd412 = function (I) {
	var x1 = objects (I, F, T, T);
	var x2 = argmax (x1, numcolors);
	var x3 = normalize (x2);
	var x4 = lbind (matcher, first);
	var x5 = compose (x4, mostcolor);
	var x6 = fork (sfilter, identity, x5);
	var x7 = fork (difference, identity, x6);
	var x8 = lbind (rbind, upscale);
	var x9 = interval (ONE, FOUR, ONE);
	var x10 = apply (x8, x9);
	var x11 = lbind (recolor, ZERO);
	var x12 = compose (x11, outbox);
	var x13 = fork (combine, identity, x12);
	var x14 = lbind (occurrences, I);
	var x15 = lbind (rbind, subtract);
	var x16 = lbind (apply, increment);
	var x17 = lbind (lbind, shift);
	var x18 = chain (x15, ulcorner, x7);
	var x19 = chain (x14, x13, x7);
	var x20 = fork (apply, x18, x19);
	var x21 = compose (x16, x20);
	var x22 = fork (mapply, x17, x21);
	var x23 = rapply (x10, x3);
	var x24 = mapply (x22, x23);
	var O = paint (I, x24);
	return O;
};
export var solve_2bcee788 = function (I) {
	var x1 = mostcolor (I);
	var x2 = objects (I, T, F, T);
	var x3 = py_replace (I, x1, THREE);
	var x4 = argmax (x2, size);
	var x5 = argmin (x2, size);
	var x6 = position (x4, x5);
	var x7 = first (x6);
	var x8 = last (x6);
	var x9 = subgrid (x4, x3);
	var x10 = hline (x5);
	var x11 = hmirror (x9);
	var x12 = vmirror (x9);
	var x13 = branch (x10, x11, x12);
	var x14 = branch (x10, x7, ZERO);
	var x15 = branch (x10, ZERO, x8);
	var x16 = asobject (x13);
	var x17 = matcher (first, THREE);
	var x18 = compose (flip, x17);
	var x19 = sfilter (x16, x18);
	var x20 = ulcorner (x4);
	var x21 = shape (x4);
	var x22 = astuple (x14, x15);
	var x23 = multiply (x21, x22);
	var x24 = add (x20, x23);
	var x25 = shift (x19, x24);
	var O = paint (x3, x25);
	return O;
};
export var solve_776ffc46 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = colorfilter (x1, FIVE);
	var x3 = totuple (x2);
	var x4 = rbind (subgrid, I);
	var x5 = apply (x4, x3);
	var x6 = multiply (FOUR, SIX);
	var x7 = rbind (ofcolor, FIVE);
	var x8 = compose (size, x7);
	var x9 = matcher (x8, x6);
	var x10 = extract (x5, x9);
	var x11 = astuple (FIVE, FIVE);
	var x12 = crop (x10, UNITY, x11);
	var x13 = objects (x12, T, F, T);
	var x14 = first (x13);
	var x15 = color (x14);
	var x16 = normalize (x14);
	var x17 = toindices (x16);
	var x18 = totuple (x1);
	var x19 = apply (normalize, x18);
	var x20 = apply (toindices, x19);
	var x21 = pair (x18, x20);
	var x22 = matcher (last, x17);
	var x23 = sfilter (x21, x22);
	var x24 = mapply (first, x23);
	var x25 = recolor (x15, x24);
	var O = paint (I, x25);
	return O;
};
export var solve_f35d900a = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = merge (x1);
	var x3 = hmirror (x2);
	var x4 = compose (neighbors, last);
	var x5 = fork (recolor, first, x4);
	var x6 = mapply (x5, x3);
	var x7 = paint (I, x6);
	var x8 = outbox (x2);
	var x9 = ulcorner (x8);
	var x10 = subgrid (x8, x7);
	var x11 = dneighbors (ORIGIN);
	var x12 = rbind (multiply, FOUR);
	var x13 = apply (double, x11);
	var x14 = apply (x12, x11);
	var x15 = apply (increment, x13);
	var x16 = apply (increment, x14);
	var x17 = combine (x15, x16);
	var x18 = underfill (x10, FIVE, x17);
	var x19 = vmirror (x18);
	var x20 = underfill (x19, FIVE, x17);
	var x21 = ofcolor (x20, FIVE);
	var x22 = hmirror (x20);
	var x23 = underfill (x22, FIVE, x21);
	var x24 = ofcolor (x23, FIVE);
	var x25 = shift (x24, x9);
	var O = fill (x7, FIVE, x25);
	return O;
};
export var solve_0dfd9992 = function (I) {
	var x1 = height (I);
	var x2 = width (I);
	var x3 = partition (I);
	var x4 = colorfilter (x3, ZERO);
	var x5 = difference (x3, x4);
	var x6 = merge (x5);
	var x7 = astuple (x1, ONE);
	var x8 = astuple (ONE, x2);
	var x9 = decrement (x1);
	var x10 = decrement (x2);
	var x11 = toivec (x10);
	var x12 = tojvec (x9);
	var x13 = crop (I, x11, x8);
	var x14 = crop (I, x12, x7);
	var x15 = asobject (x14);
	var x16 = asobject (x13);
	var x17 = vperiod (x15);
	var x18 = hperiod (x16);
	var x19 = astuple (x17, x18);
	var x20 = lbind (multiply, x19);
	var x21 = neighbors (ORIGIN);
	var x22 = mapply (neighbors, x21);
	var x23 = apply (x20, x22);
	var x24 = lbind (shift, x6);
	var x25 = mapply (x24, x23);
	var O = paint (I, x25);
	return O;
};
export var solve_29ec7d0e = function (I) {
	var x1 = height (I);
	var x2 = width (I);
	var x3 = partition (I);
	var x4 = colorfilter (x3, ZERO);
	var x5 = difference (x3, x4);
	var x6 = merge (x5);
	var x7 = astuple (x1, ONE);
	var x8 = astuple (ONE, x2);
	var x9 = decrement (x1);
	var x10 = decrement (x2);
	var x11 = toivec (x10);
	var x12 = tojvec (x9);
	var x13 = crop (I, x11, x8);
	var x14 = crop (I, x12, x7);
	var x15 = asobject (x14);
	var x16 = asobject (x13);
	var x17 = vperiod (x15);
	var x18 = hperiod (x16);
	var x19 = astuple (x17, x18);
	var x20 = lbind (multiply, x19);
	var x21 = neighbors (ORIGIN);
	var x22 = mapply (neighbors, x21);
	var x23 = apply (x20, x22);
	var x24 = lbind (shift, x6);
	var x25 = mapply (x24, x23);
	var O = paint (I, x25);
	return O;
};
export var solve_36d67576 = function (I) {
	var x1 = objects (I, F, F, T);
	var x2 = argmax (x1, numcolors);
	var x3 = astuple (TWO, FOUR);
	var x4 = rbind (contained, x3);
	var x5 = compose (x4, first);
	var x6 = rbind (sfilter, x5);
	var x7 = lbind (rbind, subtract);
	var x8 = lbind (occurrences, I);
	var x9 = lbind (lbind, shift);
	var x10 = compose (x7, ulcorner);
	var x11 = chain (x10, x6, normalize);
	var x12 = chain (x8, x6, normalize);
	var x13 = fork (apply, x11, x12);
	var x14 = compose (x9, normalize);
	var x15 = fork (mapply, x14, x13);
	var x16 = astuple (cmirror, dmirror);
	var x17 = astuple (hmirror, vmirror);
	var x18 = combine (x16, x17);
	var x19 = product (x18, x18);
	var x20 = fork (compose, first, last);
	var x21 = apply (x20, x19);
	var x22 = totuple (x21);
	var x23 = combine (x18, x22);
	var x24 = rapply (x23, x2);
	var x25 = mapply (x15, x24);
	var O = paint (I, x25);
	return O;
};
export var solve_98cf29f8 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = fork (multiply, height, width);
	var x3 = fork (equality, size, x2);
	var x4 = extract (x1, x3);
	var x5 = other (x1, x4);
	var x6 = toindices (x5);
	var x7 = rbind (add, LEFT);
	var x8 = rbind (add, RIGHT);
	var x9 = rbind (add, UP);
	var x10 = rbind (add, DOWN);
	var x11 = initset (ZERO);
	var x12 = compose (initset, x8);
	var x13 = compose (initset, x10);
	var x14 = fork (insert, x7, x12);
	var x15 = fork (insert, x9, x13);
	var x16 = matcher (palette, x11);
	var x17 = rbind (toobject, I);
	var x18 = chain (x16, x17, x14);
	var x19 = chain (x16, x17, x15);
	var x20 = fork (either, x18, x19);
	var x21 = sfilter (x6, x20);
	var x22 = cover (I, x21);
	var x23 = difference (x6, x21);
	var x24 = gravitate (x23, x4);
	var x25 = toobject (x23, x22);
	var O = move (x22, x25, x24);
	return O;
};
export var solve_469497ad = function (I) {
	var x1 = numcolors (I);
	var x2 = decrement (x1);
	var x3 = upscale (I, x2);
	var x4 = objects (x3, F, F, T);
	var x5 = argmin (x4, size);
	var x6 = ulcorner (x5);
	var x7 = llcorner (x5);
	var x8 = shoot (x6, NEG_UNITY);
	var x9 = shoot (x6, UNITY);
	var x10 = shoot (x7, DOWN_LEFT);
	var x11 = shoot (x7, UP_RIGHT);
	var x12 = combine (x8, x9);
	var x13 = combine (x10, x11);
	var x14 = combine (x12, x13);
	var x15 = underfill (x3, TWO, x14);
	var x16 = objects (x15, T, F, T);
	var x17 = argmax (x16, lrcorner);
	var x18 = urcorner (x17);
	var x19 = tojvec (NEG_TWO);
	var x20 = add (x18, x19);
	var x21 = connect (x18, x20);
	var x22 = toobject (x21, x15);
	var x23 = shift (x22, UP);
	var x24 = color (x23);
	var x25 = equality (x24, SIX);
	var x26 = branch (x25, x23, x17);
	var O = paint (x15, x26);
	return O;
};
export var solve_39e1d7f9 = function (I) {
	var x1 = fgpartition (I);
	var x2 = objects (I, T, F, T);
	var x3 = order (x1, height);
	var x4 = last (x3);
	var x5 = remove (x4, x3);
	var x6 = last (x5);
	var x7 = color (x6);
	var x8 = colorfilter (x2, x7);
	var x9 = power (outbox, TWO);
	var x10 = rbind (toobject, I);
	var x11 = chain (numcolors, x10, x9);
	var x12 = argmax (x8, x11);
	var x13 = ulcorner (x12);
	var x14 = shape (x12);
	var x15 = subtract (x13, x14);
	var x16 = decrement (x15);
	var x17 = multiply (x14, THREE);
	var x18 = add (x17, TWO_BY_TWO);
	var x19 = crop (I, x16, x18);
	var x20 = asobject (x19);
	var x21 = apply (ulcorner, x8);
	var x22 = increment (x14);
	var x23 = rbind (subtract, x22);
	var x24 = apply (x23, x21);
	var x25 = lbind (shift, x20);
	var x26 = mapply (x25, x24);
	var O = paint (I, x26);
	return O;
};
export var solve_484b58aa = function (I) {
	var x1 = height (I);
	var x2 = width (I);
	var x3 = partition (I);
	var x4 = colorfilter (x3, ZERO);
	var x5 = difference (x3, x4);
	var x6 = merge (x5);
	var x7 = astuple (x1, TWO);
	var x8 = astuple (TWO, x2);
	var x9 = power (decrement, TWO);
	var x10 = x9 (x1);
	var x11 = x9 (x2);
	var x12 = toivec (x11);
	var x13 = tojvec (x10);
	var x14 = crop (I, x12, x8);
	var x15 = crop (I, x13, x7);
	var x16 = asobject (x15);
	var x17 = asobject (x14);
	var x18 = vperiod (x16);
	var x19 = hperiod (x17);
	var x20 = astuple (x18, x19);
	var x21 = lbind (multiply, x20);
	var x22 = neighbors (ORIGIN);
	var x23 = mapply (neighbors, x22);
	var x24 = apply (x21, x23);
	var x25 = lbind (shift, x6);
	var x26 = mapply (x25, x24);
	var O = paint (I, x26);
	return O;
};
export var solve_3befdf3e = function (I) {
	var x1 = objects (I, F, F, T);
	var x2 = totuple (x1);
	var x3 = apply (mostcolor, x2);
	var x4 = apply (leastcolor, x2);
	var x5 = apply (width, x2);
	var x6 = rbind (subtract, TWO);
	var x7 = apply (x6, x5);
	var x8 = apply (invert, x7);
	var x9 = papply (recolor, x4, x2);
	var x10 = apply (toivec, x7);
	var x11 = mpapply (shift, x9, x10);
	var x12 = paint (I, x11);
	var x13 = apply (toivec, x8);
	var x14 = mpapply (shift, x9, x13);
	var x15 = paint (x12, x14);
	var x16 = apply (tojvec, x7);
	var x17 = mpapply (shift, x9, x16);
	var x18 = paint (x15, x17);
	var x19 = apply (tojvec, x8);
	var x20 = mpapply (shift, x9, x19);
	var x21 = paint (x18, x20);
	var x22 = merge (x2);
	var x23 = paint (x21, x22);
	var x24 = first (x4);
	var x25 = first (x3);
	var x26 = py_replace (x23, x24, NEG_ONE);
	var x27 = py_replace (x26, x25, x24);
	var O = py_replace (x27, NEG_ONE, x25);
	return O;
};
export var solve_9aec4887 = function (I) {
	var x1 = objects (I, F, T, T);
	var x2 = colorfilter (x1, EIGHT);
	var x3 = first (x2);
	var x4 = other (x1, x3);
	var x5 = subgrid (x4, I);
	var x6 = normalize (x3);
	var x7 = shift (x6, UNITY);
	var x8 = paint (x5, x7);
	var x9 = palette (x8);
	var x10 = remove (ZERO, x9);
	var x11 = remove (EIGHT, x10);
	var x12 = lbind (ofcolor, x8);
	var x13 = rbind (remove, x11);
	var x14 = lbind (mapply, x12);
	var x15 = lbind (rbind, manhattan);
	var x16 = chain (x15, x14, x13);
	var x17 = rbind (compose, initset);
	var x18 = lbind (lbind, manhattan);
	var x19 = chain (x17, x18, x12);
	var x20 = compose (x17, x16);
	var x21 = lbind (fork, greater);
	var x22 = fork (x21, x20, x19);
	var x23 = ofcolor (x8, EIGHT);
	var x24 = lbind (sfilter, x23);
	var x25 = compose (x24, x22);
	var x26 = fork (recolor, identity, x25);
	var x27 = mapply (x26, x11);
	var O = paint (x8, x27);
	return O;
};
export var solve_49d1d64f = function (I) {
	var x1 = upscale (I, TWO);
	var x2 = asindices (x1);
	var x3 = corners (x2);
	var x4 = fill (x1, ZERO, x3);
	var x5 = height (x4);
	var x6 = width (x4);
	var x7 = equality (x5, SIX);
	var x8 = equality (x6, SIX);
	var x9 = compose (decrement, halve);
	var x10 = x9 (x5);
	var x11 = x9 (x6);
	var x12 = astuple (x10, x6);
	var x13 = crop (x4, ORIGIN, x12);
	var x14 = increment (x10);
	var x15 = toivec (x14);
	var x16 = astuple (x14, x6);
	var x17 = crop (x4, x15, x16);
	var x18 = vconcat (x13, x17);
	var x19 = branch (x7, x18, x4);
	var x20 = decrement (x5);
	var x21 = branch (x7, x20, x5);
	var x22 = astuple (x21, x11);
	var x23 = crop (x19, ORIGIN, x22);
	var x24 = increment (x11);
	var x25 = tojvec (x24);
	var x26 = astuple (x21, x24);
	var x27 = crop (x19, x25, x26);
	var x28 = hconcat (x23, x27);
	var O = branch (x8, x28, x19);
	return O;
};
export var solve_57aa92db = function (I) {
	var x1 = objects (I, F, T, T);
	var x2 = objects (I, T, F, T);
	var x3 = lbind (lbind, colorcount);
	var x4 = fork (apply, x3, palette);
	var x5 = compose (maximum, x4);
	var x6 = compose (minimum, x4);
	var x7 = fork (subtract, x5, x6);
	var x8 = argmax (x1, x7);
	var x9 = leastcolor (x8);
	var x10 = normalize (x8);
	var x11 = matcher (first, x9);
	var x12 = sfilter (x10, x11);
	var x13 = ulcorner (x12);
	var x14 = colorfilter (x2, x9);
	var x15 = rbind (toobject, I);
	var x16 = lbind (remove, ZERO);
	var x17 = chain (first, x16, palette);
	var x18 = chain (x17, x15, outbox);
	var x19 = lbind (multiply, x13);
	var x20 = compose (x19, width);
	var x21 = fork (subtract, ulcorner, x20);
	var x22 = lbind (shift, x10);
	var x23 = compose (x22, x21);
	var x24 = fork (upscale, x23, width);
	var x25 = fork (recolor, x18, x24);
	var x26 = mapply (x25, x14);
	var x27 = paint (I, x26);
	var x28 = merge (x2);
	var O = paint (x27, x28);
	return O;
};
export var solve_aba27056 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = mapply (toindices, x1);
	var x3 = box (x2);
	var x4 = difference (x3, x2);
	var x5 = delta (x2);
	var x6 = position (x5, x4);
	var x7 = interval (ZERO, NINE, ONE);
	var x8 = lbind (multiply, x6);
	var x9 = apply (x8, x7);
	var x10 = lbind (shift, x4);
	var x11 = mapply (x10, x9);
	var x12 = fill (I, FOUR, x5);
	var x13 = fill (x12, FOUR, x11);
	var x14 = corners (x4);
	var x15 = ofcolor (x13, ZERO);
	var x16 = rbind (toobject, x13);
	var x17 = rbind (colorcount, ZERO);
	var x18 = chain (x17, x16, dneighbors);
	var x19 = matcher (x18, TWO);
	var x20 = rbind (adjacent, x2);
	var x21 = rbind (adjacent, x11);
	var x22 = fork (both, x20, x21);
	var x23 = compose (x22, initset);
	var x24 = sfilter (x15, x19);
	var x25 = sfilter (x24, x23);
	var x26 = product (x14, x25);
	var x27 = fork (subtract, last, first);
	var x28 = fork (shoot, first, x27);
	var x29 = mapply (x28, x26);
	var O = fill (x13, FOUR, x29);
	return O;
};
export var solve_f1cefba8 = function (I) {
	var x1 = palette (I);
	var x2 = objects (I, F, F, T);
	var x3 = ofcolor (I, ZERO);
	var x4 = first (x2);
	var x5 = ulcorner (x4);
	var x6 = subgrid (x4, I);
	var x7 = power (trim, TWO);
	var x8 = x7 (x6);
	var x9 = asindices (x8);
	var x10 = shift (x9, TWO_BY_TWO);
	var x11 = fill (x6, ZERO, x10);
	var x12 = leastcolor (x11);
	var x13 = remove (ZERO, x1);
	var x14 = other (x13, x12);
	var x15 = ofcolor (x11, x12);
	var x16 = shift (x15, x5);
	var x17 = ofcolor (I, x12);
	var x18 = uppermost (x17);
	var x19 = lowermost (x17);
	var x20 = matcher (first, x18);
	var x21 = matcher (first, x19);
	var x22 = fork (either, x20, x21);
	var x23 = sfilter (x16, x22);
	var x24 = difference (x16, x23);
	var x25 = mapply (vfrontier, x23);
	var x26 = mapply (hfrontier, x24);
	var x27 = combine (x25, x26);
	var x28 = intersection (x3, x27);
	var x29 = fill (I, x14, x27);
	var O = fill (x29, x12, x28);
	return O;
};
export var solve_1e32b0e9 = function (I) {
	var x1 = height (I);
	var x2 = mostcolor (I);
	var x3 = asobject (I);
	var x4 = subtract (x1, TWO);
	var x5 = divide (x4, THREE);
	var x6 = astuple (x5, x5);
	var x7 = crop (I, ORIGIN, x6);
	var x8 = partition (x7);
	var x9 = matcher (color, ZERO);
	var x10 = compose (flip, x9);
	var x11 = extract (x8, x10);
	var x12 = initset (x2);
	var x13 = palette (x3);
	var x14 = palette (x11);
	var x15 = difference (x13, x14);
	var x16 = difference (x15, x12);
	var x17 = first (x16);
	var x18 = interval (ZERO, THREE, ONE);
	var x19 = product (x18, x18);
	var x20 = totuple (x19);
	var x21 = apply (first, x20);
	var x22 = apply (last, x20);
	var x23 = lbind (multiply, x5);
	var x24 = apply (x23, x21);
	var x25 = apply (x23, x22);
	var x26 = papply (add, x24, x21);
	var x27 = papply (add, x25, x22);
	var x28 = papply (astuple, x26, x27);
	var x29 = lbind (shift, x11);
	var x30 = mapply (x29, x28);
	var O = underfill (I, x17, x30);
	return O;
};
export var solve_28e73c20 = function (I) {
	var x1 = width (I);
	var x2 = astuple (ONE, TWO);
	var x3 = astuple (TWO, TWO);
	var x4 = astuple (TWO, ONE);
	var x5 = astuple (THREE, ONE);
	var x6 = canvas (THREE, UNITY);
	var x7 = upscale (x6, FOUR);
	var x8 = initset (DOWN);
	var x9 = insert (UNITY, x8);
	var x10 = insert (x2, x9);
	var x11 = insert (x3, x10);
	var x12 = fill (x7, ZERO, x11);
	var x13 = vupscale (x6, FIVE);
	var x14 = hupscale (x13, THREE);
	var x15 = insert (x4, x9);
	var x16 = insert (x5, x15);
	var x17 = fill (x14, ZERO, x16);
	var x18 = even (x1);
	var x19 = branch (x18, x12, x17);
	var x20 = canvas (ZERO, UNITY);
	var x21 = lbind (hupscale, x20);
	var x22 = chain (x21, decrement, height);
	var x23 = rbind (hconcat, x6);
	var x24 = compose (x23, x22);
	var x25 = lbind (hupscale, x6);
	var x26 = compose (x25, height);
	var x27 = fork (vconcat, x24, rot90);
	var x28 = fork (vconcat, x26, x27);
	var x29 = subtract (x1, FOUR);
	var x30 = power (x28, x29);
	var O = x30 (x19);
	return O;
};
export var solve_4c5c2cf0 = function (I) {
	var x1 = objects (I, T, T, T);
	var x2 = objects (I, F, T, T);
	var x3 = first (x2);
	var x4 = rbind (subgrid, I);
	var x5 = fork (equality, identity, rot90);
	var x6 = compose (x5, x4);
	var x7 = extract (x1, x6);
	var x8 = center (x7);
	var x9 = subgrid (x3, I);
	var x10 = hmirror (x9);
	var x11 = objects (x10, F, T, T);
	var x12 = first (x11);
	var x13 = objects (x10, T, T, T);
	var x14 = extract (x13, x6);
	var x15 = center (x14);
	var x16 = subtract (x8, x15);
	var x17 = shift (x12, x16);
	var x18 = paint (I, x17);
	var x19 = objects (x18, F, T, T);
	var x20 = first (x19);
	var x21 = subgrid (x20, x18);
	var x22 = vmirror (x21);
	var x23 = objects (x22, F, T, T);
	var x24 = first (x23);
	var x25 = objects (x22, T, T, T);
	var x26 = color (x7);
	var x27 = matcher (color, x26);
	var x28 = extract (x25, x27);
	var x29 = center (x28);
	var x30 = subtract (x8, x29);
	var x31 = shift (x24, x30);
	var O = paint (x18, x31);
	return O;
};
export var solve_508bd3b6 = function (I) {
	var x1 = width (I);
	var x2 = objects (I, T, T, T);
	var x3 = argmin (x2, size);
	var x4 = argmax (x2, size);
	var x5 = ulcorner (x3);
	var x6 = urcorner (x3);
	var x7 = index (I, x5);
	var x8 = equality (x7, EIGHT);
	var x9 = branch (x8, x5, x6);
	var x10 = branch (x8, UNITY, DOWN_LEFT);
	var x11 = multiply (x10, x1);
	var x12 = double (x11);
	var x13 = add (x9, x12);
	var x14 = subtract (x9, x12);
	var x15 = connect (x13, x14);
	var x16 = fill (I, THREE, x15);
	var x17 = paint (x16, x4);
	var x18 = objects (x17, T, F, T);
	var x19 = rbind (adjacent, x4);
	var x20 = extract (x18, x19);
	var x21 = first (x20);
	var x22 = last (x21);
	var x23 = flip (x8);
	var x24 = branch (x23, UNITY, DOWN_LEFT);
	var x25 = multiply (x24, x1);
	var x26 = double (x25);
	var x27 = add (x22, x26);
	var x28 = subtract (x22, x26);
	var x29 = connect (x27, x28);
	var x30 = fill (x17, THREE, x29);
	var x31 = paint (x30, x3);
	var O = paint (x31, x4);
	return O;
};
export var solve_6d0160f0 = function (I) {
	var x1 = ofcolor (I, FOUR);
	var x2 = first (x1);
	var x3 = first (x2);
	var x4 = last (x2);
	var x5 = greater (x3, THREE);
	var x6 = greater (x3, SEVEN);
	var x7 = greater (x4, THREE);
	var x8 = greater (x4, SEVEN);
	var x9 = branch (x5, FOUR, ZERO);
	var x10 = branch (x6, EIGHT, x9);
	var x11 = branch (x7, FOUR, ZERO);
	var x12 = branch (x8, EIGHT, x11);
	var x13 = astuple (x10, x12);
	var x14 = initset (ZERO);
	var x15 = insert (FOUR, x14);
	var x16 = insert (EIGHT, x15);
	var x17 = product (x16, x16);
	var x18 = crop (I, ORIGIN, THREE_BY_THREE);
	var x19 = asindices (x18);
	var x20 = recolor (ZERO, x19);
	var x21 = lbind (shift, x20);
	var x22 = mapply (x21, x17);
	var x23 = paint (I, x22);
	var x24 = crop (I, x13, THREE_BY_THREE);
	var x25 = py_replace (x24, FIVE, ZERO);
	var x26 = ofcolor (x25, FOUR);
	var x27 = first (x26);
	var x28 = asindices (x25);
	var x29 = toobject (x28, x25);
	var x30 = multiply (x27, FOUR);
	var x31 = shift (x29, x30);
	var O = paint (x23, x31);
	return O;
};
export var solve_f8a8fe49 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = py_replace (I, FIVE, ZERO);
	var x3 = colorfilter (x1, TWO);
	var x4 = first (x3);
	var x5 = portrait (x4);
	var x6 = branch (x5, hsplit, vsplit);
	var x7 = branch (x5, vmirror, hmirror);
	var x8 = ofcolor (I, TWO);
	var x9 = subgrid (x8, I);
	var x10 = trim (x9);
	var x11 = x7 (x10);
	var x12 = x6 (x11, TWO);
	var x13 = compose (normalize, asobject);
	var x14 = apply (x13, x12);
	var x15 = last (x14);
	var x16 = first (x14);
	var x17 = ulcorner (x8);
	var x18 = increment (x17);
	var x19 = shift (x15, x18);
	var x20 = shift (x16, x18);
	var x21 = branch (x5, width, height);
	var x22 = branch (x5, tojvec, toivec);
	var x23 = x21 (x15);
	var x24 = double (x23);
	var x25 = compose (x22, increment);
	var x26 = x25 (x23);
	var x27 = invert (x26);
	var x28 = x25 (x24);
	var x29 = shift (x19, x27);
	var x30 = shift (x20, x28);
	var x31 = paint (x2, x29);
	var O = paint (x31, x30);
	return O;
};
export var solve_d07ae81c = function (I) {
	var x1 = objects (I, T, F, F);
	var x2 = sizefilter (x1, ONE);
	var x3 = apply (color, x2);
	var x4 = difference (x1, x2);
	var x5 = apply (color, x4);
	var x6 = first (x5);
	var x7 = last (x5);
	var x8 = ofcolor (I, x6);
	var x9 = ofcolor (I, x7);
	var x10 = rbind (shoot, UNITY);
	var x11 = rbind (shoot, NEG_UNITY);
	var x12 = rbind (shoot, DOWN_LEFT);
	var x13 = rbind (shoot, UP_RIGHT);
	var x14 = fork (combine, x10, x11);
	var x15 = fork (combine, x12, x13);
	var x16 = fork (combine, x14, x15);
	var x17 = compose (x16, center);
	var x18 = mapply (x17, x2);
	var x19 = intersection (x8, x18);
	var x20 = intersection (x9, x18);
	var x21 = first (x2);
	var x22 = color (x21);
	var x23 = center (x21);
	var x24 = neighbors (x23);
	var x25 = toobject (x24, I);
	var x26 = mostcolor (x25);
	var x27 = other (x3, x22);
	var x28 = equality (x26, x6);
	var x29 = branch (x28, x22, x27);
	var x30 = branch (x28, x27, x22);
	var x31 = fill (I, x29, x19);
	var O = fill (x31, x30, x20);
	return O;
};
export var solve_6a1e5592 = function (I) {
	var x1 = width (I);
	var x2 = objects (I, T, F, T);
	var x3 = astuple (FIVE, x1);
	var x4 = crop (I, ORIGIN, x3);
	var x5 = colorfilter (x2, FIVE);
	var x6 = merge (x5);
	var x7 = cover (I, x6);
	var x8 = compose (toindices, normalize);
	var x9 = apply (x8, x5);
	var x10 = asindices (x4);
	var x11 = ofcolor (x4, ZERO);
	var x12 = ofcolor (x4, TWO);
	var x13 = rbind (multiply, TEN);
	var x14 = rbind (multiply, EIGHT);
	var x15 = rbind (intersection, x12);
	var x16 = rbind (intersection, x11);
	var x17 = rbind (intersection, x10);
	var x18 = chain (x13, size, x15);
	var x19 = chain (size, x16, delta);
	var x20 = compose (x14, uppermost);
	var x21 = chain (size, x16, outbox);
	var x22 = chain (x13, size, x17);
	var x23 = compose (invert, x18);
	var x24 = fork (add, x22, x23);
	var x25 = fork (subtract, x24, x21);
	var x26 = fork (subtract, x25, x20);
	var x27 = fork (subtract, x26, x19);
	var x28 = rbind (apply, x10);
	var x29 = lbind (lbind, shift);
	var x30 = rbind (argmax, x27);
	var x31 = chain (x30, x28, x29);
	var x32 = mapply (x31, x9);
	var O = fill (x7, ONE, x32);
	return O;
};
export var solve_0e206a2e = function (I) {
	var x1 = palette (I);
	var x2 = objects (I, F, F, T);
	var x3 = rbind (greater, ONE);
	var x4 = compose (x3, numcolors);
	var x5 = sfilter (x2, x4);
	var x6 = remove (ZERO, x1);
	var x7 = lbind (colorcount, I);
	var x8 = argmax (x6, x7);
	var x9 = remove (x8, x6);
	var x10 = rbind (contained, x9);
	var x11 = compose (x10, first);
	var x12 = rbind (sfilter, x11);
	var x13 = lbind (rbind, subtract);
	var x14 = lbind (occurrences, I);
	var x15 = lbind (lbind, shift);
	var x16 = compose (x13, ulcorner);
	var x17 = chain (x16, x12, normalize);
	var x18 = chain (x14, x12, normalize);
	var x19 = fork (apply, x17, x18);
	var x20 = compose (x15, normalize);
	var x21 = fork (mapply, x20, x19);
	var x22 = astuple (cmirror, dmirror);
	var x23 = astuple (hmirror, vmirror);
	var x24 = combine (x22, x23);
	var x25 = product (x24, x24);
	var x26 = fork (compose, first, last);
	var x27 = apply (x26, x25);
	var x28 = totuple (x27);
	var x29 = combine (x24, x28);
	var x30 = lbind (rapply, x29);
	var x31 = mapply (x30, x5);
	var x32 = mapply (x21, x31);
	var x33 = paint (I, x32);
	var x34 = merge (x5);
	var O = cover (x33, x34);
	return O;
};
export var solve_d22278a0 = function (I) {
	var x1 = asindices (I);
	var x2 = objects (I, T, F, T);
	var x3 = fork (multiply, sign, identity);
	var x4 = lbind (apply, x3);
	var x5 = chain (even, maximum, x4);
	var x6 = lbind (sfilter, x1);
	var x7 = fork (add, first, last);
	var x8 = rbind (remove, x2);
	var x9 = compose (center, last);
	var x10 = fork (subtract, first, x9);
	var x11 = compose (x5, x10);
	var x12 = lbind (rbind, equality);
	var x13 = lbind (argmin, x2);
	var x14 = chain (x7, x4, x10);
	var x15 = lbind (lbind, astuple);
	var x16 = lbind (rbind, astuple);
	var x17 = lbind (compose, x11);
	var x18 = lbind (compose, x14);
	var x19 = compose (x18, x15);
	var x20 = compose (x18, x16);
	var x21 = compose (x13, x19);
	var x22 = rbind (compose, x21);
	var x23 = lbind (lbind, valmin);
	var x24 = rbind (compose, x19);
	var x25 = chain (x24, x23, x8);
	var x26 = lbind (fork, greater);
	var x27 = fork (x26, x25, x20);
	var x28 = chain (x6, x17, x16);
	var x29 = chain (x6, x22, x12);
	var x30 = fork (intersection, x28, x29);
	var x31 = compose (x6, x27);
	var x32 = fork (intersection, x30, x31);
	var x33 = fork (recolor, color, x32);
	var x34 = mapply (x33, x2);
	var O = paint (I, x34);
	return O;
};
export var solve_4290ef0e = function (I) {
	var x1 = mostcolor (I);
	var x2 = fgpartition (I);
	var x3 = objects (I, T, F, T);
	var x4 = rbind (valmax, width);
	var x5 = lbind (colorfilter, x3);
	var x6 = chain (x4, x5, color);
	var x7 = compose (maximum, shape);
	var x8 = fork (add, x7, x6);
	var x9 = compose (invert, x8);
	var x10 = order (x2, x9);
	var x11 = rbind (argmin, centerofmass);
	var x12 = compose (initset, vmirror);
	var x13 = fork (insert, dmirror, x12);
	var x14 = fork (insert, cmirror, x13);
	var x15 = fork (insert, hmirror, x14);
	var x16 = compose (x11, x15);
	var x17 = apply (x16, x10);
	var x18 = size (x2);
	var x19 = apply (size, x2);
	var x20 = contained (ONE, x19);
	var x21 = increment (x18);
	var x22 = branch (x20, x18, x21);
	var x23 = double (x22);
	var x24 = decrement (x23);
	var x25 = apply (normalize, x17);
	var x26 = interval (ZERO, x22, ONE);
	var x27 = pair (x26, x26);
	var x28 = mpapply (shift, x25, x27);
	var x29 = astuple (x24, x24);
	var x30 = canvas (x1, x29);
	var x31 = paint (x30, x28);
	var x32 = rot90 (x31);
	var x33 = paint (x32, x28);
	var x34 = rot90 (x33);
	var x35 = paint (x34, x28);
	var x36 = rot90 (x35);
	var O = paint (x36, x28);
	return O;
};
export var solve_50846271 = function (I) {
	var x1 = ofcolor (I, TWO);
	var x2 = prapply (connect, x1, x1);
	var x3 = lbind (greater, SIX);
	var x4 = compose (x3, size);
	var x5 = fork (either, vline, hline);
	var x6 = fork (both, x4, x5);
	var x7 = mfilter (x2, x6);
	var x8 = fill (I, TWO, x7);
	var x9 = objects (x8, T, F, F);
	var x10 = colorfilter (x9, TWO);
	var x11 = valmax (x10, width);
	var x12 = halve (x11);
	var x13 = toivec (x12);
	var x14 = tojvec (x12);
	var x15 = rbind (add, ZERO_BY_TWO);
	var x16 = rbind (add, TWO_BY_ZERO);
	var x17 = rbind (subtract, ZERO_BY_TWO);
	var x18 = rbind (subtract, TWO_BY_ZERO);
	var x19 = rbind (colorcount, TWO);
	var x20 = rbind (toobject, x8);
	var x21 = compose (initset, x15);
	var x22 = fork (insert, x16, x21);
	var x23 = fork (insert, x17, x22);
	var x24 = fork (insert, x18, x23);
	var x25 = fork (combine, dneighbors, x24);
	var x26 = chain (x19, x20, x25);
	var x27 = rbind (argmax, x26);
	var x28 = compose (x27, toindices);
	var x29 = apply (x28, x10);
	var x30 = rbind (add, x13);
	var x31 = rbind (subtract, x13);
	var x32 = rbind (add, x14);
	var x33 = rbind (subtract, x14);
	var x34 = fork (connect, x30, x31);
	var x35 = fork (connect, x32, x33);
	var x36 = fork (combine, x34, x35);
	var x37 = mapply (x36, x29);
	var x38 = fill (x8, EIGHT, x37);
	var O = fill (x38, TWO, x1);
	return O;
};
export var solve_b527c5c6 = function (I) {
	var x1 = objects (I, F, F, T);
	var x2 = matcher (first, TWO);
	var x3 = rbind (sfilter, x2);
	var x4 = compose (lowermost, x3);
	var x5 = compose (rightmost, x3);
	var x6 = compose (uppermost, x3);
	var x7 = compose (leftmost, x3);
	var x8 = fork (equality, x4, lowermost);
	var x9 = fork (equality, x5, rightmost);
	var x10 = fork (equality, x6, uppermost);
	var x11 = fork (equality, x7, leftmost);
	var x12 = compose (invert, x10);
	var x13 = compose (invert, x11);
	var x14 = fork (add, x12, x8);
	var x15 = fork (add, x13, x9);
	var x16 = fork (astuple, x14, x15);
	var x17 = compose (center, x3);
	var x18 = fork (shoot, x17, x16);
	var x19 = mapply (x18, x1);
	var x20 = fill (I, TWO, x19);
	var x21 = compose (vline, x18);
	var x22 = sfilter (x1, x21);
	var x23 = difference (x1, x22);
	var x24 = chain (decrement, minimum, shape);
	var x25 = compose (increment, x24);
	var x26 = compose (invert, x24);
	var x27 = rbind (interval, ONE);
	var x28 = fork (x27, x26, x25);
	var x29 = lbind (apply, toivec);
	var x30 = lbind (apply, tojvec);
	var x31 = lbind (lbind, shift);
	var x32 = compose (x31, x18);
	var x33 = compose (x29, x28);
	var x34 = compose (x30, x28);
	var x35 = fork (mapply, x32, x33);
	var x36 = fork (mapply, x32, x34);
	var x37 = mapply (x35, x23);
	var x38 = mapply (x36, x22);
	var x39 = combine (x37, x38);
	var O = underfill (x20, THREE, x39);
	return O;
};
export var solve_150deff5 = function (I) {
	var x1 = canvas (FIVE, TWO_BY_TWO);
	var x2 = asobject (x1);
	var x3 = occurrences (I, x2);
	var x4 = lbind (shift, x2);
	var x5 = mapply (x4, x3);
	var x6 = fill (I, EIGHT, x5);
	var x7 = canvas (FIVE, UNITY);
	var x8 = astuple (TWO, ONE);
	var x9 = canvas (EIGHT, x8);
	var x10 = vconcat (x9, x7);
	var x11 = asobject (x10);
	var x12 = occurrences (x6, x11);
	var x13 = lbind (shift, x11);
	var x14 = mapply (x13, x12);
	var x15 = fill (x6, TWO, x14);
	var x16 = astuple (ONE, THREE);
	var x17 = canvas (FIVE, x16);
	var x18 = asobject (x17);
	var x19 = occurrences (x15, x18);
	var x20 = lbind (shift, x18);
	var x21 = mapply (x20, x19);
	var x22 = fill (x15, TWO, x21);
	var x23 = hmirror (x10);
	var x24 = asobject (x23);
	var x25 = occurrences (x22, x24);
	var x26 = lbind (shift, x24);
	var x27 = mapply (x26, x25);
	var x28 = fill (x22, TWO, x27);
	var x29 = dmirror (x10);
	var x30 = asobject (x29);
	var x31 = occurrences (x28, x30);
	var x32 = lbind (shift, x30);
	var x33 = mapply (x32, x31);
	var x34 = fill (x28, TWO, x33);
	var x35 = vmirror (x29);
	var x36 = asobject (x35);
	var x37 = occurrences (x34, x36);
	var x38 = lbind (shift, x36);
	var x39 = mapply (x38, x37);
	var O = fill (x34, TWO, x39);
	return O;
};
export var solve_b7249182 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = merge (x1);
	var x3 = portrait (x2);
	var x4 = branch (x3, identity, dmirror);
	var x5 = x4 (I);
	var x6 = objects (x5, T, F, T);
	var x7 = order (x6, uppermost);
	var x8 = first (x7);
	var x9 = last (x7);
	var x10 = color (x8);
	var x11 = color (x9);
	var x12 = compose (first, toindices);
	var x13 = x12 (x8);
	var x14 = x12 (x9);
	var x15 = connect (x13, x14);
	var x16 = centerofmass (x15);
	var x17 = connect (x13, x16);
	var x18 = fill (x5, x11, x15);
	var x19 = fill (x18, x10, x17);
	var x20 = add (x16, DOWN);
	var x21 = initset (x16);
	var x22 = insert (x20, x21);
	var x23 = toobject (x22, x19);
	var x24 = astuple (ZERO, NEG_TWO);
	var x25 = shift (x23, ZERO_BY_TWO);
	var x26 = shift (x23, x24);
	var x27 = combine (x25, x26);
	var x28 = ulcorner (x27);
	var x29 = urcorner (x27);
	var x30 = connect (x28, x29);
	var x31 = shift (x30, UP);
	var x32 = llcorner (x27);
	var x33 = lrcorner (x27);
	var x34 = connect (x32, x33);
	var x35 = shift (x34, DOWN);
	var x36 = paint (x19, x27);
	var x37 = fill (x36, x10, x31);
	var x38 = fill (x37, x11, x35);
	var x39 = cover (x38, x22);
	var O = x4 (x39);
	return O;
};
export var solve_9d9215db = function (I) {
	var x1 = rot90 (I);
	var x2 = rot180 (I);
	var x3 = rot270 (I);
	var x4 = initset (I);
	var x5 = chain (numcolors, lefthalf, tophalf);
	var x6 = insert (x1, x4);
	var x7 = insert (x2, x6);
	var x8 = insert (x3, x7);
	var x9 = argmax (x8, x5);
	var x10 = vmirror (x9);
	var x11 = papply (pair, x9, x10);
	var x12 = lbind (apply, maximum);
	var x13 = apply (x12, x11);
	var x14 = partition (x13);
	var x15 = sizefilter (x14, FOUR);
	var x16 = apply (llcorner, x15);
	var x17 = apply (lrcorner, x15);
	var x18 = combine (x16, x17);
	var x19 = cover (x13, x18);
	var x20 = tojvec (NEG_TWO);
	var x21 = rbind (add, ZERO_BY_TWO);
	var x22 = rbind (add, x20);
	var x23 = compose (x21, ulcorner);
	var x24 = compose (x22, urcorner);
	var x25 = fork (connect, x23, x24);
	var x26 = compose (even, last);
	var x27 = rbind (sfilter, x26);
	var x28 = chain (normalize, x27, x25);
	var x29 = fork (shift, x28, x23);
	var x30 = fork (recolor, color, x29);
	var x31 = mapply (x30, x15);
	var x32 = paint (x19, x31);
	var x33 = rot90 (x32);
	var x34 = rot180 (x32);
	var x35 = rot270 (x32);
	var x36 = papply (pair, x32, x33);
	var x37 = apply (x12, x36);
	var x38 = papply (pair, x37, x34);
	var x39 = apply (x12, x38);
	var x40 = papply (pair, x39, x35);
	var O = apply (x12, x40);
	return O;
};
export var solve_6855a6e4 = function (I) {
	var x1 = fgpartition (I);
	var x2 = rot90 (I);
	var x3 = colorfilter (x1, TWO);
	var x4 = first (x3);
	var x5 = portrait (x4);
	var x6 = branch (x5, I, x2);
	var x7 = objects (x6, T, F, T);
	var x8 = colorfilter (x7, FIVE);
	var x9 = apply (center, x8);
	var x10 = valmin (x9, first);
	var x11 = compose (first, center);
	var x12 = matcher (x11, x10);
	var x13 = compose (flip, x12);
	var x14 = extract (x8, x12);
	var x15 = extract (x8, x13);
	var x16 = ulcorner (x14);
	var x17 = ulcorner (x15);
	var x18 = subgrid (x14, x6);
	var x19 = subgrid (x15, x6);
	var x20 = hmirror (x18);
	var x21 = hmirror (x19);
	var x22 = ofcolor (x20, FIVE);
	var x23 = recolor (FIVE, x22);
	var x24 = ofcolor (x21, FIVE);
	var x25 = recolor (FIVE, x24);
	var x26 = height (x23);
	var x27 = height (x25);
	var x28 = add (THREE, x26);
	var x29 = add (THREE, x27);
	var x30 = toivec (x28);
	var x31 = toivec (x29);
	var x32 = add (x16, x30);
	var x33 = subtract (x17, x31);
	var x34 = shift (x23, x32);
	var x35 = shift (x25, x33);
	var x36 = merge (x8);
	var x37 = cover (x6, x36);
	var x38 = paint (x37, x34);
	var x39 = paint (x38, x35);
	var x40 = rot270 (x39);
	var O = branch (x5, x39, x40);
	return O;
};
export var solve_264363fd = function (I) {
	var x1 = objects (I, F, F, T);
	var x2 = argmin (x1, size);
	var x3 = normalize (x2);
	var x4 = height (x2);
	var x5 = width (x2);
	var x6 = equality (x4, FIVE);
	var x7 = equality (x5, FIVE);
	var x8 = astuple (x6, x7);
	var x9 = add (UNITY, x8);
	var x10 = invert (x9);
	var x11 = center (x2);
	var x12 = index (I, x11);
	var x13 = branch (x6, UP, RIGHT);
	var x14 = add (x13, x11);
	var x15 = index (I, x14);
	var x16 = astuple (x12, ORIGIN);
	var x17 = initset (x16);
	var x18 = cover (I, x2);
	var x19 = mostcolor (x18);
	var x20 = ofcolor (x18, x19);
	var x21 = occurrences (x18, x17);
	var x22 = objects (x18, F, F, T);
	var x23 = rbind (occurrences, x17);
	var x24 = rbind (subgrid, x18);
	var x25 = compose (x23, x24);
	var x26 = lbind (mapply, vfrontier);
	var x27 = lbind (mapply, hfrontier);
	var x28 = compose (x26, x25);
	var x29 = compose (x27, x25);
	var x30 = branch (x6, x28, x29);
	var x31 = branch (x7, x29, x28);
	var x32 = fork (combine, x30, x31);
	var x33 = lbind (recolor, x15);
	var x34 = compose (x33, x32);
	var x35 = fork (paint, x24, x34);
	var x36 = compose (asobject, x35);
	var x37 = fork (shift, x36, ulcorner);
	var x38 = mapply (x37, x22);
	var x39 = paint (x18, x38);
	var x40 = shift (x3, x10);
	var x41 = lbind (shift, x40);
	var x42 = mapply (x41, x21);
	var x43 = paint (x39, x42);
	var O = fill (x43, x19, x20);
	return O;
};
export var solve_7df24a62 = function (I) {
	var x1 = height (I);
	var x2 = width (I);
	var x3 = ofcolor (I, ONE);
	var x4 = ofcolor (I, FOUR);
	var x5 = ulcorner (x3);
	var x6 = subgrid (x3, I);
	var x7 = rot90 (x6);
	var x8 = rot180 (x6);
	var x9 = rot270 (x6);
	var x10 = matcher (size, ZERO);
	var x11 = rbind (ofcolor, ONE);
	var x12 = compose (normalize, x11);
	var x13 = rbind (ofcolor, FOUR);
	var x14 = rbind (shift, x5);
	var x15 = compose (x14, x13);
	var x16 = lbind (subtract, x1);
	var x17 = chain (increment, x16, height);
	var x18 = lbind (subtract, x2);
	var x19 = chain (increment, x18, width);
	var x20 = rbind (interval, ONE);
	var x21 = lbind (x20, ZERO);
	var x22 = compose (x21, x17);
	var x23 = compose (x21, x19);
	var x24 = fork (product, x22, x23);
	var x25 = rbind (shift, NEG_UNITY);
	var x26 = lbind (lbind, shift);
	var x27 = chain (x26, x25, x12);
	var x28 = astuple (x6, x7);
	var x29 = astuple (x8, x9);
	var x30 = combine (x28, x29);
	var x31 = apply (x15, x30);
	var x32 = lbind (difference, x4);
	var x33 = apply (x32, x31);
	var x34 = apply (normalize, x31);
	var x35 = apply (x24, x34);
	var x36 = lbind (rbind, difference);
	var x37 = apply (x26, x34);
	var x38 = apply (x36, x33);
	var x39 = papply (compose, x38, x37);
	var x40 = lbind (compose, x10);
	var x41 = apply (x40, x39);
	var x42 = papply (sfilter, x35, x41);
	var x43 = apply (x27, x30);
	var x44 = mpapply (mapply, x43, x42);
	var O = fill (I, ONE, x44);
	return O;
};
export var solve_f15e1fac = function (I) {
	var x1 = ofcolor (I, TWO);
	var x2 = portrait (x1);
	var x3 = branch (x2, identity, dmirror);
	var x4 = x3 (I);
	var x5 = leftmost (x1);
	var x6 = equality (x5, ZERO);
	var x7 = branch (x6, identity, vmirror);
	var x8 = x7 (x4);
	var x9 = ofcolor (x8, EIGHT);
	var x10 = uppermost (x9);
	var x11 = equality (x10, ZERO);
	var x12 = branch (x11, identity, hmirror);
	var x13 = x12 (x8);
	var x14 = ofcolor (x13, EIGHT);
	var x15 = ofcolor (x13, TWO);
	var x16 = rbind (shoot, DOWN);
	var x17 = mapply (x16, x14);
	var x18 = height (x13);
	var x19 = apply (first, x15);
	var x20 = insert (ZERO, x19);
	var x21 = insert (x18, x19);
	var x22 = apply (decrement, x21);
	var x23 = order (x20, identity);
	var x24 = order (x22, identity);
	var x25 = size (x15);
	var x26 = increment (x25);
	var x27 = interval (ZERO, x26, ONE);
	var x28 = apply (tojvec, x27);
	var x29 = pair (x23, x24);
	var x30 = lbind (sfilter, x17);
	var x31 = compose (first, last);
	var x32 = chain (decrement, first, first);
	var x33 = fork (greater, x31, x32);
	var x34 = chain (increment, last, first);
	var x35 = fork (greater, x34, x31);
	var x36 = fork (both, x33, x35);
	var x37 = lbind (lbind, astuple);
	var x38 = lbind (compose, x36);
	var x39 = chain (x30, x38, x37);
	var x40 = apply (x39, x29);
	var x41 = papply (shift, x40, x28);
	var x42 = merge (x41);
	var x43 = fill (x13, EIGHT, x42);
	var x44 = chain (x3, x7, x12);
	var O = x44 (x43);
	return O;
};
export var solve_234bbc79 = function (I) {
	var x1 = rbind (objects, T);
	var x2 = rbind (x1, F);
	var x3 = rbind (x2, F);
	var x4 = rbind (argmin, leftmost);
	var x5 = compose (x4, x3);
	var x6 = fork (remove, x5, x3);
	var x7 = compose (x4, x6);
	var x8 = compose (last, last);
	var x9 = matcher (first, FIVE);
	var x10 = rbind (sfilter, x9);
	var x11 = fork (difference, identity, x10);
	var x12 = rbind (argmin, x8);
	var x13 = compose (x12, x10);
	var x14 = compose (last, x13);
	var x15 = rbind (add, RIGHT);
	var x16 = compose (x14, x7);
	var x17 = compose (x14, x5);
	var x18 = fork (subtract, x16, x17);
	var x19 = compose (invert, x18);
	var x20 = compose (x15, x19);
	var x21 = compose (mostcolor, x11);
	var x22 = fork (astuple, x21, x14);
	var x23 = fork (remove, x13, identity);
	var x24 = fork (insert, x22, x23);
	var x25 = compose (x24, x7);
	var x26 = fork (cover, identity, x25);
	var x27 = fork (shift, x25, x20);
	var x28 = fork (paint, x26, x27);
	var x29 = rbind (argmax, x8);
	var x30 = chain (first, x29, x11);
	var x31 = fork (recolor, x30, x10);
	var x32 = fork (combine, x11, x31);
	var x33 = compose (x32, x5);
	var x34 = fork (paint, x28, x33);
	var x35 = x34 (I);
	var x36 = x34 (x35);
	var x37 = palette (x36);
	var x38 = contained (FIVE, x37);
	var x39 = branch (x38, x34, identity);
	var x40 = x39 (x36);
	var x41 = x3 (x40);
	var x42 = merge (x41);
	var x43 = width (x42);
	var x44 = astuple (THREE, x43);
	var O = crop (x40, ORIGIN, x44);
	return O;
};
export var solve_22233c11 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = objects (I, T, T, T);
	var x3 = first (x1);
	var x4 = recolor (EIGHT, x3);
	var x5 = normalize (x4);
	var x6 = totuple (x2);
	var x7 = apply (width, x6);
	var x8 = lbind (index, I);
	var x9 = rbind (equality, ZERO);
	var x10 = chain (flip, x9, x8);
	var x11 = apply (urcorner, x6);
	var x12 = apply (x10, x11);
	var x13 = first (x7);
	var x14 = halve (x13);
	var x15 = pair (x6, x12);
	var x16 = sfilter (x15, last);
	var x17 = apply (first, x16);
	var x18 = apply (flip, x12);
	var x19 = pair (x6, x18);
	var x20 = sfilter (x19, last);
	var x21 = apply (first, x20);
	var x22 = apply (urcorner, x21);
	var x23 = invert (x14);
	var x24 = astuple (x23, ONE);
	var x25 = lbind (add, x24);
	var x26 = apply (x25, x22);
	var x27 = lbind (shift, x5);
	var x28 = mapply (x27, x26);
	var x29 = apply (llcorner, x21);
	var x30 = astuple (ONE, x23);
	var x31 = lbind (add, x30);
	var x32 = apply (x31, x29);
	var x33 = mapply (x27, x32);
	var x34 = apply (ulcorner, x17);
	var x35 = astuple (x23, x23);
	var x36 = lbind (add, x35);
	var x37 = apply (x36, x34);
	var x38 = mapply (x27, x37);
	var x39 = apply (lrcorner, x17);
	var x40 = lbind (add, UNITY);
	var x41 = apply (x40, x39);
	var x42 = mapply (x27, x41);
	var x43 = paint (I, x28);
	var x44 = paint (x43, x33);
	var x45 = paint (x44, x38);
	var O = paint (x45, x42);
	return O;
};
export var solve_2dd70a9a = function (I) {
	var x1 = ofcolor (I, TWO);
	var x2 = ofcolor (I, THREE);
	var x3 = vline (x1);
	var x4 = vline (x2);
	var x5 = center (x1);
	var x6 = branch (x4, uppermost, rightmost);
	var x7 = x6 (x1);
	var x8 = x6 (x2);
	var x9 = greater (x7, x8);
	var x10 = both (x4, x9);
	var x11 = branch (x10, lowermost, uppermost);
	var x12 = x11 (x2);
	var x13 = branch (x4, leftmost, rightmost);
	var x14 = x13 (x2);
	var x15 = astuple (x12, x14);
	var x16 = other (x2, x15);
	var x17 = subtract (x15, x16);
	var x18 = shoot (x15, x17);
	var x19 = underfill (I, ONE, x18);
	var x20 = objects (x19, T, F, F);
	var x21 = colorfilter (x20, ONE);
	var x22 = rbind (adjacent, x2);
	var x23 = sfilter (x21, x22);
	var x24 = difference (x21, x23);
	var x25 = merge (x24);
	var x26 = cover (x19, x25);
	var x27 = shoot (x5, DOWN);
	var x28 = shoot (x5, UP);
	var x29 = shoot (x5, LEFT);
	var x30 = shoot (x5, RIGHT);
	var x31 = combine (x27, x28);
	var x32 = combine (x29, x30);
	var x33 = branch (x3, x31, x32);
	var x34 = ofcolor (x26, ONE);
	var x35 = initset (x15);
	var x36 = rbind (manhattan, x35);
	var x37 = compose (x36, initset);
	var x38 = argmax (x34, x37);
	var x39 = initset (x38);
	var x40 = gravitate (x39, x33);
	var x41 = crement (x40);
	var x42 = add (x38, x41);
	var x43 = connect (x38, x42);
	var x44 = fill (x26, ONE, x43);
	var x45 = connect (x42, x5);
	var x46 = underfill (x44, ONE, x45);
	var O = py_replace (x46, ONE, THREE);
	return O;
};
export var solve_a64e4611 = function (I) {
	var x1 = asindices (I);
	var x2 = fork (product, identity, identity);
	var x3 = lbind (canvas, ZERO);
	var x4 = compose (asobject, x3);
	var x5 = fork (multiply, first, last);
	var x6 = compose (positive, size);
	var x7 = lbind (lbind, shift);
	var x8 = rbind (fork, x5);
	var x9 = lbind (x8, multiply);
	var x10 = lbind (chain, x6);
	var x11 = rbind (x10, x4);
	var x12 = lbind (lbind, occurrences);
	var x13 = chain (x9, x11, x12);
	var x14 = compose (x2, first);
	var x15 = compose (x13, last);
	var x16 = fork (argmax, x14, x15);
	var x17 = chain (x7, x4, x16);
	var x18 = compose (x4, x16);
	var x19 = fork (occurrences, last, x18);
	var x20 = fork (mapply, x17, x19);
	var x21 = multiply (TWO, SIX);
	var x22 = interval (THREE, x21, ONE);
	var x23 = astuple (x22, I);
	var x24 = x20 (x23);
	var x25 = fill (I, THREE, x24);
	var x26 = interval (THREE, TEN, ONE);
	var x27 = astuple (x26, x25);
	var x28 = x20 (x27);
	var x29 = fill (x25, THREE, x28);
	var x30 = astuple (x26, x29);
	var x31 = x20 (x30);
	var x32 = fill (x29, THREE, x31);
	var x33 = rbind (toobject, x32);
	var x34 = rbind (colorcount, THREE);
	var x35 = chain (x34, x33, neighbors);
	var x36 = matcher (x35, EIGHT);
	var x37 = sfilter (x1, x36);
	var x38 = fill (I, THREE, x37);
	var x39 = ofcolor (x38, ZERO);
	var x40 = rbind (bordering, x38);
	var x41 = compose (x40, initset);
	var x42 = lbind (contained, THREE);
	var x43 = rbind (toobject, x38);
	var x44 = chain (x42, palette, x43);
	var x45 = compose (x44, dneighbors);
	var x46 = fork (both, x45, x41);
	var x47 = sfilter (x39, x46);
	var O = fill (x38, THREE, x47);
	return O;
};
export var solve_7837ac64 = function (I) {
	var x1 = compress (I);
	var x2 = lbind (colorcount, x1);
	var x3 = palette (x1);
	var x4 = order (x3, x2);
	var x5 = remove (ZERO, x4);
	var x6 = last (x5);
	var x7 = py_replace (x1, x6, ZERO);
	var x8 = objects (x7, T, F, T);
	var x9 = merge (x8);
	var x10 = subgrid (x9, x7);
	var x11 = index (x10, ORIGIN);
	var x12 = vmirror (x10);
	var x13 = index (x12, ORIGIN);
	var x14 = hmirror (x10);
	var x15 = index (x14, ORIGIN);
	var x16 = vmirror (x14);
	var x17 = index (x16, ORIGIN);
	var x18 = width (x10);
	var x19 = subtract (x18, FOUR);
	var x20 = divide (x19, THREE);
	var x21 = increment (x20);
	var x22 = tojvec (x21);
	var x23 = toivec (x21);
	var x24 = index (x10, x22);
	var x25 = index (x12, x22);
	var x26 = index (x14, x22);
	var x27 = index (x16, x22);
	var x28 = index (x10, x23);
	var x29 = index (x14, x23);
	var x30 = index (x12, x23);
	var x31 = equality (x24, x25);
	var x32 = equality (x26, x27);
	var x33 = equality (x28, x29);
	var x34 = equality (x29, x30);
	var x35 = branch (x31, x24, ZERO);
	var x36 = branch (x32, x26, ZERO);
	var x37 = branch (x33, x28, ZERO);
	var x38 = branch (x34, x29, ZERO);
	var x39 = astuple (x11, x35);
	var x40 = repeat (x13, ONE);
	var x41 = combine (x39, x40);
	var x42 = astuple (x37, ZERO);
	var x43 = repeat (x38, ONE);
	var x44 = combine (x42, x43);
	var x45 = astuple (x15, x36);
	var x46 = repeat (x17, ONE);
	var x47 = combine (x45, x46);
	var x48 = astuple (x41, x44);
	var x49 = repeat (x47, ONE);
	var O = vconcat (x48, x49);
	return O;
};
export var solve_a8c38be5 = function (I) {
	var x1 = objects (I, T, F, T);
	var x2 = colorfilter (x1, FIVE);
	var x3 = argmax (x2, size);
	var x4 = subgrid (x3, I);
	var x5 = difference (x1, x2);
	var x6 = fork (equality, height, width);
	var x7 = fork (greater, width, height);
	var x8 = sfilter (x5, x6);
	var x9 = sfilter (x5, portrait);
	var x10 = sfilter (x5, x7);
	var x11 = rbind (subgrid, I);
	var x12 = chain (center, delta, normalize);
	var x13 = order (x8, x12);
	var x14 = apply (x11, x13);
	var x15 = order (x9, x12);
	var x16 = apply (x11, x15);
	var x17 = order (x10, x12);
	var x18 = apply (x11, x17);
	var x19 = first (x14);
	var x20 = remove (x19, x14);
	var x21 = first (x20);
	var x22 = remove (x21, x20);
	var x23 = first (x22);
	var x24 = last (x14);
	var x25 = last (x16);
	var x26 = first (x16);
	var x27 = last (x18);
	var x28 = first (x18);
	var x29 = astuple (ONE, TWO);
	var x30 = astuple (ONE, THREE);
	var x31 = astuple (NINE, ONE);
	var x32 = canvas (FIVE, x29);
	var x33 = canvas (FIVE, x30);
	var x34 = canvas (FIVE, x31);
	var x35 = vconcat (x24, x32);
	var x36 = vconcat (x35, x25);
	var x37 = vconcat (x36, x32);
	var x38 = vconcat (x37, x21);
	var x39 = vconcat (x27, x33);
	var x40 = vconcat (x39, x4);
	var x41 = vconcat (x40, x33);
	var x42 = vconcat (x41, x28);
	var x43 = vconcat (x23, x32);
	var x44 = vconcat (x43, x26);
	var x45 = vconcat (x44, x32);
	var x46 = vconcat (x45, x19);
	var x47 = hconcat (x38, x34);
	var x48 = hconcat (x47, x42);
	var x49 = hconcat (x48, x34);
	var O = hconcat (x49, x46);
	return O;
};
export var solve_b775ac94 = function (I) {
	var x1 = objects (I, F, T, T);
	var x2 = lbind (rbind, equality);
	var x3 = rbind (compose, first);
	var x4 = chain (x3, x2, mostcolor);
	var x5 = fork (sfilter, identity, x4);
	var x6 = fork (difference, identity, x5);
	var x7 = lbind (rbind, adjacent);
	var x8 = rbind (compose, initset);
	var x9 = chain (x8, x7, x6);
	var x10 = fork (extract, x5, x9);
	var x11 = fork (insert, x10, x6);
	var x12 = lbind (recolor, ZERO);
	var x13 = chain (x12, delta, x11);
	var x14 = fork (combine, x11, x13);
	var x15 = fork (position, x5, x6);
	var x16 = chain (toivec, first, x15);
	var x17 = chain (tojvec, last, x15);
	var x18 = fork (multiply, shape, x16);
	var x19 = fork (multiply, shape, x17);
	var x20 = fork (multiply, shape, x15);
	var x21 = fork (shift, hmirror, x18);
	var x22 = fork (shift, vmirror, x19);
	var x23 = compose (hmirror, vmirror);
	var x24 = fork (shift, x23, x20);
	var x25 = lbind (compose, x5);
	var x26 = x25 (x21);
	var x27 = x25 (x22);
	var x28 = x25 (x24);
	var x29 = compose (crement, invert);
	var x30 = lbind (compose, x29);
	var x31 = x30 (x16);
	var x32 = x30 (x17);
	var x33 = x30 (x15);
	var x34 = fork (shift, x26, x31);
	var x35 = fork (shift, x27, x32);
	var x36 = fork (shift, x28, x33);
	var x37 = lbind (index, I);
	var x38 = lbind (compose, toindices);
	var x39 = x38 (x14);
	var x40 = x38 (x34);
	var x41 = x38 (x35);
	var x42 = x38 (x36);
	var x43 = fork (intersection, x39, x40);
	var x44 = fork (intersection, x39, x41);
	var x45 = fork (intersection, x39, x42);
	var x46 = chain (x37, first, x43);
	var x47 = chain (x37, first, x44);
	var x48 = chain (x37, first, x45);
	var x49 = fork (recolor, x46, x34);
	var x50 = fork (recolor, x47, x35);
	var x51 = fork (recolor, x48, x36);
	var x52 = mapply (x49, x1);
	var x53 = mapply (x50, x1);
	var x54 = mapply (x51, x1);
	var x55 = paint (I, x52);
	var x56 = paint (x55, x53);
	var O = paint (x56, x54);
	return O;
};
export var solve_97a05b5b = function (I) {
	var x1 = objects (I, F, T, T);
	var x2 = argmax (x1, size);
	var x3 = subgrid (x2, I);
	var x4 = rbind (greater, ONE);
	var x5 = compose (x4, numcolors);
	var x6 = sfilter (x1, x5);
	var x7 = lbind (rbind, subtract);
	var x8 = py_switch (x3, TWO, ZERO);
	var x9 = lbind (occurrences, x8);
	var x10 = lbind (lbind, shift);
	var x11 = compose (x7, ulcorner);
	var x12 = matcher (first, TWO);
	var x13 = compose (flip, x12);
	var x14 = rbind (sfilter, x12);
	var x15 = rbind (sfilter, x13);
	var x16 = lbind (recolor, ZERO);
	var x17 = compose (x16, x15);
	var x18 = fork (combine, x17, x14);
	var x19 = chain (x11, x18, normalize);
	var x20 = objects (x8, T, T, T);
	var x21 = apply (toindices, x20);
	var x22 = chain (x9, x18, normalize);
	var x23 = rbind (colorcount, TWO);
	var x24 = lbind (sfilter, x21);
	var x25 = chain (size, first, x24);
	var x26 = compose (positive, size);
	var x27 = lbind (lbind, contained);
	var x28 = chain (x26, x24, x27);
	var x29 = compose (x25, x27);
	var x30 = rbind (sfilter, x28);
	var x31 = compose (x30, x22);
	var x32 = lbind (rbind, equality);
	var x33 = rbind (compose, x29);
	var x34 = chain (x33, x32, x23);
	var x35 = fork (sfilter, x31, x34);
	var x36 = fork (apply, x19, x35);
	var x37 = compose (x10, normalize);
	var x38 = fork (mapply, x37, x36);
	var x39 = astuple (cmirror, dmirror);
	var x40 = astuple (hmirror, vmirror);
	var x41 = combine (x39, x40);
	var x42 = product (x41, x41);
	var x43 = fork (compose, first, last);
	var x44 = apply (x43, x42);
	var x45 = lbind (rapply, x44);
	var x46 = mapply (x45, x6);
	var x47 = mapply (x38, x46);
	var x48 = paint (x3, x47);
	var x49 = palette (x47);
	var x50 = lbind (remove, TWO);
	var x51 = x50 (x49);
	var x52 = chain (first, x50, palette);
	var x53 = rbind (contained, x51);
	var x54 = chain (flip, x53, x52);
	var x55 = sfilter (x6, x54);
	var x56 = fork (apply, x19, x22);
	var x57 = fork (mapply, x37, x56);
	var x58 = mapply (x45, x55);
	var x59 = mapply (x57, x58);
	var O = paint (x48, x59);
	return O;
};
export var solve_3e980e27 = function (I) {
	var x1 = objects (I, F, T, T);
	var x2 = lbind (contained, TWO);
	var x3 = compose (x2, palette);
	var x4 = lbind (contained, THREE);
	var x5 = compose (x4, palette);
	var x6 = sfilter (x1, x3);
	var x7 = sfilter (x1, x5);
	var x8 = compose (positive, size);
	var x9 = x8 (x7);
	var x10 = x8 (x6);
	var x11 = both (x9, x10);
	var x12 = repeat (ZERO, ZERO);
	var x13 = rbind (subgrid, I);
	var x14 = chain (asobject, vmirror, x13);
	var x15 = matcher (first, ZERO);
	var x16 = compose (flip, x15);
	var x17 = lbind (matcher, first);
	var x18 = lbind (rbind, add);
	var x19 = rbind (argmax, size);
	var x20 = chain (x18, invert, ulcorner);
	var x21 = lbind (lbind, shift);
	var x22 = lbind (occurrences, I);
	var x23 = rbind (astuple, ORIGIN);
	var x24 = chain (x22, initset, x23);
	var x25 = branch (x9, x7, x6);
	var x26 = x19 (x25);
	var x27 = branch (x9, identity, x14);
	var x28 = branch (x9, THREE, TWO);
	var x29 = x27 (x26);
	var x30 = sfilter (x29, x16);
	var x31 = x24 (x28);
	var x32 = x17 (x28);
	var x33 = sfilter (x26, x32);
	var x34 = center (x33);
	var x35 = remove (x34, x31);
	var x36 = normalize (x30);
	var x37 = sfilter (x36, x32);
	var x38 = x20 (x37);
	var x39 = apply (x38, x35);
	var x40 = x21 (x36);
	var x41 = mapply (x40, x39);
	var x42 = paint (I, x41);
	var x43 = branch (x10, x6, x7);
	var x44 = x19 (x43);
	var x45 = branch (x9, x14, identity);
	var x46 = branch (x10, TWO, THREE);
	var x47 = x45 (x44);
	var x48 = sfilter (x47, x16);
	var x49 = x24 (x46);
	var x50 = x17 (x46);
	var x51 = sfilter (x44, x50);
	var x52 = center (x51);
	var x53 = remove (x52, x49);
	var x54 = normalize (x48);
	var x55 = sfilter (x54, x50);
	var x56 = x20 (x55);
	var x57 = apply (x56, x53);
	var x58 = branch (x11, x57, x12);
	var x59 = x21 (x54);
	var x60 = mapply (x59, x58);
	var O = paint (x42, x60);
	return O;
	var solve_626c0bcc = function (I) {
		var x1 = connect (ORIGIN, RIGHT);
		var x2 = connect (DOWN, UNITY);
		var x3 = insert (UNITY, x1);
		var x4 = insert (ORIGIN, x2);
		var x5 = insert (RIGHT, x2);
		var x6 = astuple (THREE, x3);
		var x7 = astuple (FOUR, x4);
		var x8 = astuple (TWO, x5);
		var x9 = initset (x6);
		var x10 = insert (x7, x9);
		var x11 = insert (x8, x10);
		var x12 = compose (positive, size);
		var x13 = lbind (compose, x12);
		var x14 = fork (recolor, first, last);
		var x15 = lbind (rbind, difference);
		var x16 = lbind (rbind, remove);
		var x17 = lbind (recolor, EIGHT);
		var x18 = rbind (compose, x17);
		var x19 = lbind (lbind, occurrences);
		var x20 = compose (x18, x19);
		var x21 = lbind (lbind, shift);
		var x22 = lbind (fork, apply);
		var x23 = lbind (x22, x21);
		var x24 = lbind (chain, merge);
		var x25 = rbind (x24, last);
		var x26 = rbind (apply, x11);
		var x27 = rbind (x24, merge);
		var x28 = lbind (chain, x13);
		var x29 = lbind (x28, x15);
		var x30 = lbind (fork, sfilter);
		var x31 = lbind (x30, identity);
		var x32 = lbind (compose, merge);
		var x33 = rbind (chain, last);
		var x34 = lbind (compose, x14);
		var x35 = compose (x23, x20);
		var x36 = chain (x16, x26, x25);
		var x37 = chain (x29, x27, x36);
		var x38 = chain (x32, x31, x37);
		var x39 = lbind (fork, astuple);
		var x40 = lbind (x39, first);
		var x41 = rbind (mapply, x11);
		var x42 = chain (x41, x34, x40);
		var x43 = compose (x38, x35);
		var x44 = fork (x33, x43, x35);
		var x45 = compose (x42, x44);
		var x46 = fork (paint, identity, x45);
		var x47 = power (x46, FOUR);
		var x48 = x47 (I);
		var O = py_replace (x48, EIGHT, ONE);
		return O;
	};
};

//# sourceMappingURL=allinone.map