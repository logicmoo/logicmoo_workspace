// Transcrypt'ed from Python, 2023-03-06 00:46:45
import {AssertionError, AttributeError, BaseException, DeprecationWarning, Exception, IndexError, IterableError, KeyError, NotImplementedError, RuntimeWarning, StopIteration, UserWarning, ValueError, Warning, __JsIterator__, __PyIterator__, __Terminal__, __add__, __and__, __call__, __class__, __envir__, __eq__, __floordiv__, __ge__, __get__, __getcm__, __getitem__, __getslice__, __getsm__, __gt__, __i__, __iadd__, __iand__, __idiv__, __ijsmod__, __ilshift__, __imatmul__, __imod__, __imul__, __in__, __init__, __ior__, __ipow__, __irshift__, __isub__, __ixor__, __jsUsePyNext__, __jsmod__, __k__, __kwargtrans__, __le__, __lshift__, __lt__, __matmul__, __mergefields__, __mergekwargtrans__, __mod__, __mul__, __ne__, __neg__, __nest__, __or__, __pow__, __pragma__, __pyUseJsNext__, __rshift__, __setitem__, __setproperty__, __setslice__, __sort__, __specialattrib__, __sub__, __super__, __t__, __terminal__, __truediv__, __withblock__, __xor__, abs, all, any, assert, bool, bytearray, bytes, callable, chr, copy, deepcopy, delattr, dict, dir, divmod, enumerate, filter, float, getattr, hasattr, input, int, isinstance, issubclass, len, list, map, max, min, object, ord, pow, print, property, py_TypeError, py_iter, py_metatype, py_next, py_reversed, py_typeof, range, repr, round, set, setattr, sorted, str, sum, tuple, zip} from './org.transcrypt.__runtime__.js';
var __name__ = 'dsl';
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

//# sourceMappingURL=dsl.map