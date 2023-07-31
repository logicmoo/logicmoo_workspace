// Transcrypt'ed from Python, 2023-03-06 00:46:46
import {AssertionError, AttributeError, BaseException, DeprecationWarning, Exception, IndexError, IterableError, KeyError, NotImplementedError, RuntimeWarning, StopIteration, UserWarning, ValueError, Warning, __JsIterator__, __PyIterator__, __Terminal__, __add__, __and__, __call__, __class__, __envir__, __eq__, __floordiv__, __ge__, __get__, __getcm__, __getitem__, __getslice__, __getsm__, __gt__, __i__, __iadd__, __iand__, __idiv__, __ijsmod__, __ilshift__, __imatmul__, __imod__, __imul__, __in__, __init__, __ior__, __ipow__, __irshift__, __isub__, __ixor__, __jsUsePyNext__, __jsmod__, __k__, __kwargtrans__, __le__, __lshift__, __lt__, __matmul__, __mergefields__, __mergekwargtrans__, __mod__, __mul__, __ne__, __neg__, __nest__, __or__, __pow__, __pragma__, __pyUseJsNext__, __rshift__, __setitem__, __setproperty__, __setslice__, __sort__, __specialattrib__, __sub__, __super__, __t__, __terminal__, __truediv__, __withblock__, __xor__, abs, all, any, assert, bool, bytearray, bytes, callable, chr, copy, deepcopy, delattr, dict, dir, divmod, enumerate, filter, float, getattr, hasattr, input, int, isinstance, issubclass, len, list, map, max, min, object, ord, pow, print, property, py_TypeError, py_iter, py_metatype, py_next, py_reversed, py_typeof, range, repr, round, set, setattr, sorted, str, sum, tuple, zip} from './org.transcrypt.__runtime__.js';
var __name__ = 'htmltree.htmltree';
export var KWElement = function (tag) {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					case 'tag': var tag = __allkwargs0__ [__attrib0__]; break;
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (1, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	if (len (content) == 1 && content [0] === null) {
		var content = null;
	}
	else {
		var content = list (content);
	}
	return HtmlElement (tag, convertAttrKeys (attrs), content);
};
export var convertAttrKeys = function (attrdict) {
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					case 'attrdict': var attrdict = __allkwargs0__ [__attrib0__]; break;
				}
			}
		}
	}
	else {
	}
	var newdict = dict ({});
	for (var [k, v] of attrdict.py_items ()) {
		var k = k.py_replace ('_', '-');
		if (k.endswith ('-')) {
			var k = k.__getslice__ (0, -(1), 1).py_replace ('_', '-');
		}
		else if (k.startswith ('-')) {
			var k = k.__getslice__ (1, null, 1);
		}
		if (k == 'style') {
			newdict [k] = convertAttrKeys (v);
		}
		else {
			newdict [k] = v;
		}
	}
	return newdict;
};
export var HtmlElement =  __class__ ('HtmlElement', [object], {
	__module__: __name__,
	get __init__ () {return __get__ (this, function (self, tagname, attrs, content) {
		if (arguments.length) {
			var __ilastarg0__ = arguments.length - 1;
			if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
				var __allkwargs0__ = arguments [__ilastarg0__--];
				for (var __attrib0__ in __allkwargs0__) {
					switch (__attrib0__) {
						case 'self': var self = __allkwargs0__ [__attrib0__]; break;
						case 'tagname': var tagname = __allkwargs0__ [__attrib0__]; break;
						case 'attrs': var attrs = __allkwargs0__ [__attrib0__]; break;
						case 'content': var content = __allkwargs0__ [__attrib0__]; break;
					}
				}
			}
		}
		else {
		}
		self.T = tagname.lower ();
		if (self.T == '!--') {
			var attrs = null;
			self.endtag = ' -->';
		}
		else {
			self.endtag = '</{}>'.format (self.T);
		}
		if (attrs !== null) {
			try {
				var _ = attrs.py_items ();
			}
			catch (__except0__) {
				if (isinstance (__except0__, AttributeError)) {
					var msg = 'attrs must be a dict-like object or None';
					var __except1__ = ValueError (msg);
					__except1__.__cause__ = null;
					throw __except1__;
				}
				else {
					throw __except0__;
				}
			}
		}
		self.A = attrs;
		self.C = content;
	});},
	get render () {return __get__ (this, function (self, indent) {
		if (typeof indent == 'undefined' || (indent != null && indent.hasOwnProperty ("__kwargtrans__"))) {;
			var indent = -(1);
		};
		if (arguments.length) {
			var __ilastarg0__ = arguments.length - 1;
			if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
				var __allkwargs0__ = arguments [__ilastarg0__--];
				for (var __attrib0__ in __allkwargs0__) {
					switch (__attrib0__) {
						case 'self': var self = __allkwargs0__ [__attrib0__]; break;
						case 'indent': var indent = __allkwargs0__ [__attrib0__]; break;
					}
				}
			}
		}
		else {
		}
		var rlist = [];
		var opentag = '<{}'.format (self.T);
		rlist.append (indented (opentag, indent));
		if (self.A !== null) {
			for (var [a, v] of self.A.py_items ()) {
				if (isinstance (v, str)) {
					rlist.append (' {}="{}"'.format (a, v));
				}
				else if (v === null) {
					rlist.append (' {}'.format (a));
				}
				else if (isinstance (v, list)) {
					var _ = ' '.join (v);
					rlist.append (' {}="{}"'.format (a, _));
				}
				else if (a == 'style') {
					rlist.append (' {}="{}"'.format (a, renderInlineStyle (v)));
				}
				else {
					var msg = "Don't know what to with attribute {}={}".format (a, v);
					var __except0__ = ValueError (msg);
					__except0__.__cause__ = null;
					throw __except0__;
				}
			}
		}
		if (self.C === null && self.T != '!--') {
			var closing = '>';
		}
		else {
			if (self.T == '!--') {
				rlist.append (' ');
			}
			else {
				rlist.append ('>');
			}
			if (isinstance (self.C, str)) {
				rlist.append (indented (self.C, indent));
			}
			else if (self.T == 'style') {
				rlist.append (renderCss (self.C, indent));
			}
			else {
				var cindent = (indent >= 0 ? indent + 1 : indent);
				for (var c of self.C) {
					if (isinstance (c, tuple ([str, int, float]))) {
						rlist.append (indented (str (c), cindent));
					}
					else {
						rlist.append (c.render (cindent));
					}
				}
			}
			var closing = indented (self.endtag, indent);
		}
		rlist.append (closing);
		return ''.join (rlist);
	});}
});
export var indented = function (contentstring, indent) {
	if (typeof indent == 'undefined' || (indent != null && indent.hasOwnProperty ("__kwargtrans__"))) {;
		var indent = -(1);
	};
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					case 'contentstring': var contentstring = __allkwargs0__ [__attrib0__]; break;
					case 'indent': var indent = __allkwargs0__ [__attrib0__]; break;
				}
			}
		}
	}
	else {
	}
	if (!(indent >= 0)) {
		return contentstring;
	}
	else {
		return '\n{}{}'.format ('  ' * indent, contentstring);
	}
};
export var renderInlineStyle = function (d) {
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					case 'd': var d = __allkwargs0__ [__attrib0__]; break;
				}
			}
		}
	}
	else {
	}
	if (isinstance (d, tuple ([str, int, float]))) {
		var result = str (d);
	}
	else {
		var style = [];
		for (var [k, v] of d.py_items ()) {
			style.append ('{}:{};'.format (k, v));
		}
		var separator = ' ';
		var result = separator.join (style);
	}
	return result;
};
export var renderCss = function (d, indent) {
	if (typeof indent == 'undefined' || (indent != null && indent.hasOwnProperty ("__kwargtrans__"))) {;
		var indent = -(1);
	};
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					case 'd': var d = __allkwargs0__ [__attrib0__]; break;
					case 'indent': var indent = __allkwargs0__ [__attrib0__]; break;
				}
			}
		}
	}
	else {
	}
	var rulesetlist = [];
	for (var [py_selector, declaration] of d.py_items ()) {
		var ruleset = ' '.join ([py_selector, '{', renderInlineStyle (declaration), '}']);
		rulesetlist.append (indented (ruleset, indent));
	}
	return ' '.join (rulesetlist);
};
export var Html = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('html', ...content, __kwargtrans__ (attrs));
};
export var Head = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('head', ...content, __kwargtrans__ (attrs));
};
export var Body = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('body', ...content, __kwargtrans__ (attrs));
};
export var Link = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
	}
	else {
	}
	return KWElement ('link', null, __kwargtrans__ (attrs));
};
export var Meta = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
	}
	else {
	}
	return KWElement ('meta', null, __kwargtrans__ (attrs));
};
export var Title = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('title', ...content, __kwargtrans__ (attrs));
};
export var Style = function () {
	var content = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: content [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete content.__kwargtrans__;
		}
	}
	else {
	}
	var _ = convertAttrKeys (content);
	var newcontent = dict ({});
	for (var [k, v] of _.py_items ()) {
		newcontent [k] = convertAttrKeys (v);
	}
	return HtmlElement ('style', dict ({}), newcontent);
};
export var Address = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('address', ...content, __kwargtrans__ (attrs));
};
export var Article = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('article', ...content, __kwargtrans__ (attrs));
};
export var Aside = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('aside', ...content, __kwargtrans__ (attrs));
};
export var Footer = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('footer', ...content, __kwargtrans__ (attrs));
};
export var Header = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('header', ...content, __kwargtrans__ (attrs));
};
export var H1 = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('h1', ...content, __kwargtrans__ (attrs));
};
export var H2 = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('h2', ...content, __kwargtrans__ (attrs));
};
export var H3 = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('h3', ...content, __kwargtrans__ (attrs));
};
export var H4 = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('h4', ...content, __kwargtrans__ (attrs));
};
export var H5 = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('h5', ...content, __kwargtrans__ (attrs));
};
export var H6 = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('h6', ...content, __kwargtrans__ (attrs));
};
export var Nav = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('nav', ...content, __kwargtrans__ (attrs));
};
export var Section = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('section', ...content, __kwargtrans__ (attrs));
};
export var Blockquote = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('blockquote', ...content, __kwargtrans__ (attrs));
};
export var Dd = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('dd', ...content, __kwargtrans__ (attrs));
};
export var Div = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('div', ...content, __kwargtrans__ (attrs));
};
export var Dl = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('dl', ...content, __kwargtrans__ (attrs));
};
export var Dt = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('dt', ...content, __kwargtrans__ (attrs));
};
export var Figcaption = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('figcaption', ...content, __kwargtrans__ (attrs));
};
export var Figure = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('figure', ...content, __kwargtrans__ (attrs));
};
export var Hr = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
	}
	else {
	}
	return KWElement ('hr', null, __kwargtrans__ (attrs));
};
export var Li = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('li', ...content, __kwargtrans__ (attrs));
};
export var Main = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('main', ...content, __kwargtrans__ (attrs));
};
export var Ol = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('ol', ...content, __kwargtrans__ (attrs));
};
export var P = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('p', ...content, __kwargtrans__ (attrs));
};
export var Pre = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('pre', ...content, __kwargtrans__ (attrs));
};
export var Ul = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('ul', ...content, __kwargtrans__ (attrs));
};
export var A = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('a', ...content, __kwargtrans__ (attrs));
};
export var B = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('b', ...content, __kwargtrans__ (attrs));
};
export var Br = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
	}
	else {
	}
	return KWElement ('br', null, __kwargtrans__ (attrs));
};
export var Cite = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('cite', ...content, __kwargtrans__ (attrs));
};
export var Code = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('code', ...content, __kwargtrans__ (attrs));
};
export var Em = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('em', ...content, __kwargtrans__ (attrs));
};
export var I = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('i', ...content, __kwargtrans__ (attrs));
};
export var S = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('s', ...content, __kwargtrans__ (attrs));
};
export var Samp = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('samp', ...content, __kwargtrans__ (attrs));
};
export var Small = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('small', ...content, __kwargtrans__ (attrs));
};
export var Span = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('span', ...content, __kwargtrans__ (attrs));
};
export var Strong = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('strong', ...content, __kwargtrans__ (attrs));
};
export var Sub = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('sub', ...content, __kwargtrans__ (attrs));
};
export var Sup = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('sup', ...content, __kwargtrans__ (attrs));
};
export var U = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('u', ...content, __kwargtrans__ (attrs));
};
export var Area = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
	}
	else {
	}
	return KWElement ('area', null, __kwargtrans__ (attrs));
};
export var Audio = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('audio', ...content, __kwargtrans__ (attrs));
};
export var Img = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
	}
	else {
	}
	return KWElement ('img', null, __kwargtrans__ (attrs));
};
export var Map = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('map', ...content, __kwargtrans__ (attrs));
};
export var Track = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
	}
	else {
	}
	return KWElement ('track', null, __kwargtrans__ (attrs));
};
export var Video = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('video', ...content, __kwargtrans__ (attrs));
};
export var Embed = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
	}
	else {
	}
	return KWElement ('embed', null, __kwargtrans__ (attrs));
};
export var Object = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('object', ...content, __kwargtrans__ (attrs));
};
export var Param = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
	}
	else {
	}
	return KWElement ('param', null, __kwargtrans__ (attrs));
};
export var Source = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
	}
	else {
	}
	return KWElement ('source', null, __kwargtrans__ (attrs));
};
export var Canvas = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('canvas', ...content, __kwargtrans__ (attrs));
};
export var Noscript = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('noscript', ...content, __kwargtrans__ (attrs));
};
export var Script = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('script', ...content, __kwargtrans__ (attrs));
};
export var Caption = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('caption', ...content, __kwargtrans__ (attrs));
};
export var Col = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
	}
	else {
	}
	return KWElement ('col', null, __kwargtrans__ (attrs));
};
export var Table = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('table', ...content, __kwargtrans__ (attrs));
};
export var Tbody = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('tbody', ...content, __kwargtrans__ (attrs));
};
export var Td = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('td', ...content, __kwargtrans__ (attrs));
};
export var Tfoot = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('tfoot', ...content, __kwargtrans__ (attrs));
};
export var Th = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('th', ...content, __kwargtrans__ (attrs));
};
export var Thead = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('thead', ...content, __kwargtrans__ (attrs));
};
export var Tr = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('tr', ...content, __kwargtrans__ (attrs));
};
export var Button = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('button', ...content, __kwargtrans__ (attrs));
};
export var Datalist = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('datalist', ...content, __kwargtrans__ (attrs));
};
export var Fieldset = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('fieldset', ...content, __kwargtrans__ (attrs));
};
export var Form = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('form', ...content, __kwargtrans__ (attrs));
};
export var Input = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
	}
	else {
	}
	return KWElement ('input', null, __kwargtrans__ (attrs));
};
export var Label = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('label', ...content, __kwargtrans__ (attrs));
};
export var Legend = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('legend', ...content, __kwargtrans__ (attrs));
};
export var Meter = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('meter', ...content, __kwargtrans__ (attrs));
};
export var Optgroup = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('optgroup', ...content, __kwargtrans__ (attrs));
};
export var Option = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('option', ...content, __kwargtrans__ (attrs));
};
export var Output = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('output', ...content, __kwargtrans__ (attrs));
};
export var Progress = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('progress', ...content, __kwargtrans__ (attrs));
};
export var Select = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('select', ...content, __kwargtrans__ (attrs));
};
export var Textarea = function () {
	var attrs = dict ();
	if (arguments.length) {
		var __ilastarg0__ = arguments.length - 1;
		if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
			var __allkwargs0__ = arguments [__ilastarg0__--];
			for (var __attrib0__ in __allkwargs0__) {
				switch (__attrib0__) {
					default: attrs [__attrib0__] = __allkwargs0__ [__attrib0__];
				}
			}
			delete attrs.__kwargtrans__;
		}
		var content = tuple ([].slice.apply (arguments).slice (0, __ilastarg0__ + 1));
	}
	else {
		var content = tuple ();
	}
	return KWElement ('textarea', ...content, __kwargtrans__ (attrs));
};

//# sourceMappingURL=htmltree.htmltree.map