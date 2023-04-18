// Transcrypt'ed from Python, 2023-03-06 00:46:46
import {AssertionError, AttributeError, BaseException, DeprecationWarning, Exception, IndexError, IterableError, KeyError, NotImplementedError, RuntimeWarning, StopIteration, UserWarning, ValueError, Warning, __JsIterator__, __PyIterator__, __Terminal__, __add__, __and__, __call__, __class__, __envir__, __eq__, __floordiv__, __ge__, __get__, __getcm__, __getitem__, __getslice__, __getsm__, __gt__, __i__, __iadd__, __iand__, __idiv__, __ijsmod__, __ilshift__, __imatmul__, __imod__, __imul__, __in__, __init__, __ior__, __ipow__, __irshift__, __isub__, __ixor__, __jsUsePyNext__, __jsmod__, __k__, __kwargtrans__, __le__, __lshift__, __lt__, __matmul__, __mergefields__, __mergekwargtrans__, __mod__, __mul__, __ne__, __neg__, __nest__, __or__, __pow__, __pragma__, __pyUseJsNext__, __rshift__, __setitem__, __setproperty__, __setslice__, __sort__, __specialattrib__, __sub__, __super__, __t__, __terminal__, __truediv__, __withblock__, __xor__, abs, all, any, assert, bool, bytearray, bytes, callable, chr, copy, deepcopy, delattr, dict, dir, divmod, enumerate, filter, float, getattr, hasattr, input, int, isinstance, issubclass, len, list, map, max, min, object, ord, pow, print, property, py_TypeError, py_iter, py_metatype, py_next, py_reversed, py_typeof, range, repr, round, set, setattr, sorted, str, sum, tuple, zip} from './org.transcrypt.__runtime__.js';
import {add, adjacent, apply, argmax, argmin, asindices, asobject, astuple, backdrop, bordering, both, bottomhalf, box, branch, canvas, cellwise, center, centerofmass, chain, cmirror, color, colorcount, colorfilter, combine, compose, compress, connect, contained, corners, cover, crement, crop, decrement, dedupe, delta, difference, divide, dmirror, dneighbors, double, downscale, either, equality, even, extract, fgpartition, fill, first, flip, fork, frontiers, gravitate, greater, halve, hconcat, height, hfrontier, hline, hmatching, hmirror, hperiod, hsplit, hupscale, identity, inbox, increment, index, ineighbors, initset, insert, intersection, interval, invert, last, lbind, leastcolor, leastcommon, lefthalf, leftmost, llcorner, lowermost, lrcorner, manhattan, mapply, matcher, maximum, merge, mfilter, minimum, mostcolor, mostcommon, move, mpapply, multiply, neighbors, normalize, numcolors, objects, occurrences, ofcolor, order, other, outbox, paint, pair, palette, papply, partition, portrait, position, positive, power, prapply, product, py_replace, py_switch, rapply, rbind, recolor, remove, repeat, righthalf, rightmost, rot180, rot270, rot90, sfilter, shape, shift, shoot, sign, size, sizefilter, square, subgrid, subtract, toindices, toivec, tojvec, toobject, tophalf, totuple, trim, ulcorner, underfill, underpaint, uppermost, upscale, urcorner, valmax, valmin, vconcat, vfrontier, vline, vmatching, vmirror, vperiod, vsplit, vupscale, width} from './dsl.js';
var __name__ = 'tests';
export var A = tuple ([tuple ([1, 0]), tuple ([0, 1]), tuple ([1, 0])]);
export var B = tuple ([tuple ([2, 1]), tuple ([0, 1]), tuple ([2, 1])]);
export var C = tuple ([tuple ([3, 4]), tuple ([5, 5])]);
export var D = tuple ([tuple ([1, 2, 3]), tuple ([4, 5, 6]), tuple ([7, 8, 0])]);
export var E = tuple ([tuple ([1, 2]), tuple ([4, 5])]);
export var F = tuple ([tuple ([5, 6]), tuple ([8, 0])]);
export var G = tuple ([tuple ([1, 0, 0, 0, 3]), tuple ([0, 1, 1, 0, 0]), tuple ([0, 1, 1, 2, 0]), tuple ([0, 0, 2, 2, 0]), tuple ([0, 2, 0, 0, 0])]);
export var H = tuple ([tuple ([0, 0, 0, 0, 0]), tuple ([0, 2, 0, 2, 0]), tuple ([2, 0, 0, 2, 0]), tuple ([0, 0, 0, 0, 0]), tuple ([0, 0, 2, 0, 0])]);
export var I = tuple ([tuple ([0, 0, 2, 0, 0]), tuple ([0, 2, 0, 2, 0]), tuple ([2, 0, 0, 2, 0]), tuple ([0, 2, 0, 2, 0]), tuple ([0, 0, 2, 0, 0])]);
export var J = tuple ([tuple ([0, 0, 2, 0, 0]), tuple ([0, 2, 0, 2, 0]), tuple ([0, 0, 2, 2, 0]), tuple ([0, 2, 0, 2, 0]), tuple ([0, 0, 2, 0, 0])]);
export var K = tuple ([tuple ([0, 0, 1, 0, 0, 1, 0, 0]), tuple ([0, 0, 1, 0, 0, 1, 0, 0]), tuple ([1, 1, 1, 1, 1, 1, 1, 1]), tuple ([0, 0, 1, 0, 0, 1, 0, 0]), tuple ([0, 0, 1, 0, 0, 1, 0, 0]), tuple ([1, 1, 1, 1, 1, 1, 1, 1]), tuple ([0, 0, 1, 0, 0, 1, 0, 0]), tuple ([0, 0, 1, 0, 0, 1, 0, 0])]);
export var test_identity = function () {
};
export var test_add = function () {
};
export var test_subtract = function () {
};
export var test_multiply = function () {
};
export var test_divide = function () {
};
export var test_invert = function () {
};
export var test_even = function () {
};
export var test_double = function () {
};
export var test_halve = function () {
};
export var test_flip = function () {
};
export var test_equality = function () {
};
export var test_contained = function () {
};
export var test_combine = function () {
};
export var test_intersection = function () {
};
export var test_difference = function () {
};
export var test_dedupe = function () {
};
export var test_order = function () {
};
export var test_repeat = function () {
};
export var test_greater = function () {
};
export var test_size = function () {
};
export var test_merge = function () {
};
export var test_maximum = function () {
};
export var test_minimum = function () {
};
export var test_valmax = function () {
};
export var test_valmin = function () {
};
export var test_argmax = function () {
};
export var test_argmin = function () {
};
export var test_mostcommon = function () {
};
export var test_leastcommon = function () {
};
export var test_initset = function () {
};
export var test_both = function () {
};
export var test_either = function () {
};
export var test_increment = function () {
};
export var test_decrement = function () {
};
export var test_crement = function () {
};
export var test_sign = function () {
};
export var test_positive = function () {
};
export var test_toivec = function () {
};
export var test_tojvec = function () {
};
export var test_sfilter = function () {
};
export var test_mfilter = function () {
};
export var test_extract = function () {
};
export var test_totuple = function () {
};
export var test_first = function () {
};
export var test_last = function () {
};
export var test_insert = function () {
};
export var test_remove = function () {
};
export var test_other = function () {
};
export var test_interval = function () {
};
export var test_astuple = function () {
};
export var test_product = function () {
};
export var test_pair = function () {
};
export var test_branch = function () {
};
export var test_compose = function () {
};
export var test_chain = function () {
};
export var test_matcher = function () {
};
export var test_rbind = function () {
};
export var test_lbind = function () {
};
export var test_power = function () {
};
export var test_fork = function () {
};
export var test_apply = function () {
};
export var test_rapply = function () {
};
export var test_mapply = function () {
};
export var test_papply = function () {
};
export var test_mpapply = function () {
};
export var test_prapply = function () {
};
export var test_mostcolor = function () {
};
export var test_leastcolor = function () {
};
export var test_height = function () {
};
export var test_width = function () {
};
export var test_shape = function () {
};
export var test_portrait = function () {
};
export var test_colorcount = function () {
};
export var test_colorfilter = function () {
};
export var test_sizefilter = function () {
};
export var test_asindices = function () {
};
export var test_ofcolor = function () {
};
export var test_ulcorner = function () {
};
export var test_urcorner = function () {
};
export var test_llcorner = function () {
};
export var test_lrcorner = function () {
};
export var test_crop = function () {
};
export var test_toindices = function () {
};
export var test_recolor = function () {
};
export var test_shift = function () {
};
export var test_normalize = function () {
};
export var test_dneighbors = function () {
};
export var test_ineighbors = function () {
};
export var test_neighbors = function () {
};
export var test_objects = function () {
};
export var test_partition = function () {
};
export var test_fgpartition = function () {
};
export var test_uppermost = function () {
};
export var test_lowermost = function () {
};
export var test_leftmost = function () {
};
export var test_rightmost = function () {
};
export var test_square = function () {
};
export var test_vline = function () {
};
export var test_hline = function () {
};
export var test_hmatching = function () {
};
export var test_vmatching = function () {
};
export var test_manhattan = function () {
};
export var test_adjacent = function () {
};
export var test_bordering = function () {
};
export var test_centerofmass = function () {
};
export var test_palette = function () {
};
export var test_numcolors = function () {
};
export var test_color = function () {
};
export var test_toobject = function () {
};
export var test_asobject = function () {
};
export var test_rot90 = function () {
};
export var test_rot180 = function () {
};
export var test_rot270 = function () {
};
export var test_hmirror = function () {
};
export var test_vmirror = function () {
};
export var test_dmirror = function () {
};
export var test_cmirror = function () {
};
export var test_fill = function () {
};
export var test_paint = function () {
};
export var test_underfill = function () {
};
export var test_underpaint = function () {
};
export var test_hupscale = function () {
};
export var test_vupscale = function () {
};
export var test_upscale = function () {
};
export var test_downscale = function () {
};
export var test_hconcat = function () {
};
export var test_vconcat = function () {
};
export var test_subgrid = function () {
};
export var test_hsplit = function () {
};
export var test_vsplit = function () {
};
export var test_cellwise = function () {
};
export var test_replace = function () {
};
export var test_switch = function () {
};
export var test_center = function () {
};
export var test_position = function () {
};
export var test_index = function () {
};
export var test_canvas = function () {
};
export var test_corners = function () {
};
export var test_connect = function () {
};
export var test_cover = function () {
};
export var test_trim = function () {
};
export var test_move = function () {
};
export var test_tophalf = function () {
};
export var test_bottomhalf = function () {
};
export var test_lefthalf = function () {
};
export var test_righthalf = function () {
};
export var test_vfrontier = function () {
};
export var test_hfrontier = function () {
};
export var test_backdrop = function () {
};
export var test_delta = function () {
};
export var test_gravitate = function () {
};
export var test_inbox = function () {
};
export var test_outbox = function () {
};
export var test_box = function () {
};
export var test_shoot = function () {
};
export var test_occurrences = function () {
};
export var test_frontiers = function () {
};
export var test_compress = function () {
};
export var test_hperiod = function () {
};
export var test_vperiod = function () {
};

//# sourceMappingURL=tests.map