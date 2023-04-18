// Transcrypt'ed from Python, 2023-03-06 00:46:46
import {AssertionError, AttributeError, BaseException, DeprecationWarning, Exception, IndexError, IterableError, KeyError, NotImplementedError, RuntimeWarning, StopIteration, UserWarning, ValueError, Warning, __JsIterator__, __PyIterator__, __Terminal__, __add__, __and__, __call__, __class__, __envir__, __eq__, __floordiv__, __ge__, __get__, __getcm__, __getitem__, __getslice__, __getsm__, __gt__, __i__, __iadd__, __iand__, __idiv__, __ijsmod__, __ilshift__, __imatmul__, __imod__, __imul__, __in__, __init__, __ior__, __ipow__, __irshift__, __isub__, __ixor__, __jsUsePyNext__, __jsmod__, __k__, __kwargtrans__, __le__, __lshift__, __lt__, __matmul__, __mergefields__, __mergekwargtrans__, __mod__, __mul__, __ne__, __neg__, __nest__, __or__, __pow__, __pragma__, __pyUseJsNext__, __rshift__, __setitem__, __setproperty__, __setslice__, __sort__, __specialattrib__, __sub__, __super__, __t__, __terminal__, __truediv__, __withblock__, __xor__, abs, all, any, assert, bool, bytearray, bytes, callable, chr, copy, deepcopy, delattr, dict, dir, divmod, enumerate, filter, float, getattr, hasattr, input, int, isinstance, issubclass, len, list, map, max, min, object, ord, pow, print, property, py_TypeError, py_iter, py_metatype, py_next, py_reversed, py_typeof, range, repr, round, set, setattr, sorted, str, sum, tuple, zip} from './org.transcrypt.__runtime__.js';
import {Any, Callable, Container, FrozenSet, Iterable, List, Tuple, Union} from './typing.js';
var __name__ = 'arc_types';
export var Boolean = bool;
export var Integer = int;
export var IntegerTuple = Tuple.__getitem__ ([Integer, Integer]);
export var Numerical = Union.__getitem__ ([Integer, IntegerTuple]);
export var IntegerSet = FrozenSet [Integer];
export var Grid = Tuple [Tuple [Integer]];
export var Cell = Tuple.__getitem__ ([Integer, IntegerTuple]);
export var Object = FrozenSet [Cell];
export var Objects = FrozenSet [Object];
export var Indices = FrozenSet [IntegerTuple];
export var IndicesSet = FrozenSet [Indices];
export var Patch = Union.__getitem__ ([Object, Indices]);
export var Element = Union.__getitem__ ([Object, Grid]);
export var Piece = Union.__getitem__ ([Grid, Patch]);
export var TupleTuple = Tuple [Tuple];
export var ContainerContainer = Container [Container];

//# sourceMappingURL=arc_types.map