/*

from typing import (
    List,
    Union,
    Tuple,
    Any,
    Container,
    Callable,
    FrozenSet,
    Iterable
)

Boolean = bool
Integer = int
IntegerTuple = Tuple[Integer, Integer]
Numerical = Union[Integer, IntegerTuple]
IntegerSet = FrozenSet[Integer]
Grid = Tuple[Tuple[Integer]]
Cell = Tuple[Integer, IntegerTuple]
Object = FrozenSet[Cell]
Objects = FrozenSet[Object]
Indices = FrozenSet[IntegerTuple]
IndicesSet = FrozenSet[Indices]
Patch = Union[Object, Indices]
Element = Union[Object, Grid]
Piece = Union[Grid, Patch]
TupleTuple = Tuple[Tuple]
ContainerContainer = Container[Container]

*/

fix_type_arg(_When,Var,Var,true):- is_ftVar(Var),!.
fix_type_arg(When,Var:Type,Var,ensure_type(When,Var,Type)).
fix_type_arg(When,Name=Value,Value,ensure_val(When,Name,Value)).

flatten_code(A,B):- flatten(A,B),!.

translate_program(Prop,Merged):- is_list(Prop),
  maplist(translate_call(on_enter),Prop,Call,TransIn0),flatten_code(TransIn0,TransIn1),list_to_set(TransIn1,TransIn),
  maplist(translate_call(on_leave),Prop,Call,TransOut0),flatten_code(TransOut0,TransOut1),
  reverse(TransOut1,TransOut2),list_to_set(TransOut2,TransOut3),reverse(TransOut3,TransOut),
  merge_calls(TransIn,TransOut,Merged),!.
translate_program(Prop,Prop):- !.
merge_calls(TransIn,TransOut,Merged):-
 append(TransInL,[CallIn|TransInR],TransIn),
 append(TransOutL,[CallOut|TransOutR],TransOut),
 functor(CallIn,f,_),CallIn==CallOut,
 append(TransOutL,TransInL,TransLeft0),
 list_to_set(TransLeft0,TransLeft),
 merge_calls(TransInR,TransOutR,TransR),
 append([TransLeft,CallIn,TransR],Merged).

translate_call(On_enter,Prog,Call,TransIn):-
  Prog=..[F|ARGS],
  maplist(fix_type_arg(On_enter),ARGS,NARGS,TransIn),
  Call=..[ff,F|NARGS].

% the same 
identical_impl(rot180).
identical_impl(hconcat).
identical_impl(vconcat).
identical_impl(rot270).

% michods -> dmiles
identical_impl_differing_names(vmirror,flipV).
identical_impl_differing_names(hmirror,flipH).
identical_impl_differing_names(dmirror,flipD).
identical_impl_differing_names(replace,subst001).

% michods -> dmiles
translated_call_patterns([upscale,I,Mass,O],  increase_size(Mass,I,O)).
translated_call_patterns([downscale,I,Mass,O],increase_size(Mass,O,I)).  % Prolog mades automatically bidirectional
translated_call_patterns([crop,I,TL,BR,O],crop(I,L,T,R,B,O)):- tl_br(TL,T,L),tl_br(BR,B,R).
translated_call_patterns([switch,N1,N2,O],swap_colors(C1,C2,fg,O)):- color_name(N1,C1),color_name(N2,C2).

tl_br(TL,T,L):- arg(1,TL,T),arg(2,TL,L),!.

:- dynamic(p_solve/3).

maybe_jit_one_test(TestID):-
  (var(TestID)->kaggle_arc(TestID,trn+0,_,_);true),  
  test_id_atom(TestID,TestIDA),
  clause(l_solve(TestIDA,IN,OUT),Program),
  if_t( ( \+ clause(p_solve(TestIDA,_,_), _)),
   must_det_ll(((
    translate_program(Program,Trans),
    list_to_conjuncts(Trans,Body),
    assert_if_new(p_solve(TestIDA,IN,OUT):- Body),
    pp(p_solve(TestIDA,IN,OUT):- Body))))).

into_pygrid(IO,IO).

solve(N,IN,OUT):-
  maybe_jit_one_test(N),
  fix_test_name(N,TestID),
  forall(kaggle_arc(TestID,ExampleNum,I,O),
    (once((into_pygrid(I,IN),into_pygrid(O,TOUT))),
    ignore((p_solve(N,IN,OUT),
      print_ss(TestID>ExampleNum,OUT,TOUT))))).

l_solve('67a3c6ac', IN, OUT) :-
    [f(vmirror, IN:'Piece', OUT:'Piece')].
l_solve('68b16354', IN, OUT) :-
    [f(hmirror, IN:'Piece', OUT:'Piece')].
l_solve('74dd1130', IN, OUT) :-
    [f(dmirror, IN:'Piece', OUT:'Piece')].
l_solve('3c9b0459', IN, OUT) :-
    [f(rot180, IN:'Grid', OUT:'Grid')].
l_solve('6150a2bd', IN, OUT) :-
    [f(rot180, IN:'Grid', OUT:'Grid')].
l_solve('9172f3a0', IN, OUT) :-
    [f(upscale, IN:'Element', 'THREE':'Integer', OUT:'Element')].
l_solve('9dfd6313', IN, OUT) :-
    [f(dmirror, IN:'Piece', OUT:'Piece')].
l_solve(a416b8f3, IN, OUT) :-
    [f(hconcat, IN:'Grid', IN:'Grid', OUT:'Grid')].
l_solve(b1948b0a, IN, OUT) :-
    [f(replace, IN:'Grid', 'SIX':'Integer', 'TWO':'Integer', OUT:'Grid')].
l_solve(c59eb873, IN, OUT) :-
    [f(upscale, IN:'Element', 'TWO':'Integer', OUT:'Element')].
l_solve(c8f0f002, IN, OUT) :-
    [f(replace, IN:'Grid', 'SEVEN':'Integer', 'FIVE':'Integer', OUT:'Grid')].
l_solve(d10ecb37, IN, OUT) :-
    [f(crop, IN:'Grid', (0, 0):'Point', (2, 2):'Point', OUT:'Grid')].
l_solve(d511f180, IN, OUT) :-
    [f(switch, IN:'Grid', 'FIVE':'Integer', 'EIGHT':'Integer', OUT:'Grid')].
l_solve(ed36ccf7, IN, OUT) :-
    [f(rot270, IN:'Grid', OUT:'Grid')].
l_solve('4c4377d9', IN, OUT) :-
    [ f(hmirror, IN:'Piece', X1:'Piece'),
      f(vconcat, X1:'Grid', IN:'Grid', OUT:'Grid')
    ].
l_solve('6d0aefbc', IN, OUT) :-
    [ f(vmirror, IN:'Piece', X1:'Piece'),
      f(hconcat, IN:'Grid', X1:'Grid', OUT:'Grid')
    ].
l_solve('6fa7a44f', IN, OUT) :-
    [ f(hmirror, IN:'Piece', X1:'Piece'),
      f(vconcat, IN:'Grid', X1:'Grid', OUT:'Grid')
    ].
l_solve('5614dbcf', IN, OUT) :-
    [ f(replace, IN:'Grid', 'FIVE':'Integer', 'ZERO':'Integer', X1:'Grid'),
      f(downscale, X1:'Grid', 'THREE':'Integer', OUT:'Grid')
    ].
l_solve('5bd6f4ac', IN, OUT) :-
    [ f(tojvec, 'SIX':'Integer', X1:'Point'),
      f(crop, IN:'Grid', X1:'Point', (3, 3):'Point', OUT:'Grid')
    ].
l_solve('5582e5ca', IN, OUT) :-
    [ f(mostcolor, IN:'Element', X1:'Integer'),
      f(canvas, X1:'Integer', (3, 3):'Point', OUT:'Grid')
    ].
l_solve('8be77c9e', IN, OUT) :-
    [ f(hmirror, IN:'Piece', X1:'Piece'),
      f(vconcat, IN:'Grid', X1:'Grid', OUT:'Grid')
    ].
l_solve(c9e6f938, IN, OUT) :-
    [ f(vmirror, IN:'Piece', X1:'Piece'),
      f(hconcat, IN:'Grid', X1:'Grid', OUT:'Grid')
    ].
l_solve('2dee498d', IN, OUT) :-
    [ f(hsplit, IN:'Grid', 'THREE':'Integer', X1:'Tuple'),
      f(first, X1:'Container', OUT:'Any')
    ].
l_solve('1cf80156', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(first, X1:'Container', X2:'Any'),
      f(subgrid, X2:'Patch', IN:'Grid', OUT:'Grid')
    ].
l_solve(32597951, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'EIGHT':'Integer', X1:'Indices'),
      f(delta, X1:'Patch', X2:'Indices'),
      f(fill, IN:'Grid', 'THREE':'Integer', X2:'Patch', OUT:'Grid')
    ].
l_solve('25ff71a9', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(first, X1:'Container', X2:'Any'),
      f(move, IN:'Grid', X2:'Object', (1, 0):'Point', OUT:'Grid')
    ].
l_solve('0b148d64', IN, OUT) :-
    [ f(partition, IN:'Grid', X1:'Objects'),
      f(argmin, X1:'Container', size, X2:'Any'),
      f(subgrid, X2:'Patch', IN:'Grid', OUT:'Grid')
    ].
l_solve('1f85a75f', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(argmax, X1:'Container', size, X2:'Any'),
      f(subgrid, X2:'Patch', IN:'Grid', OUT:'Grid')
    ].
l_solve('23b5c85d', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(argmin, X1:'Container', size, X2:'Any'),
      f(subgrid, X2:'Patch', IN:'Grid', OUT:'Grid')
    ].
l_solve('9ecd008a', IN, OUT) :-
    [ f(vmirror, IN:'Piece', X1:'Piece'),
      f(ofcolor, IN:'Grid', 'ZERO':'Integer', X2:'Indices'),
      f(subgrid, X2:'Patch', X1:'Grid', OUT:'Grid')
    ].
l_solve(ac0a08a4, IN, OUT) :-
    [ f(colorcount, IN:'Element', 'ZERO':'Integer', X1:'Integer'),
      f(subtract, 'NINE':'Numerical', X1:'Numerical', X2:'Numerical'),
      f(upscale, IN:'Element', X2:'Integer', OUT:'Element')
    ].
l_solve(be94b721, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(argmax, X1:'Container', size, X2:'Any'),
      f(subgrid, X2:'Patch', IN:'Grid', OUT:'Grid')
    ].
l_solve(c909285e, IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(ofcolor, IN:'Grid', X1:'Integer', X2:'Indices'),
      f(subgrid, X2:'Patch', IN:'Grid', OUT:'Grid')
    ].
l_solve(f25ffba3, IN, OUT) :-
    [ f(bottomhalf, IN:'Grid', X1:'Grid'),
      f(hmirror, X1:'Piece', X2:'Piece'),
      f(vconcat, X2:'Grid', X1:'Grid', OUT:'Grid')
    ].
l_solve(c1d99e64, IN, OUT) :-
    [ f(frontiers, IN:'Grid', X1:'Objects'),
      f(merge, X1:'ContainerContainer', X2:'Container'),
      f(fill, IN:'Grid', 'TWO':'Integer', X2:'Patch', OUT:'Grid')
    ].
l_solve(b91ae062, IN, OUT) :-
    [ f(numcolors, IN:'Element', X1:'IntegerSet'),
      f(decrement, X1:'Numerical', X2:'Numerical'),
      f(upscale, IN:'Element', X2:'Integer', OUT:'Element')
    ].
l_solve('3aa6fb7a', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(mapply, corners, X1:'ContainerContainer', X2:'FrozenSet'),
      f(underfill, IN:'Grid', 'ONE':'Integer', X2:'Patch', OUT:'Grid')
    ].
l_solve('7b7f7511', IN, OUT) :-
    [ f(portrait, IN:'Piece', returns=X1),
      f(branch, condition=X1, tophalf:'Any', lefthalf:'Any', X2:'Any'),
      f(X2, IN, OUT)
    ].
l_solve('4258a5f9', IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'FIVE':'Integer', X1:'Indices'),
      f(mapply, neighbors, X1:'ContainerContainer', X2:'FrozenSet'),
      f(fill, IN:'Grid', 'ONE':'Integer', X2:'Patch', OUT:'Grid')
    ].
l_solve('2dc579da', IN, OUT) :-
    [ f(vsplit, IN:'Grid', 'TWO':'Integer', X1:'Tuple'),
      f(rbind, hsplit, 'TWO':'Any', X2:'Callable'),
      f(mapply, X2, X1:'ContainerContainer', X3:'FrozenSet'),
      f(argmax, X3:'Container', numcolors, OUT:'Any')
    ].
l_solve('28bf18c6', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(first, X1:'Container', X2:'Any'),
      f(subgrid, X2:'Patch', IN:'Grid', X3:'Grid'),
      f(hconcat, X3:'Grid', X3:'Grid', OUT:'Grid')
    ].
l_solve('3af2c5a8', IN, OUT) :-
    [ f(vmirror, IN:'Piece', X1:'Piece'),
      f(hconcat, IN:'Grid', X1:'Grid', X2:'Grid'), 
      f(hmirror, X2:'Piece', X3:'Piece'),      
      f(vconcat, X2:'Grid', X3:'Grid', OUT:'Grid')
    ].
l_solve('44f52bb0', IN, OUT) :-
    [ f(vmirror, IN:'Piece', X1:'Piece'),
      f(equality, X1:'Any', IN:'Any', returns=X2),
      f(branch, condition=X2, 'ONE':'Any', 'SEVEN':'Any', X3:'Any'),
      f(canvas, X3:'Integer', (1, 1):'Point', OUT:'Grid')
    ].
l_solve('62c24649', IN, OUT) :-
    [ f(vmirror, IN:'Piece', X1:'Piece'),
      f(hconcat, IN:'Grid', X1:'Grid', X2:'Grid'),
      f(hmirror, X2:'Piece', X3:'Piece'),
      f(vconcat, X2:'Grid', X3:'Grid', OUT:'Grid')
    ].
l_solve('67e8384a', IN, OUT) :-
    [ f(vmirror, IN:'Piece', X1:'Piece'),
      f(hconcat, IN:'Grid', X1:'Grid', X2:'Grid'),
      f(hmirror, X2:'Piece', X3:'Piece'),
      f(vconcat, X2:'Grid', X3:'Grid', OUT:'Grid')
    ].
l_solve('7468f01a', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(first, X1:'Container', X2:'Any'),
      f(subgrid, X2:'Patch', IN:'Grid', X3:'Grid'),
      f(vmirror, X3:'Piece', OUT:'Piece')
    ].
l_solve('662c240a', IN, OUT) :-
    [ f(vsplit, IN:'Grid', 'THREE':'Integer', X1:'Tuple'),
      f(fork, equality, dmirror, identity, X2:'Callable'),
      f(compose, flip, X2, X3:'Callable'),
      f(extract, X1:'Container', X3, OUT:'Any')
    ].
l_solve('42a50994', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(sizefilter, X1:'Container', 'ONE':'Integer', X2:'FrozenSet'),
      f(merge, X2:'ContainerContainer', X3:'Container'),
      f(cover, IN:'Grid', X3:'Patch', OUT:'Grid')
    ].
l_solve('56ff96f3', IN, OUT) :-
    [ f(fgpartition, IN:'Grid', X1:'Objects'),
      f(fork, recolor, color, backdrop, X2:'Callable'),
      f(mapply, X2, X1:'ContainerContainer', X3:'FrozenSet'),
      f(paint, IN:'Grid', X3:'Object', OUT:'Grid')
    ].
l_solve('50cb2852', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(compose, backdrop, inbox, X2:'Callable'),
      f(mapply, X2, X1:'ContainerContainer', X3:'FrozenSet'),
      f(fill, IN:'Grid', 'EIGHT':'Integer', X3:'Patch', OUT:'Grid')
    ].
l_solve('4347f46a', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(fork, difference, toindices, box, X2:'Callable'),
      f(mapply, X2, X1:'ContainerContainer', X3:'FrozenSet'),
      f(fill, IN:'Grid', 'ZERO':'Integer', X3:'Patch', OUT:'Grid')
    ].
l_solve('46f33fce', IN, OUT) :-
    [ f(rot180, IN:'Grid', X1:'Grid'),
      f(downscale, X1:'Grid', 'TWO':'Integer', X2:'Grid'),
      f(rot180, X2:'Grid', X3:'Grid'),
      f(upscale, X3:'Element', 'FOUR':'Integer', OUT:'Element')
    ].
l_solve(a740d043, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(merge, X1:'ContainerContainer', X2:'Container'),
      f(subgrid, X2:'Patch', IN:'Grid', X3:'Grid'),
      f(replace, X3:'Grid', 'ONE':'Integer', 'ZERO':'Integer', OUT:'Grid')
    ].
l_solve(a79310a0, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(first, X1:'Container', X2:'Any'),
      f(move, IN:'Grid', X2:'Object', (1, 0):'Point', X3:'Grid'),
      f(replace, X3:'Grid', 'EIGHT':'Integer', 'TWO':'Integer', OUT:'Grid')
    ].
l_solve(aabf363d, IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(replace, IN:'Grid', X1:'Integer', 'ZERO':'Integer', X2:'Grid'),
      f(leastcolor, X2:'Element', X3:'Integer'),
      f(replace,
        X2:'Grid',
        X3:'Integer',
        X1:'Integer',
        OUT:'Grid')
    ].
l_solve(ae4f1146, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(rbind, colorcount, 'ONE':'Any', X2:'Callable'),
      f(argmax, X1:'Container', X2, X3:'Any'),
      f(subgrid, X3:'Patch', IN:'Grid', OUT:'Grid')
    ].
l_solve(b27ca6d3, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(sizefilter, X1:'Container', 'TWO':'Integer', X2:'FrozenSet'),
      f(mapply, outbox, X2:'ContainerContainer', X3:'FrozenSet'),
      f(fill, IN:'Grid', 'THREE':'Integer', X3:'Patch', OUT:'Grid')
    ].
l_solve(ce22a75a, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(apply, outbox, X1:'Container', X2:'Container'),
      f(mapply, backdrop, X2:'ContainerContainer', X3:'FrozenSet'),
      f(fill, IN:'Grid', 'ONE':'Integer', X3:'Patch', OUT:'Grid')
    ].
l_solve(dc1df850, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'TWO':'Integer', X2:'Objects'),
      f(mapply, outbox, X2:'ContainerContainer', X3:'FrozenSet'),
      f(fill, IN:'Grid', 'ONE':'Integer', X3:'Patch', OUT:'Grid')
    ].
l_solve(f25fbde4, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(first, X1:'Container', X2:'Any'),
      f(subgrid, X2:'Patch', IN:'Grid', X3:'Grid'),
      f(upscale, X3:'Element', 'TWO':'Integer', OUT:'Element')
    ].
l_solve('44d8ac46', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(apply, delta, X1:'Container', X2:'Container'),
      f(mfilter, X2:'Container', square, X3:'FrozenSet'),
      f(fill, IN:'Grid', 'TWO':'Integer', X3:'Patch', OUT:'Grid')
    ].
l_solve('1e0a9b12', IN, OUT) :-
    [ f(rot270, IN:'Grid', X1:'Grid'),
      f(rbind, order, identity:'Any', X2:'Callable'),
      f(apply, X2, X1:'Container', X3:'Container'),
      f(rot90, X3:'Grid', OUT:'Grid')
    ].
l_solve('0d3d703e', IN, OUT) :-
    [ f(switch, IN:'Grid', 'THREE':'Integer', 'FOUR':'Integer', X1:'Grid'),
      f(switch, X1:'Grid', 'EIGHT':'Integer', 'NINE':'Integer', X2:'Grid'),
      f(switch, X2:'Grid', 'TWO':'Integer', 'SIX':'Integer', X3:'Grid'),
      f(switch, X3:'Grid', 'ONE':'Integer', 'FIVE':'Integer', OUT:'Grid')
    ].
l_solve('3618c87e', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(sizefilter, X1:'Container', 'ONE':'Integer', X2:'FrozenSet'),
      f(merge, X2:'ContainerContainer', X3:'Container'),
      f(move, IN:'Grid', X3:'Object', (2, 0):'Point', OUT:'Grid')
    ].
l_solve('1c786137', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(argmax, X1:'Container', height, X2:'Any'),
      f(subgrid, X2:'Patch', IN:'Grid', X3:'Grid'),
      f(trim, X3:'Grid', OUT:'Grid')
    ].
l_solve('8efcae92', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'ONE':'Integer', X2:'Objects'),
      f(compose, size, delta, X3:'Callable'),
      f(argmax, X2:'Container', X3, X4:'Any'),
      f(subgrid, X4:'Patch', IN:'Grid', OUT:'Grid')
    ].
l_solve('445eab21', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(fork, multiply, height, width, X2:'Callable'),
      f(argmax, X1:'Container', X2, X3:'Any'),
      f(color, X3:'Object', X4:'Integer'),
      f(canvas, X4:'Integer', (2, 2):'Point', OUT:'Grid')
    ].
l_solve('6f8cd79b', IN, OUT) :-
    [ f(asindices, IN:'Grid', X1:'Indices'),
      f(apply, initset, X1:'Container', X2:'Container'),
      f(rbind, bordering, IN:'Any', X3:'Callable'),
      f(mfilter, X2:'Container', X3, X4:'FrozenSet'),
      f(fill, IN:'Grid', 'EIGHT':'Integer', X4:'Patch', OUT:'Grid')
    ].
l_solve('2013d3e2', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(first, X1:'Container', X2:'Any'),
      f(subgrid, X2:'Patch', IN:'Grid', X3:'Grid'),
      f(lefthalf, X3:'Grid', X4:'Grid'),
      f(tophalf, X4:'Grid', OUT:'Grid')
    ].
l_solve('41e4d17e', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(fork, combine, vfrontier, hfrontier, X2:'Callable'),
      f(compose, X2, center, X3:'Callable'),
      f(mapply, X3, X1:'ContainerContainer', X4:'FrozenSet'),
      f(underfill, IN:'Grid', 'SIX':'Integer', X4:'Patch', OUT:'Grid')
    ].
l_solve('9565186b', IN, OUT) :-
    [ f(shape, IN:'Piece', X1:'Point'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X2:'Objects'),
      f(argmax, X2:'Container', size, X3:'Any'),
      f(canvas, 'FIVE':'Integer', X1:'Point', X4:'Grid'),
      f(paint, X4:'Grid', X3:'Object', OUT:'Grid')
    ].
l_solve(aedd82e4, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'TWO':'Integer', X2:'Objects'),
      f(sizefilter, X2:'Container', 'ONE':'Integer', X3:'FrozenSet'),
      f(merge, X3:'ContainerContainer', X4:'Container'),
      f(fill, IN:'Grid', 'ONE':'Integer', X4:'Patch', OUT:'Grid')
    ].
l_solve(bb43febb, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'FIVE':'Integer', X2:'Objects'),
      f(compose, backdrop, inbox, X3:'Callable'),
      f(mapply, X3, X2:'ContainerContainer', X4:'FrozenSet'),
      f(fill, IN:'Grid', 'TWO':'Integer', X4:'Patch', OUT:'Grid')
    ].
l_solve(e98196ab, IN, OUT) :-
    [ f(tophalf, IN:'Grid', X1:'Grid'),
      f(bottomhalf, IN:'Grid', X2:'Grid'),
      f(objects,
        X1:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X3:'Objects'),
      f(merge, X3:'ContainerContainer', X4:'Container'),
      f(paint, X2:'Grid', X4:'Object', OUT:'Grid')
    ].
l_solve(f76d97a5, IN, OUT) :-
    [ f(palette, IN:'Element', X1:'IntegerSet'),
      f(first, X1:'Container', X2:'Any'),
      f(last, X1:'Container', X3:'Any'),
      f(switch, IN:'Grid', X2:'Integer', X3:'Integer', X4:'Grid'),
      f(replace, X4:'Grid', 'FIVE':'Integer', 'ZERO':'Integer', OUT:'Grid')
    ].
l_solve(ce9e57f2, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(fork, connect, ulcorner, centerofmass, X2:'Callable'),
      f(mapply, X2, X1:'ContainerContainer', X3:'FrozenSet'),
      f(fill, IN:'Grid', 'EIGHT':'Integer', X3:'Patch', X4:'Grid'),
      f(switch, X4:'Grid', 'EIGHT':'Integer', 'TWO':'Integer', OUT:'Grid')
    ].
l_solve('22eb0ac0', IN, OUT) :-
    [ f(fgpartition, IN:'Grid', X1:'Objects'),
      f(fork, recolor, color, backdrop, X2:'Callable'),
      f(apply, X2, X1:'Container', X3:'Container'),
      f(mfilter, X3:'Container', hline, X4:'FrozenSet'),
      f(paint, IN:'Grid', X4:'Object', OUT:'Grid')
    ].
l_solve('9f236235', IN, OUT) :-
    [ f(compress, IN:'Grid', X1:'Grid'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X2:'Objects'),
      f(vmirror, X1:'Piece', X3:'Piece'),
      f(valmin, X2:'Container', width, X4:'Integer'),
      f(downscale, X3:'Grid', X4:'Integer', OUT:'Grid')
    ].
l_solve(a699fb00, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'ONE':'Integer', X1:'Indices'),
      f(shift, X1:'Patch', (0, 1):'Point', X2:'Patch'),
      f(shift, X1:'Patch', (0, -1):'Point', X3:'Patch'),
      f(intersection, X2:'FrozenSet', X3:'FrozenSet', X4:'FrozenSet'),
      f(fill, IN:'Grid', 'TWO':'Integer', X4:'Patch', OUT:'Grid')
    ].
l_solve('46442a0e', IN, OUT) :-
    [ f(rot90, IN:'Grid', X1:'Grid'),
      f(rot180, IN:'Grid', X2:'Grid'),
      f(rot270, IN:'Grid', X3:'Grid'),
      f(hconcat, IN:'Grid', X1:'Grid', X4:'Grid'),
      f(hconcat, X3:'Grid', X2:'Grid', X5:'Grid'),
      f(vconcat, X4:'Grid', X5:'Grid', OUT:'Grid')
    ].
l_solve('7fe24cdd', IN, OUT) :-
    [ f(rot90, IN:'Grid', X1:'Grid'),
      f(rot180, IN:'Grid', X2:'Grid'),
      f(rot270, IN:'Grid', X3:'Grid'),
      f(hconcat, IN:'Grid', X1:'Grid', X4:'Grid'),
      f(hconcat, X3:'Grid', X2:'Grid', X5:'Grid'),
      f(vconcat, X4:'Grid', X5:'Grid', OUT:'Grid')
    ].
l_solve('0ca9ddb6', IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'ONE':'Integer', X1:'Indices'),
      f(ofcolor, IN:'Grid', 'TWO':'Integer', X2:'Indices'),
      f(mapply, dneighbors, X1:'ContainerContainer', X3:'FrozenSet'),
      f(mapply, ineighbors, X2:'ContainerContainer', X4:'FrozenSet'),
      f(fill, IN:'Grid', 'SEVEN':'Integer', X3:'Patch', X5:'Grid'),
      f(fill, X5:'Grid', 'FOUR':'Integer', X4:'Patch', OUT:'Grid')
    ].
l_solve('543a7ed5', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'SIX':'Integer', X2:'Objects'),
      f(mapply, outbox, X2:'ContainerContainer', X3:'FrozenSet'),
      f(fill, IN:'Grid', 'THREE':'Integer', X3:'Patch', X4:'Grid'),
      f(mapply, delta, X2:'ContainerContainer', X5:'FrozenSet'),
      f(fill, X4:'Grid', 'FOUR':'Integer', X5:'Patch', OUT:'Grid')
    ].
l_solve('0520fde7', IN, OUT) :-
    [ f(vmirror, IN:'Piece', X1:'Piece'),
      f(lefthalf, X1:'Grid', X2:'Grid'),
      f(righthalf, X1:'Grid', X3:'Grid'),
      f(vmirror, X3:'Piece', X4:'Piece'),
      f(cellwise, X2:'Grid', X4:'Grid', 'ZERO':'Integer', X5:'Grid'),
      f(replace, X5:'Grid', 'ONE':'Integer', 'TWO':'Integer', OUT:'Grid')
    ].
l_solve(dae9d2b5, IN, OUT) :-
    [ f(lefthalf, IN:'Grid', X1:'Grid'),
      f(righthalf, IN:'Grid', X2:'Grid'),
      f(ofcolor, X1:'Grid', 'FOUR':'Integer', X3:'Indices'),
      f(ofcolor, X2:'Grid', 'THREE':'Integer', X4:'Indices'),
      f(combine, X3:'Container', X4:'Container', X5:'Container'),
      f(fill, X1:'Grid', 'SIX':'Integer', X5:'Patch', OUT:'Grid')
    ].
l_solve('8d5021e8', IN, OUT) :-
    [ f(vmirror, IN:'Piece', X1:'Piece'),
      f(hconcat, X1:'Grid', IN:'Grid', X2:'Grid'),
      f(hmirror, X2:'Piece', X3:'Piece'),
      f(vconcat, X2:'Grid', X3:'Grid', X4:'Grid'),
      f(vconcat, X4:'Grid', X2:'Grid', X5:'Grid'),
      f(hmirror, X5:'Piece', OUT:'Piece')
    ].
l_solve('928ad970', IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'FIVE':'Integer', X1:'Indices'),
      f(subgrid, X1:'Patch', IN:'Grid', X2:'Grid'),
      f(trim, X2:'Grid', X3:'Grid'),
      f(leastcolor, X3:'Element', X4:'Integer'),
      f(inbox, X1:'Patch', X5:'Indices'),
      f(fill, IN:'Grid', X4:'Integer', X5:'Patch', OUT:'Grid')
    ].
l_solve(b60334d2, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'FIVE':'Integer', X1:'Indices'),
      f(replace, IN:'Grid', 'FIVE':'Integer', 'ZERO':'Integer', X2:'Grid'),
      f(mapply, dneighbors, X1:'ContainerContainer', X3:'FrozenSet'),
      f(mapply, ineighbors, X1:'ContainerContainer', X4:'FrozenSet'),
      f(fill, X2:'Grid', 'ONE':'Integer', X3:'Patch', X5:'Grid'),
      f(fill, X5:'Grid', 'FIVE':'Integer', X4:'Patch', OUT:'Grid')
    ].
l_solve(b94a9452, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(first, X1:'Container', X2:'Any'),
      f(subgrid, X2:'Patch', IN:'Grid', X3:'Grid'),
      f(leastcolor, X3:'Element', X4:'Integer'),
      f(mostcolor, X3:'Element', X5:'Integer'),
      f(switch, X3:'Grid', X4:'Integer', X5:'Integer', OUT:'Grid')
    ].
l_solve(d037b0a7, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(rbind, shoot, (1, 0):'Any', X2:'Callable'),
      f(compose, X2, center, X3:'Callable'),
      f(fork, recolor, color, X3, X4:'Callable'),
      f(mapply, X4, X1:'ContainerContainer', X5:'FrozenSet'),
      f(paint, IN:'Grid', X5:'Object', OUT:'Grid')
    ].
l_solve(d0f5fe59, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(size, X1:'Container', X2:'Integer'),
      f(astuple, X2:'Integer', X2:'Integer', X3:'Point'),
      f(canvas, 'ZERO':'Integer', X3:'Point', X4:'Grid'),
      f(shoot, (0, 0):'Point', (1, 1):'Point', X5:'Indices'),
      f(fill, X4:'Grid', 'EIGHT':'Integer', X5:'Patch', OUT:'Grid')
    ].
l_solve(e3497940, IN, OUT) :-
    [ f(lefthalf, IN:'Grid', X1:'Grid'),
      f(righthalf, IN:'Grid', X2:'Grid'),
      f(vmirror, X2:'Piece', X3:'Piece'),
      f(objects,
        X3:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X4:'Objects'),
      f(merge, X4:'ContainerContainer', X5:'Container'),
      f(paint, X1:'Grid', X5:'Object', OUT:'Grid')
    ].
l_solve(e9afcf9a, IN, OUT) :-
    [ f(astuple, 'TWO':'Integer', 'ONE':'Integer', X1:'Point'),
      f(crop, IN:'Grid', (0, 0):'Point', X1:'Point', X2:'Grid'),
      f(hmirror, X2:'Piece', X3:'Piece'),
      f(hconcat, X2:'Grid', X3:'Grid', X4:'Grid'),
      f(hconcat, X4:'Grid', X4:'Grid', X5:'Grid'),
      f(hconcat, X5:'Grid', X4:'Grid', OUT:'Grid')
    ].
l_solve('48d8fb45', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(matcher, size, 'ONE':'Any', X2:'Callable'),
      f(extract, X1:'Container', X2, X3:'Any'),
      f(lbind, adjacent, X3:'Any', X4:'Callable'),
      f(extract, X1:'Container', X4, X5:'Any'),
      f(subgrid, X5:'Patch', IN:'Grid', OUT:'Grid')
    ].
l_solve(d406998b, IN, OUT) :-
    [ f(vmirror, IN:'Piece', X1:'Piece'),
      f(ofcolor, X1:'Grid', 'FIVE':'Integer', X2:'Indices'),
      f(compose, even, last, X3:'Callable'),
      f(sfilter, X2:'Container', X3, X4:'Container'),
      f(fill, X1:'Grid', 'THREE':'Integer', X4:'Patch', X5:'Grid'),
      f(vmirror, X5:'Piece', OUT:'Piece')
    ].
l_solve(5.117e+65, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(matcher, numcolors, 'TWO':'Any', X2:'Callable'),
      f(extract, X1:'Container', X2, X3:'Any'),
      f(subgrid, X3:'Patch', IN:'Grid', X4:'Grid'),
      f(mostcolor, X3:'Element', X5:'Integer'),
      f(replace, X4:'Grid', 'EIGHT':'Integer', X5:'Integer', OUT:'Grid')
    ].
l_solve('3906de3d', IN, OUT) :-
    [ f(rot270, IN:'Grid', X1:'Grid'),
      f(rbind, order, identity:'Any', X2:'Callable'),
      f(switch, X1:'Grid', 'ONE':'Integer', 'TWO':'Integer', X3:'Grid'),
      f(apply, X2, X3:'Container', X4:'Container'),
      f(switch, X4:'Grid', 'ONE':'Integer', 'TWO':'Integer', X5:'Grid'),
      f(cmirror, X5:'Piece', OUT:'Piece')
    ].
l_solve('00d62c1b', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'ZERO':'Integer', X2:'Objects'),
      f(rbind, bordering, IN:'Any', X3:'Callable'),
      f(compose, flip, X3, X4:'Callable'),
      f(mfilter, X2:'Container', X4, X5:'FrozenSet'),
      f(fill, IN:'Grid', 'FOUR':'Integer', X5:'Patch', OUT:'Grid')
    ].
l_solve('7b6016b9', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(rbind, bordering, IN:'Any', X2:'Callable'),
      f(compose, flip, X2, X3:'Callable'),
      f(mfilter, X1:'Container', X3, X4:'FrozenSet'),
      f(fill, IN:'Grid', 'TWO':'Integer', X4:'Patch', X5:'Grid'),
      f(replace, X5:'Grid', 'ZERO':'Integer', 'THREE':'Integer', OUT:'Grid')
    ].
l_solve('67385a82', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'THREE':'Integer', X2:'Objects'),
      f(sizefilter, X2:'Container', 'ONE':'Integer', X3:'FrozenSet'),
      f(difference, X2:'FrozenSet', X3:'FrozenSet', X4:'FrozenSet'),
      f(merge, X4:'ContainerContainer', X5:'Container'),
      f(fill, IN:'Grid', 'EIGHT':'Integer', X5:'Patch', OUT:'Grid')
    ].
l_solve(a5313dff, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'ZERO':'Integer', X2:'Objects'),
      f(rbind, bordering, IN:'Any', X3:'Callable'),
      f(compose, flip, X3, X4:'Callable'),
      f(mfilter, X2:'Container', X4, X5:'FrozenSet'),
      f(fill, IN:'Grid', 'ONE':'Integer', X5:'Patch', OUT:'Grid')
    ].
l_solve(ea32f347, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(replace, IN:'Grid', 'FIVE':'Integer', 'FOUR':'Integer', X2:'Grid'),
      f(argmin, X1:'Container', size, X3:'Any'),
      f(argmax, X1:'Container', size, X4:'Any'),
      f(fill, X2:'Grid', 'ONE':'Integer', X4:'Patch', X5:'Grid'),
      f(fill, X5:'Grid', 'TWO':'Integer', X3:'Patch', OUT:'Grid')
    ].
l_solve(d631b094, IN, OUT) :-
    [ f(palette, IN:'Element', X1:'IntegerSet'),
      f(other, X1:'Container', 'ZERO':'Any', X2:'Any'),
      f(ofcolor, IN:'Grid', X2:'Integer', X3:'Indices'),
      f(size, X3:'Container', X4:'Integer'),
      f(astuple, 'ONE':'Integer', X4:'Integer', X5:'Point'),
      f(canvas, X2:'Integer', X5:'Point', OUT:'Grid')
    ].
l_solve('10fcaaa3', IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(hconcat, IN:'Grid', IN:'Grid', X2:'Grid'),
      f(vconcat, X2:'Grid', X2:'Grid', X3:'Grid'),
      f(ofcolor, X3:'Grid', X1:'Integer', X4:'Indices'),
      f(mapply, ineighbors, X4:'ContainerContainer', X5:'FrozenSet'),
      f(underfill, X3:'Grid', 'EIGHT':'Integer', X5:'Patch', OUT:'Grid')
    ].
l_solve('007bbfb7', IN, OUT) :-
    [ f(hupscale, IN:'Grid', 'THREE':'Integer', X1:'Grid'),
      f(vupscale, X1:'Grid', 'THREE':'Integer', X2:'Grid'),
      f(hconcat, IN:'Grid', IN:'Grid', X3:'Grid'),
      f(hconcat, X3:'Grid', IN:'Grid', X4:'Grid'),
      f(vconcat, X4:'Grid', X4:'Grid', X5:'Grid'),
      f(vconcat, X5:'Grid', X4:'Grid', X6:'Grid'),
      f(cellwise, X2:'Grid', X6:'Grid', 'ZERO':'Integer', OUT:'Grid')
    ].
l_solve('496994bd', IN, OUT) :-
    [ f(width, IN:'Piece', X1:'Integer'),
      f(height, IN:'Piece', X2:'Integer'),
      f(halve, X2:'Numerical', X3:'Numerical'),
      f(astuple, X3:'Integer', X1:'Integer', X4:'Point'),
      f(crop, IN:'Grid', (0, 0):'Point', X4:'Point', X5:'Grid'),
      f(hmirror, X5:'Piece', X6:'Piece'),
      f(vconcat, X5:'Grid', X6:'Grid', OUT:'Grid')
    ].
l_solve('1f876c06', IN, OUT) :-
    [ f(fgpartition, IN:'Grid', X1:'Objects'),
      f(compose, last, first, X2:'Callable'),
      f(power, last, 'TWO':'Integer', X3:'Callable'),
      f(fork, connect, X2, X3, X4:'Callable'),
      f(fork, recolor, color, X4, X5:'Callable'),
      f(mapply, X5, X1:'ContainerContainer', X6:'FrozenSet'),
      f(paint, IN:'Grid', X6:'Object', OUT:'Grid')
    ].
l_solve('05f2a901', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'TWO':'Integer', X2:'Objects'),
      f(first, X2:'Container', X3:'Any'),
      f(colorfilter, X1:'Objects', 'EIGHT':'Integer', X4:'Objects'),
      f(first, X4:'Container', X5:'Any'),
      f(gravitate, X3:'Patch', X5:'Patch', X6:'Point'),
      f(move, IN:'Grid', X3:'Object', X6:'Point', OUT:'Grid')
    ].
l_solve('39a8645d', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(totuple, X1:'FrozenSet', X2:'Tuple'),
      f(apply, color, X2:'Container', X3:'Container'),
      f(mostcommon, X3:'Container', X4:'Any'),
      f(matcher, color, X4:'Any', X5:'Callable'),
      f(extract, X1:'Container', X5, X6:'Any'),
      f(subgrid, X6:'Patch', IN:'Grid', OUT:'Grid')
    ].
l_solve('1b2d62fb', IN, OUT) :-
    [ f(lefthalf, IN:'Grid', X1:'Grid'),
      f(righthalf, IN:'Grid', X2:'Grid'),
      f(ofcolor, X1:'Grid', 'ZERO':'Integer', X3:'Indices'),
      f(ofcolor, X2:'Grid', 'ZERO':'Integer', X4:'Indices'),
      f(intersection, X3:'FrozenSet', X4:'FrozenSet', X5:'FrozenSet'),
      f(replace, X1:'Grid', 'NINE':'Integer', 'ZERO':'Integer', X6:'Grid'),
      f(fill, X6:'Grid', 'EIGHT':'Integer', X5:'Patch', OUT:'Grid')
    ].
l_solve('90c28cc7', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(first, X1:'Container', X2:'Any'),
      f(subgrid, X2:'Patch', IN:'Grid', X3:'Grid'),
      f(dedupe, X3:'Tuple', X4:'Tuple'),
      f(rot90, X4:'Grid', X5:'Grid'),
      f(dedupe, X5:'Tuple', X6:'Tuple'),
      f(rot270, X6:'Grid', OUT:'Grid')
    ].
l_solve(b6afb2da, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(replace, IN:'Grid', 'FIVE':'Integer', 'TWO':'Integer', X2:'Grid'),
      f(colorfilter, X1:'Objects', 'FIVE':'Integer', X3:'Objects'),
      f(mapply, box, X3:'ContainerContainer', X4:'FrozenSet'),
      f(fill, X2:'Grid', 'FOUR':'Integer', X4:'Patch', X5:'Grid'),
      f(mapply, corners, X3:'ContainerContainer', X6:'FrozenSet'),
      f(fill, X5:'Grid', 'ONE':'Integer', X6:'Patch', OUT:'Grid')
    ].
l_solve(b9b7f026, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(argmin, X1:'Container', size, X2:'Any'),
      f(rbind, adjacent, X2:'Any', X3:'Callable'),
      f(remove, X2:'Any', X1:'Container', X4:'Container'),
      f(extract, X4:'Container', X3, X5:'Any'),
      f(color, X5:'Object', X6:'Integer'),
      f(canvas, X6:'Integer', (1, 1):'Point', OUT:'Grid')
    ].
l_solve(ba97ae07, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(totuple, X1:'FrozenSet', X2:'Tuple'),
      f(apply, color, X2:'Container', X3:'Container'),
      f(mostcommon, X3:'Container', X4:'Any'),
      f(ofcolor, IN:'Grid', X4:'Integer', X5:'Indices'),
      f(backdrop, X5:'Patch', X6:'Indices'),
      f(fill, IN:'Grid', X4:'Integer', X6:'Patch', OUT:'Grid')
    ].
l_solve(c9f8e694, IN, OUT) :-
    [ f(height, IN:'Piece', X1:'Integer'),
      f(width, IN:'Piece', X2:'Integer'),
      f(ofcolor, IN:'Grid', 'ZERO':'Integer', X3:'Indices'),
      f(astuple, X1:'Integer', 'ONE':'Integer', X4:'Point'),
      f(crop, IN:'Grid', (0, 0):'Point', X4:'Point', X5:'Grid'),
      f(hupscale, X5:'Grid', X2:'Integer', X6:'Grid'),
      f(fill, X6:'Grid', 'ZERO':'Integer', X3:'Patch', OUT:'Grid')
    ].
l_solve(d23f8c26, IN, OUT) :-
    [ f(asindices, IN:'Grid', X1:'Indices'),
      f(width, IN:'Piece', X2:'Integer'),
      f(halve, X2:'Numerical', X3:'Numerical'),
      f(matcher, last, X3:'Any', X4:'Callable'),
      f(compose, flip, X4, X5:'Callable'),
      f(sfilter, X1:'Container', X5, X6:'Container'),
      f(fill, IN:'Grid', 'ZERO':'Integer', X6:'Patch', OUT:'Grid')
    ].
l_solve(d5d6de2d, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(sfilter, X1:'Container', square, X2:'Container'),
      f(difference, X1:'FrozenSet', X2:'FrozenSet', X3:'FrozenSet'),
      f(compose, backdrop, inbox, X4:'Callable'),
      f(mapply, X4, X3:'ContainerContainer', X5:'FrozenSet'),
      f(replace, IN:'Grid', 'TWO':'Integer', 'ZERO':'Integer', X6:'Grid'),
      f(fill, X6:'Grid', 'THREE':'Integer', X5:'Patch', OUT:'Grid')
    ].
l_solve(dbc1a6ce, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'ONE':'Integer', X1:'Indices'),
      f(product, X1:'Container', X1:'Container', X2:'FrozenSet'),
      f(fork, connect, first, last, X3:'Callable'),
      f(apply, X3, X2:'Container', X4:'Container'),
      f(fork, either, vline, hline, X5:'Callable'),
      f(mfilter, X4:'Container', X5, X6:'FrozenSet'),
      f(underfill, IN:'Grid', 'EIGHT':'Integer', X6:'Patch', OUT:'Grid')
    ].
l_solve(ded97339, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'EIGHT':'Integer', X1:'Indices'),
      f(product, X1:'Container', X1:'Container', X2:'FrozenSet'),
      f(fork, connect, first, last, X3:'Callable'),
      f(apply, X3, X2:'Container', X4:'Container'),
      f(fork, either, vline, hline, X5:'Callable'),
      f(mfilter, X4:'Container', X5, X6:'FrozenSet'),
      f(underfill, IN:'Grid', 'EIGHT':'Integer', X6:'Patch', OUT:'Grid')
    ].
l_solve(ea786f4a, IN, OUT) :-
    [ f(width, IN:'Piece', X1:'Integer'),
      f(shoot, (0, 0):'Point', (1, 1):'Point', X2:'Indices'),
      f(decrement, X1:'Numerical', X3:'Numerical'),
      f(tojvec, X3:'Integer', X4:'Point'),
      f(shoot, X4:'Point', (1, -1):'Point', X5:'Indices'),
      f(combine, X2:'Container', X5:'Container', X6:'Container'),
      f(fill, IN:'Grid', 'ZERO':'Integer', X6:'Patch', OUT:'Grid')
    ].
l_solve('08ed6ac7', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(totuple, X1:'FrozenSet', X2:'Tuple'),
      f(order, X1:'Container', height, X3:'Tuple'),
      f(size, X2:'Container', X4:'Integer'),
      f(interval, X4:'Integer', 'ZERO':'Integer', 'NEG_ONE':'Integer', X5:'Tuple'),
      f(mpapply, recolor, X5:'Tuple', X3:'Tuple', X6:'Tuple'),
      f(paint, IN:'Grid', X6:'Object', OUT:'Grid')
    ].
l_solve(40853293, IN, OUT) :-
    [ f(partition, IN:'Grid', X1:'Objects'),
      f(fork, recolor, color, backdrop, X2:'Callable'),
      f(apply, X2, X1:'Container', X3:'Container'),
      f(mfilter, X3:'Container', hline, X4:'FrozenSet'),
      f(mfilter, X3:'Container', vline, X5:'FrozenSet'),
      f(paint, IN:'Grid', X4:'Object', X6:'Grid'),
      f(paint, X6:'Grid', X5:'Object', OUT:'Grid')
    ].
l_solve('5521c0d9', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(merge, X1:'ContainerContainer', X2:'Container'),
      f(cover, IN:'Grid', X2:'Patch', X3:'Grid'),
      f(chain, toivec, invert, height, X4:'Callable'),
      f(fork, shift, identity, X4, X5:'Callable'),
      f(mapply, X5, X1:'ContainerContainer', X6:'FrozenSet'),
      f(paint, X3:'Grid', X6:'Object', OUT:'Grid')
    ].
l_solve(f8ff0b80, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(order, X1:'Container', size, X2:'Tuple'),
      f(apply, color, X2:'Container', X3:'Container'),
      f(rbind, canvas, (1, 1):'Any', X4:'Callable'),
      f(apply, X4, X3:'Container', X5:'Container'),
      f(merge, X5:'ContainerContainer', X6:'Container'),
      f(hmirror, X6:'Piece', OUT:'Piece')
    ].
l_solve('85c4e7cd', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(compose, invert, size, X2:'Callable'),
      f(order, X1:'Container', size, X3:'Tuple'),
      f(order, X1:'Container', X2, X4:'Tuple'),
      f(apply, color, X4:'Container', X5:'Container'),
      f(mpapply, recolor, X5:'Tuple', X3:'Tuple', X6:'Tuple'),
      f(paint, IN:'Grid', X6:'Object', OUT:'Grid')
    ].
l_solve(d2abd087, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(matcher, size, 'SIX':'Any', X2:'Callable'),
      f(compose, flip, X2, X3:'Callable'),
      f(mfilter, X1:'Container', X2, X4:'FrozenSet'),
      f(mfilter, X1:'Container', X3, X5:'FrozenSet'),
      f(fill, IN:'Grid', 'TWO':'Integer', X4:'Patch', X6:'Grid'),
      f(fill, X6:'Grid', 'ONE':'Integer', X5:'Patch', OUT:'Grid')
    ].
l_solve('017c7c7b', IN, OUT) :-
    [ f(tophalf, IN:'Grid', X1:'Grid'),
      f(bottomhalf, IN:'Grid', X2:'Grid'),
      f(equality, X1:'Any', X2:'Any', returns=X3),
      f(crop, IN:'Grid', (2, 0):'Point', (3, 3):'Point', X4:'Grid'),
      f(branch, condition=X3, X2:'Any', X4:'Any', X5:'Any'),
      f(vconcat, IN:'Grid', X5:'Grid', X6:'Grid'),
      f(replace, X6:'Grid', 'ONE':'Integer', 'TWO':'Integer', OUT:'Grid')
    ].
l_solve('363442ee', IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'ONE':'Integer', X1:'Indices'),
      f(crop, IN:'Grid', (0, 0):'Point', (3, 3):'Point', X2:'Grid'),
      f(asobject, X2:'Grid', X3:'Object'),
      f(lbind, shift, X3:'Any', X4:'Callable'),
      f(compose, X4, decrement, X5:'Callable'),
      f(mapply, X5, X1:'ContainerContainer', X6:'FrozenSet'),
      f(paint, IN:'Grid', X6:'Object', OUT:'Grid')
    ].
l_solve('5168d44c', IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'THREE':'Integer', X1:'Indices'),
      f(height, X1:'Piece', X2:'Integer'),
      f(equality, X2:'Any', 'ONE':'Any', returns=X3),
      f(branch, condition=X3, (0, 2):'Any', (2, 0):'Any', X4:'Any'),
      f(ofcolor, IN:'Grid', 'TWO':'Integer', X5:'Indices'),
      f(recolor, 'TWO':'Integer', X5:'Patch', X6:'Object'),
      f(move, IN:'Grid', X6:'Object', X4:'Point', OUT:'Grid')
    ].
l_solve(e9614598, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'ONE':'Integer', X1:'Indices'),
      f(fork, add, first, last, X2:'Callable'),
      f(X2, X1, X3),
      f(halve, X3:'Numerical', X4:'Numerical'),
      f(dneighbors, X4:'Point', X5:'Indices'),
      f(insert, X4:'Any', X5:'FrozenSet', X6:'FrozenSet'),
      f(fill, IN:'Grid', 'THREE':'Integer', X6:'Patch', OUT:'Grid')
    ].
l_solve(d9fac9be, IN, OUT) :-
    [ f(palette, IN:'Element', X1:'IntegerSet'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X2:'Objects'),
      f(argmax, X2:'Container', size, X3:'Any'),
      f(color, X3:'Object', X4:'Integer'),
      f(remove, 'ZERO':'Any', X1:'Container', X5:'Container'),
      f(other, X5:'Container', X4:'Any', X6:'Any'),
      f(canvas, X6:'Integer', (1, 1):'Point', OUT:'Grid')
    ].
l_solve(e50d258f, IN, OUT) :-
    [ f(width, IN:'Piece', X1:'Integer'),
      f(astuple, 'NINE':'Integer', X1:'Integer', X2:'Point'),
      f(canvas, 'ZERO':'Integer', X2:'Point', X3:'Grid'),
      f(vconcat, IN:'Grid', X3:'Grid', X4:'Grid'),
      f(objects,
        X4:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X5:'Objects'),
      f(rbind, colorcount, 'TWO':'Any', X6:'Callable'),
      f(argmax, X5:'Container', X6, X7:'Any'),
      f(subgrid, X7:'Patch', IN:'Grid', OUT:'Grid')
    ].
l_solve('810b9b61', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(apply, toindices, X1:'Container', X2:'Container'),
      f(fork, either, vline, hline, X3:'Callable'),
      f(sfilter, X2:'Container', X3, X4:'Container'),
      f(difference, X2:'FrozenSet', X4:'FrozenSet', X5:'FrozenSet'),
      f(fork, equality, identity, box, X6:'Callable'),
      f(mfilter, X5:'Container', X6, X7:'FrozenSet'),
      f(fill, IN:'Grid', 'THREE':'Integer', X7:'Patch', OUT:'Grid')
    ].
l_solve('54d82841', IN, OUT) :-
    [ f(height, IN:'Piece', X1:'Integer'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X2:'Objects'),
      f(compose, last, center, X3:'Callable'),
      f(apply, X3, X2:'Container', X4:'Container'),
      f(decrement, X1:'Numerical', X5:'Numerical'),
      f(lbind, astuple, X5:'Any', X6:'Callable'),
      f(apply, X6, X4:'Container', X7:'Container'),
      f(fill, IN:'Grid', 'FOUR':'Integer', X7:'Patch', OUT:'Grid')
    ].
l_solve('60b61512', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(rbind, subgrid, IN:'Any', X2:'Callable'),
      f(compose, asindices, X2, X3:'Callable'),
      f(fork, shift, X3, ulcorner, X4:'Callable'),
      f(mapply, X4, X1:'ContainerContainer', X5:'FrozenSet'),
      f(fill, IN:'Grid', 'SEVEN':'Integer', X5:'Patch', X6:'Grid'),
      f(merge, X1:'ContainerContainer', X7:'Container'),
      f(paint, X6:'Grid', X7:'Object', OUT:'Grid')
    ].
l_solve('25d8a9c8', IN, OUT) :-
    [ f(asindices, IN:'Grid', X1:'Indices'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X2:'Objects'),
      f(sizefilter, X2:'Container', 'THREE':'Integer', X3:'FrozenSet'),
      f(mfilter, X3:'Container', hline, X4:'FrozenSet'),
      f(toindices, X4:'Patch', X5:'Indices'),
      f(difference, X1:'FrozenSet', X5:'FrozenSet', X6:'FrozenSet'),
      f(fill, IN:'Grid', 'FIVE':'Integer', X5:'Patch', X7:'Grid'),
      f(fill, X7:'Grid', 'ZERO':'Integer', X6:'Patch', OUT:'Grid')
    ].
l_solve('239be575', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(lbind, contained, 'TWO':'Any', X2:'Callable'),
      f(compose, X2, palette, X3:'Callable'),
      f(sfilter, X1:'Container', X3, X4:'Container'),
      f(size, X4:'Container', X5:'Integer'),
      f(greater, X5:'Integer', 'ONE':'Integer', returns=X6),
      f(branch, condition=X6, 'ZERO':'Any', 'EIGHT':'Any', X7:'Any'),
      f(canvas, X7:'Integer', (1, 1):'Point', OUT:'Grid')
    ].
l_solve('67a423a3', IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X2:'Objects'),
      f(colorfilter, X2:'Objects', X1:'Integer', X3:'Objects'),
      f(merge, X3:'ContainerContainer', X4:'Container'),
      f(delta, X4:'Patch', X5:'Indices'),
      f(first, X5:'Container', X6:'Any'),
      f(neighbors, X6:'Point', X7:'Indices'),
      f(fill, IN:'Grid', 'FOUR':'Integer', X7:'Patch', OUT:'Grid')
    ].
l_solve('5c0a986e', IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'TWO':'Integer', X1:'Indices'),
      f(ofcolor, IN:'Grid', 'ONE':'Integer', X2:'Indices'),
      f(lrcorner, X1:'Patch', X3:'Point'),
      f(ulcorner, X2:'Patch', X4:'Point'),
      f(shoot, X3:'Point', (1, 1):'Point', X5:'Indices'),
      f(shoot, X4:'Point', (-1, -1):'Point', X6:'Indices'),
      f(fill, IN:'Grid', 'TWO':'Integer', X5:'Patch', X7:'Grid'),
      f(fill, X7:'Grid', 'ONE':'Integer', X6:'Patch', OUT:'Grid')
    ].
l_solve('6430c8c4', IN, OUT) :-
    [ f(tophalf, IN:'Grid', X1:'Grid'),
      f(bottomhalf, IN:'Grid', X2:'Grid'),
      f(astuple, 'FOUR':'Integer', 'FOUR':'Integer', X3:'Point'),
      f(ofcolor, X1:'Grid', 'ZERO':'Integer', X4:'Indices'),
      f(ofcolor, X2:'Grid', 'ZERO':'Integer', X5:'Indices'),
      f(intersection, X4:'FrozenSet', X5:'FrozenSet', X6:'FrozenSet'),
      f(canvas, 'ZERO':'Integer', X3:'Point', X7:'Grid'),
      f(fill, X7:'Grid', 'THREE':'Integer', X6:'Patch', OUT:'Grid')
    ].
l_solve('94f9d214', IN, OUT) :-
    [ f(tophalf, IN:'Grid', X1:'Grid'),
      f(bottomhalf, IN:'Grid', X2:'Grid'),
      f(ofcolor, X1:'Grid', 'ZERO':'Integer', X3:'Indices'),
      f(ofcolor, X2:'Grid', 'ZERO':'Integer', X4:'Indices'),
      f(astuple, 'FOUR':'Integer', 'FOUR':'Integer', X5:'Point'),
      f(canvas, 'ZERO':'Integer', X5:'Point', X6:'Grid'),
      f(intersection, X3:'FrozenSet', X4:'FrozenSet', X7:'FrozenSet'),
      f(fill, X6:'Grid', 'TWO':'Integer', X7:'Patch', OUT:'Grid')
    ].
l_solve(a1570a43, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'TWO':'Integer', X1:'Indices'),
      f(ofcolor, IN:'Grid', 'THREE':'Integer', X2:'Indices'),
      f(recolor, 'TWO':'Integer', X1:'Patch', X3:'Object'),
      f(ulcorner, X2:'Patch', X4:'Point'),
      f(ulcorner, X1:'Patch', X5:'Point'),
      f(subtract, X4:'Numerical', X5:'Numerical', X6:'Numerical'),
      f(increment, X6:'Numerical', X7:'Numerical'),
      f(move, IN:'Grid', X3:'Object', X7:'Point', OUT:'Grid')
    ].
l_solve(ce4f8723, IN, OUT) :-
    [ f(tophalf, IN:'Grid', X1:'Grid'),
      f(bottomhalf, IN:'Grid', X2:'Grid'),
      f(ofcolor, X1:'Grid', 'ZERO':'Integer', X3:'Indices'),
      f(ofcolor, X2:'Grid', 'ZERO':'Integer', X4:'Indices'),
      f(intersection, X3:'FrozenSet', X4:'FrozenSet', X5:'FrozenSet'),
      f(astuple, 'FOUR':'Integer', 'FOUR':'Integer', X6:'Point'),
      f(canvas, 'THREE':'Integer', X6:'Point', X7:'Grid'),
      f(fill, X7:'Grid', 'ZERO':'Integer', X5:'Patch', OUT:'Grid')
    ].
l_solve(d13f3404, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(rbind, shoot, (1, 1):'Any', X2:'Callable'),
      f(compose, X2, center, X3:'Callable'),
      f(fork, recolor, color, X3, X4:'Callable'),
      f(mapply, X4, X1:'ContainerContainer', X5:'FrozenSet'),
      f(astuple, 'SIX':'Integer', 'SIX':'Integer', X6:'Point'),
      f(canvas, 'ZERO':'Integer', X6:'Point', X7:'Grid'),
      f(paint, X7:'Grid', X5:'Object', OUT:'Grid')
    ].
l_solve(dc433765, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'THREE':'Integer', X1:'Indices'),
      f(ofcolor, IN:'Grid', 'FOUR':'Integer', X2:'Indices'),
      f(first, X1:'Container', X3:'Any'),
      f(first, X2:'Container', X4:'Any'),
      f(subtract, X4:'Numerical', X3:'Numerical', X5:'Numerical'),
      f(sign, X5:'Numerical', X6:'Numerical'),
      f(recolor, 'THREE':'Integer', X1:'Patch', X7:'Object'),
      f(move, IN:'Grid', X7:'Object', X6:'Point', OUT:'Grid')
    ].
l_solve(f2829549, IN, OUT) :-
    [ f(lefthalf, IN:'Grid', X1:'Grid'),
      f(righthalf, IN:'Grid', X2:'Grid'),
      f(ofcolor, X1:'Grid', 'ZERO':'Integer', X3:'Indices'),
      f(ofcolor, X2:'Grid', 'ZERO':'Integer', X4:'Indices'),
      f(intersection, X3:'FrozenSet', X4:'FrozenSet', X5:'FrozenSet'),
      f(shape, X1:'Piece', X6:'Point'),
      f(canvas, 'ZERO':'Integer', X6:'Point', X7:'Grid'),
      f(fill, X7:'Grid', 'THREE':'Integer', X5:'Patch', OUT:'Grid')
    ].
l_solve(fafffa47, IN, OUT) :-
    [ f(tophalf, IN:'Grid', X1:'Grid'),
      f(bottomhalf, IN:'Grid', X2:'Grid'),
      f(shape, X2:'Piece', X3:'Point'),
      f(ofcolor, X1:'Grid', 'ZERO':'Integer', X4:'Indices'),
      f(ofcolor, X2:'Grid', 'ZERO':'Integer', X5:'Indices'),
      f(intersection, X4:'FrozenSet', X5:'FrozenSet', X6:'FrozenSet'),
      f(canvas, 'ZERO':'Integer', X3:'Point', X7:'Grid'),
      f(fill, X7:'Grid', 'TWO':'Integer', X6:'Patch', OUT:'Grid')
    ].
l_solve(fcb5c309, IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X2:'Objects'),
      f(colorfilter, X2:'Objects', X1:'Integer', X3:'Objects'),
      f(difference, X2:'FrozenSet', X3:'FrozenSet', X4:'FrozenSet'),
      f(argmax, X4:'Container', size, X5:'Any'),
      f(color, X5:'Object', X6:'Integer'),
      f(subgrid, X5:'Patch', IN:'Grid', X7:'Grid'),
      f(replace,
        X7:'Grid',
        X6:'Integer',
        X1:'Integer',
        OUT:'Grid')
    ].
l_solve(ff805c23, IN, OUT) :-
    [ f(hmirror, IN:'Piece', X1:'Piece'),
      f(vmirror, IN:'Piece', X2:'Piece'),
      f(ofcolor, IN:'Grid', 'ONE':'Integer', X3:'Indices'),
      f(subgrid, X3:'Patch', X1:'Grid', X4:'Grid'),
      f(subgrid, X3:'Patch', X2:'Grid', X5:'Grid'),
      f(palette, X4:'Element', X6:'IntegerSet'),
      f(contained, 'ONE':'Any', X6:'Container', returns=X7),
      f(branch, condition=X7, X5:'Any', X4:'Any', OUT:'Any')
    ].
l_solve(e76a88a6, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(argmax, X1:'Container', numcolors, X2:'Any'),
      f(normalize, X2:'Patch', X3:'Patch'),
      f(remove, X2:'Any', X1:'Container', X4:'Container'),
      f(apply, ulcorner, X4:'Container', X5:'Container'),
      f(lbind, shift, X3:'Any', X6:'Callable'),
      f(mapply, X6, X5:'ContainerContainer', X7:'FrozenSet'),
      f(paint, IN:'Grid', X7:'Object', OUT:'Grid')
    ].
l_solve('7c008303', IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'THREE':'Integer', X1:'Indices'),
      f(subgrid, X1:'Patch', IN:'Grid', X2:'Grid'),
      f(ofcolor, X2:'Grid', 'ZERO':'Integer', X3:'Indices'),
      f(replace, IN:'Grid', 'THREE':'Integer', 'ZERO':'Integer', X4:'Grid'),
      f(replace, X4:'Grid', 'EIGHT':'Integer', 'ZERO':'Integer', X5:'Grid'),
      f(compress, X5:'Grid', X6:'Grid'),
      f(upscale, X6:'Element', 'THREE':'Integer', X7:'Element'),
      f(fill, X7:'Grid', 'ZERO':'Integer', X3:'Patch', OUT:'Grid')
    ].
l_solve('7f4411dc', IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(ofcolor, IN:'Grid', X1:'Integer', X2:'Indices'),
      f(rbind, difference, X2:'Any', X3:'Callable'),
      f(rbind, greater, 'TWO':'Any', X4:'Callable'),
      f(chain, X4, size, X3, X5:'Callable'),
      f(compose, X5, dneighbors, X6:'Callable'),
      f(sfilter, X2:'Container', X6, X7:'Container'),
      f(fill, IN:'Grid', 'ZERO':'Integer', X7:'Patch', OUT:'Grid')
    ].
l_solve(b230c067, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(totuple, X1:'FrozenSet', X2:'Tuple'),
      f(apply, normalize, X2:'Container', X3:'Container'),
      f(leastcommon, X3:'Container', X4:'Any'),
      f(matcher, normalize, X4:'Any', X5:'Callable'),
      f(extract, X1:'Container', X5, X6:'Any'),
      f(replace, IN:'Grid', 'EIGHT':'Integer', 'ONE':'Integer', X7:'Grid'),
      f(fill, X7:'Grid', 'TWO':'Integer', X6:'Patch', OUT:'Grid')
    ].
l_solve(e8593010, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(sizefilter, X1:'Container', 'ONE':'Integer', X2:'FrozenSet'),
      f(sizefilter, X1:'Container', 'TWO':'Integer', X3:'FrozenSet'),
      f(merge, X2:'ContainerContainer', X4:'Container'),
      f(fill, IN:'Grid', 'THREE':'Integer', X4:'Patch', X5:'Grid'),
      f(merge, X3:'ContainerContainer', X6:'Container'),
      f(fill, X5:'Grid', 'TWO':'Integer', X6:'Patch', X7:'Grid'),
      f(replace, X7:'Grid', 'ZERO':'Integer', 'ONE':'Integer', OUT:'Grid')
    ].
l_solve('6d75e8bb', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(first, X1:'Container', X2:'Any'),
      f(ulcorner, X2:'Patch', X3:'Point'),
      f(subgrid, X2:'Patch', IN:'Grid', X4:'Grid'),
      f(replace, X4:'Grid', 'ZERO':'Integer', 'TWO':'Integer', X5:'Grid'),
      f(asobject, X5:'Grid', X6:'Object'),
      f(shift, X6:'Patch', X3:'Point', X7:'Patch'),
      f(paint, IN:'Grid', X7:'Object', OUT:'Grid')
    ].
l_solve('3f7978a0', IN, OUT) :-
    [ f(fgpartition, IN:'Grid', X1:'Objects'),
      f(matcher, color, 'FIVE':'Any', X2:'Callable'),
      f(extract, X1:'Container', X2, X3:'Any'),
      f(ulcorner, X3:'Patch', X4:'Point'),
      f(subtract, X4:'Numerical', (1, 0):'Numerical', X5:'Numerical'),
      f(shape, X3:'Piece', X6:'Point'),
      f(add, X6:'Numerical', (2, 0):'Numerical', X7:'Numerical'),
      f(crop, IN:'Grid', X5:'Point', X7:'Point', OUT:'Grid')
    ].
l_solve('1190e5a7', IN, OUT) :-
    [ f(mostcolor, IN:'Element', X1:'Integer'),
      f(frontiers, IN:'Grid', X2:'Objects'),
      f(sfilter, X2:'Container', vline, X3:'Container'),
      f(difference, X2:'FrozenSet', X3:'FrozenSet', X4:'FrozenSet'),
      f(astuple, X4:'Integer', X3:'Integer', X5:'Point'),
      f(apply, size, X5:'Container', X6:'Container'),
      f(increment, X6:'Numerical', X7:'Numerical'),
      f(canvas, X1:'Integer', X7:'Point', OUT:'Grid')
    ].
l_solve('6e02f1e3', IN, OUT) :-
    [ f(numcolors, IN:'Element', X1:'IntegerSet'),
      f(canvas, 'ZERO':'Integer', (3, 3):'Point', X2:'Grid'),
      f(equality, X1:'Any', 'THREE':'Any', returns=X3),
      f(equality, X1:'Any', 'TWO':'Any', returns=X4),
      f(branch, condition=X3, (2, 0):'Any', (0, 0):'Any', X5:'Any'),
      f(branch, condition=X4, (2, 2):'Any', (0, 2):'Any', X6:'Any'),
      f(connect, X5:'Point', X6:'Point', X7:'Indices'),
      f(fill, X2:'Grid', 'FIVE':'Integer', X7:'Patch', OUT:'Grid')
    ].
l_solve(a61f2674, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(argmax, X1:'Container', size, X2:'Any'),
      f(argmin, X1:'Container', size, X3:'Any'),
      f(replace, IN:'Grid', 'FIVE':'Integer', 'ZERO':'Integer', X4:'Grid'),
      f(recolor, 'ONE':'Integer', X2:'Patch', X5:'Object'),
      f(recolor, 'TWO':'Integer', X3:'Patch', X6:'Object'),
      f(combine, X5:'Container', X6:'Container', X7:'Container'),
      f(paint, X4:'Grid', X7:'Object', OUT:'Grid')
    ].
l_solve(fcc82909, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(rbind, add, (1, 0):'Any', X2:'Callable'),
      f(compose, X2, llcorner, X3:'Callable'),
      f(compose, toivec, numcolors, X4:'Callable'),
      f(fork, add, lrcorner, X4, X5:'Callable'),
      f(fork, astuple, X3, X5, X6:'Callable'),
      f(compose, box, X6, X7:'Callable'),
      f(mapply, X7, X1:'ContainerContainer', X8:'FrozenSet'),
      f(fill, IN:'Grid', 'THREE':'Integer', X8:'Patch', OUT:'Grid')
    ].
l_solve('72ca375d', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(totuple, X1:'FrozenSet', X2:'Tuple'),
      f(rbind, subgrid, IN:'Any', X3:'Callable'),
      f(apply, X3, X2:'Container', X4:'Container'),
      f(apply, vmirror, X4:'Container', X5:'Container'),
      f(papply, equality, X4:'Tuple', X5:'Tuple', X6:'Tuple'),
      f(pair, X4:'Tuple', X6:'Tuple', X7:'TupleTuple'),
      f(extract, X7:'Container', last, X8:'Any'),
      f(first, X8:'Container', OUT:'Any')
    ].
l_solve('253bf280', IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'EIGHT':'Integer', X1:'Indices'),
      f(prapply, connect, X1, X1, X2),
      f(rbind, greater, 'ONE':'Any', X3:'Callable'),
      f(compose, X3, size, X4:'Callable'),
      f(sfilter, X2:'Container', X4, X5:'Container'),
      f(fork, either, vline, hline, X6:'Callable'),
      f(mfilter, X5:'Container', X6, X7:'FrozenSet'),
      f(fill, IN:'Grid', 'THREE':'Integer', X7:'Patch', X8:'Grid'),
      f(fill, X8:'Grid', 'EIGHT':'Integer', X1:'Patch', OUT:'Grid')
    ].
l_solve('694f12f3', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'FOUR':'Integer', X2:'Objects'),
      f(compose, backdrop, inbox, X3:'Callable'),
      f(argmin, X2:'Container', size, X4:'Any'),
      f(argmax, X2:'Container', size, X5:'Any'),
      f(X3, X4, X6),
      f(X3, X5, X7),
      f(fill, IN:'Grid', 'ONE':'Integer', X6:'Patch', X8:'Grid'),
      f(fill, X8:'Grid', 'TWO':'Integer', X7:'Patch', OUT:'Grid')
    ].
l_solve('1f642eb9', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(sizefilter, X1:'Container', 'ONE':'Integer', X2:'FrozenSet'),
      f(difference, X1:'FrozenSet', X2:'FrozenSet', X3:'FrozenSet'),
      f(first, X3:'Container', X4:'Any'),
      f(rbind, gravitate, X4:'Any', X5:'Callable'),
      f(compose, crement, X5, X6:'Callable'),
      f(fork, shift, identity, X6, X7:'Callable'),
      f(mapply, X7, X2:'ContainerContainer', X8:'FrozenSet'),
      f(paint, IN:'Grid', X8:'Object', OUT:'Grid')
    ].
l_solve('31aa019c', IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(ofcolor, IN:'Grid', X1:'Integer', X2:'Indices'),
      f(first, X2:'Container', X3:'Any'),
      f(neighbors, X3:'Point', X4:'Indices'),
      f(astuple, 'TEN':'Integer', 'TEN':'Integer', X5:'Point'),
      f(canvas, 'ZERO':'Integer', X5:'Point', X6:'Grid'),
      f(initset, X3:'Any', X7:'FrozenSet'),
      f(fill, X6:'Grid', X1:'Integer', X7:'Patch', X8:'Grid'),
      f(fill, X8:'Grid', 'TWO':'Integer', X4:'Patch', OUT:'Grid')
    ].
l_solve('27a28665', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(valmax, X1:'Container', size, X2:'Integer'),
      f(equality, X2:'Any', 'ONE':'Any', returns=X3),
      f(equality, X2:'Any', 'FOUR':'Any', returns=X4),
      f(equality, X2:'Any', 'FIVE':'Any', returns=X5),
      f(branch, condition=X3, 'TWO':'Any', 'ONE':'Any', X6:'Any'),
      f(branch, condition=X4, 'THREE':'Any', X6:'Any', X7:'Any'),
      f(branch, condition=X5, 'SIX':'Any', X7:'Any', X8:'Any'),
      f(canvas, X8:'Integer', (1, 1):'Point', OUT:'Grid')
    ].
l_solve('7ddcd7ec', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(sizefilter, X1:'Container', 'ONE':'Integer', X2:'FrozenSet'),
      f(difference, X1:'FrozenSet', X2:'FrozenSet', X3:'FrozenSet'),
      f(first, X3:'Container', X4:'Any'),
      f(color, X4:'Object', X5:'Integer'),
      f(lbind, position, X4:'Any', X6:'Callable'),
      f(fork, shoot, center, X6, X7:'Callable'),
      f(mapply, X7, X2:'ContainerContainer', X8:'FrozenSet'),
      f(fill, IN:'Grid', X5:'Integer', X8:'Patch', OUT:'Grid')
    ].
l_solve('3bd67248', IN, OUT) :-
    [ f(height, IN:'Piece', X1:'Integer'),
      f(decrement, X1:'Numerical', X2:'Numerical'),
      f(decrement, X2:'Numerical', X3:'Numerical'),
      f(astuple, X3:'Integer', 'ONE':'Integer', X4:'Point'),
      f(astuple, X2:'Integer', 'ONE':'Integer', X5:'Point'),
      f(shoot, X4:'Point', (-1, 1):'Point', X6:'Indices'),
      f(shoot, X5:'Point', (0, 1):'Point', X7:'Indices'),
      f(fill, IN:'Grid', 'TWO':'Integer', X6:'Patch', X8:'Grid'),
      f(fill, X8:'Grid', 'FOUR':'Integer', X7:'Patch', OUT:'Grid')
    ].
l_solve('73251a56', IN, OUT) :-
    [ f(dmirror, IN:'Piece', X1:'Piece'),
      f(papply, pair, IN:'Tuple', X1:'Tuple', X2:'Tuple'),
      f(lbind, apply, maximum:'Any', X3:'Callable'),
      f(apply, X3, X2:'Container', X4:'Container'),
      f(mostcolor, X4:'Element', X5:'Integer'),
      f(replace, X4:'Grid', 'ZERO':'Integer', X5:'Integer', X6:'Grid'),
      f(index, X6:'Grid', (0, 0):'Point', X7:'Integer'),
      f(shoot, (0, 0):'Point', (1, 1):'Point', X8:'Indices'),
      f(fill, X6:'Grid', X7:'Integer', X8:'Patch', OUT:'Grid')
    ].
l_solve('25d487eb', IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X2:'Objects'),
      f(ofcolor, IN:'Grid', X1:'Integer', X3:'Indices'),
      f(center, X3:'Patch', X4:'Point'),
      f(merge, X2:'ContainerContainer', X5:'Container'),
      f(center, X5:'Patch', X6:'Point'),
      f(subtract, X6:'Numerical', X4:'Numerical', X7:'Numerical'),
      f(shoot, X4:'Point', X7:'Point', X8:'Indices'),
      f(underfill,
        IN:'Grid',
        X1:'Integer',
        X8:'Patch',
        OUT:'Grid')
    ].
l_solve('8f2ea7aa', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(merge, X1:'ContainerContainer', X2:'Container'),
      f(subgrid, X2:'Patch', IN:'Grid', X3:'Grid'),
      f(upscale, X3:'Element', 'THREE':'Integer', X4:'Element'),
      f(hconcat, X3:'Grid', X3:'Grid', X5:'Grid'),
      f(hconcat, X5:'Grid', X3:'Grid', X6:'Grid'),
      f(vconcat, X6:'Grid', X6:'Grid', X7:'Grid'),
      f(vconcat, X7:'Grid', X6:'Grid', X8:'Grid'),
      f(cellwise, X4:'Grid', X8:'Grid', 'ZERO':'Integer', OUT:'Grid')
    ].
l_solve(b8825c91, IN, OUT) :-
    [ f(replace, IN:'Grid', 'FOUR':'Integer', 'ZERO':'Integer', X1:'Grid'),
      f(dmirror, X1:'Piece', X2:'Piece'),
      f(papply, pair, X1:'Tuple', X2:'Tuple', X3:'Tuple'),
      f(lbind, apply, maximum:'Any', X4:'Callable'),
      f(apply, X4, X3:'Container', X5:'Container'),
      f(cmirror, X5:'Piece', X6:'Piece'),
      f(papply, pair, X5:'Tuple', X6:'Tuple', X7:'Tuple'),
      f(apply, X4, X7:'Container', X8:'Container'),
      f(cmirror, X8:'Piece', OUT:'Piece')
    ].
l_solve(cce03e0d, IN, OUT) :-
    [ f(upscale, IN:'Element', 'THREE':'Integer', X1:'Element'),
      f(hconcat, IN:'Grid', IN:'Grid', X2:'Grid'),
      f(hconcat, X2:'Grid', IN:'Grid', X3:'Grid'),
      f(vconcat, X3:'Grid', X3:'Grid', X4:'Grid'),
      f(vconcat, X4:'Grid', X3:'Grid', X5:'Grid'),
      f(ofcolor, X1:'Grid', 'ZERO':'Integer', X6:'Indices'),
      f(ofcolor, X1:'Grid', 'ONE':'Integer', X7:'Indices'),
      f(combine, X6:'Container', X7:'Container', X8:'Container'),
      f(fill, X5:'Grid', 'ZERO':'Integer', X8:'Patch', OUT:'Grid')
    ].
l_solve(d364b489, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'ONE':'Integer', X1:'Indices'),
      f(shift, X1:'Patch', (1, 0):'Point', X2:'Patch'),
      f(fill, IN:'Grid', 'EIGHT':'Integer', X2:'Patch', X3:'Grid'),
      f(shift, X1:'Patch', (-1, 0):'Point', X4:'Patch'),
      f(fill, X3:'Grid', 'TWO':'Integer', X4:'Patch', X5:'Grid'),
      f(shift, X1:'Patch', (0, 1):'Point', X6:'Patch'),
      f(fill, X5:'Grid', 'SIX':'Integer', X6:'Patch', X7:'Grid'),
      f(shift, X1:'Patch', (0, -1):'Point', X8:'Patch'),
      f(fill, X7:'Grid', 'SEVEN':'Integer', X8:'Patch', OUT:'Grid')
    ].
l_solve(a5f85a15, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(interval, 'ONE':'Integer', 'NINE':'Integer', 'ONE':'Integer', X2:'Tuple'),
      f(apply, double, X2:'Container', X3:'Container'),
      f(apply, decrement, X3:'Container', X4:'Container'),
      f(papply, astuple, X4:'Tuple', X4:'Tuple', X5:'Tuple'),
      f(apply, ulcorner, X1:'Container', X6:'Container'),
      f(lbind, shift, X5:'Any', X7:'Callable'),
      f(mapply, X7, X6:'ContainerContainer', X8:'FrozenSet'),
      f(fill, IN:'Grid', 'FOUR':'Integer', X8:'Patch', OUT:'Grid')
    ].
l_solve('3ac3eb23', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(chain, ineighbors, last, first, X2:'Callable'),
      f(fork, recolor, color, X2, X3:'Callable'),
      f(mapply, X3, X1:'ContainerContainer', X4:'FrozenSet'),
      f(paint, IN:'Grid', X4:'Object', X5:'Grid'),
      f(vsplit, X5:'Grid', 'THREE':'Integer', X6:'Tuple'),
      f(first, X6:'Container', X7:'Any'),
      f(vconcat, X7:'Grid', X7:'Grid', X8:'Grid'),
      f(vconcat, X7:'Grid', X8:'Grid', OUT:'Grid')
    ].
l_solve('444801d8', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'ONE':'Integer', X2:'Objects'),
      f(rbind, toobject, IN:'Any', X3:'Callable'),
      f(chain, leastcolor, X3, delta, X4:'Callable'),
      f(rbind, shift, (-1, 0):'Any', X5:'Callable'),
      f(compose, X5, backdrop, X6:'Callable'),
      f(fork, recolor, X4, X6, X7:'Callable'),
      f(mapply, X7, X2:'ContainerContainer', X8:'FrozenSet'),
      f(underpaint, IN:'Grid', X8:'Object', OUT:'Grid')
    ].
l_solve(22168020, IN, OUT) :-
    [ f(palette, IN:'Element', X1:'IntegerSet'),
      f(remove, 'ZERO':'Any', X1:'Container', X2:'Container'),
      f(lbind, ofcolor, IN:'Any', X3:'Callable'),
      f(lbind, prapply, connect:'Any', X4:'Callable'),
      f(fork, X4, X3, X3, X5:'Callable'),
      f(compose, merge, X5, X6:'Callable'),
      f(fork, recolor, identity, X6, X7:'Callable'),
      f(mapply, X7, X2:'ContainerContainer', X8:'FrozenSet'),
      f(paint, IN:'Grid', X8:'Object', OUT:'Grid')
    ].
l_solve('6e82a1ae', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(lbind, sizefilter, X1:'Any', X2:'Callable'),
      f(compose, merge, X2, X3:'Callable'),
      f(X3, 'TWO', X4),
      f(X3, 'THREE', X5),
      f(X3, 'FOUR', X6),
      f(fill, IN:'Grid', 'THREE':'Integer', X4:'Patch', X7:'Grid'),
      f(fill, X7:'Grid', 'TWO':'Integer', X5:'Patch', X8:'Grid'),
      f(fill, X8:'Grid', 'ONE':'Integer', X6:'Patch', OUT:'Grid')
    ].
l_solve(b2862040, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'NINE':'Integer', X2:'Objects'),
      f(colorfilter, X1:'Objects', 'ONE':'Integer', X3:'Objects'),
      f(rbind, bordering, IN:'Any', X4:'Callable'),
      f(compose, flip, X4, X5:'Callable'),
      f(mfilter, X2:'Container', X5, X6:'FrozenSet'),
      f(rbind, adjacent, X6:'Any', X7:'Callable'),
      f(mfilter, X3:'Container', X7, X8:'FrozenSet'),
      f(fill, IN:'Grid', 'EIGHT':'Integer', X8:'Patch', OUT:'Grid')
    ].
l_solve('868de0fa', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(sfilter, X1:'Container', square, X2:'Container'),
      f(compose, even, height, X3:'Callable'),
      f(sfilter, X2:'Container', X3, X4:'Container'),
      f(difference, X2:'FrozenSet', X4:'FrozenSet', X5:'FrozenSet'),
      f(merge, X4:'ContainerContainer', X6:'Container'),
      f(merge, X5:'ContainerContainer', X7:'Container'),
      f(fill, IN:'Grid', 'TWO':'Integer', X6:'Patch', X8:'Grid'),
      f(fill, X8:'Grid', 'SEVEN':'Integer', X7:'Patch', OUT:'Grid')
    ].
l_solve('681b3aeb', IN, OUT) :-
    [ f(rot270, IN:'Grid', X1:'Grid'),
      f(objects,
        X1:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X2:'Objects'),
      f(argmax, X2:'Container', size, X3:'Any'),
      f(argmin, X2:'Container', size, X4:'Any'),
      f(color, X4:'Object', X5:'Integer'),
      f(canvas, X5:'Integer', (3, 3):'Point', X6:'Grid'),
      f(normalize, X3:'Patch', X7:'Patch'),
      f(paint, X6:'Grid', X7:'Object', X8:'Grid'),
      f(rot90, X8:'Grid', OUT:'Grid')
    ].
l_solve('8e5a5113', IN, OUT) :-
    [ f(crop, IN:'Grid', (0, 0):'Point', (3, 3):'Point', X1:'Grid'),
      f(rot90, X1:'Grid', X2:'Grid'),
      f(rot180, X1:'Grid', X3:'Grid'),
      f(astuple, X2:'Integer', X3:'Integer', X4:'Point'),
      f(astuple, 'FOUR':'Integer', 'EIGHT':'Integer', X5:'Point'),
      f(apply, tojvec, X5:'Container', X6:'Container'),
      f(apply, asobject, X4:'Container', X7:'Container'),
      f(mpapply, shift, X7:'Tuple', X6:'Tuple', X8:'Tuple'),
      f(paint, IN:'Grid', X8:'Object', OUT:'Grid')
    ].
l_solve('025d127b', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(apply, color, X1:'Container', X2:'Container'),
      f(merge, X1:'ContainerContainer', X3:'Container'),
      f(lbind, colorfilter, X1:'Any', X4:'Callable'),
      f(rbind, argmax, rightmost:'Any', X5:'Callable'),
      f(compose, X5, X4, X6:'Callable'),
      f(mapply, X6, X2:'ContainerContainer', X7:'FrozenSet'),
      f(difference, X3:'FrozenSet', X7:'FrozenSet', X8:'FrozenSet'),
      f(move, IN:'Grid', X8:'Object', (0, 1):'Point', OUT:'Grid')
    ].
l_solve('2281f1f4', IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'FIVE':'Integer', X1:'Indices'),
      f(product, X1:'Container', X1:'Container', X2:'FrozenSet'),
      f(power, first, 'TWO':'Integer', X3:'Callable'),
      f(power, last, 'TWO':'Integer', X4:'Callable'),
      f(fork, astuple, X3, X4, X5:'Callable'),
      f(apply, X5, X2:'Container', X6:'Container'),
      f(urcorner, X1:'Patch', X7:'Point'),
      f(remove, X7:'Any', X6:'Container', X8:'Container'),
      f(underfill, IN:'Grid', 'TWO':'Integer', X8:'Patch', OUT:'Grid')
    ].
l_solve(cf98881b, IN, OUT) :-
    [ f(hsplit, IN:'Grid', 'THREE':'Integer', X1:'Tuple'),
      f(first, X1:'Container', X2:'Any'),
      f(remove, X2:'Any', X1:'Container', X3:'Container'),
      f(first, X3:'Container', X4:'Any'),
      f(last, X3:'Container', X5:'Any'),
      f(ofcolor, X4:'Grid', 'NINE':'Integer', X6:'Indices'),
      f(ofcolor, X2:'Grid', 'FOUR':'Integer', X7:'Indices'),
      f(fill, X5:'Grid', 'NINE':'Integer', X6:'Patch', X8:'Grid'),
      f(fill, X8:'Grid', 'FOUR':'Integer', X7:'Patch', OUT:'Grid')
    ].
l_solve(d4f3cd78, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'FIVE':'Integer', X1:'Indices'),
      f(delta, X1:'Patch', X2:'Indices'),
      f(fill, IN:'Grid', 'EIGHT':'Integer', X2:'Patch', X3:'Grid'),
      f(box, X1:'Patch', X4:'Indices'),
      f(difference, X4:'FrozenSet', X1:'FrozenSet', X5:'FrozenSet'),
      f(position, X4:'Patch', X5:'Patch', X6:'Point'),
      f(first, X5:'Container', X7:'Any'),
      f(shoot, X7:'Point', X6:'Point', X8:'Indices'),
      f(fill, X3:'Grid', 'EIGHT':'Integer', X8:'Patch', OUT:'Grid')
    ].
l_solve(bda2d7a6, IN, OUT) :-
    [ f(partition, IN:'Grid', X1:'Objects'),
      f(order, X1:'Container', size, X2:'Tuple'),
      f(apply, color, X2:'Container', X3:'Container'),
      f(last, X2:'Container', X4:'Any'),
      f(remove, X4:'Any', X2:'Container', X5:'Container'),
      f(repeat, X4:'Any', 'ONE':'Integer', X6:'Tuple'),
      f(combine, X6:'Container', X5:'Container', X7:'Container'),
      f(mpapply, recolor, X3:'Tuple', X7:'Tuple', X8:'Tuple'),
      f(paint, IN:'Grid', X8:'Object', OUT:'Grid')
    ].
l_solve('137eaa0f', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(matcher, first, 'FIVE':'Any', X2:'Callable'),
      f(rbind, sfilter, X2:'Any', X3:'Callable'),
      f(chain, invert, center, X3, X4:'Callable'),
      f(fork, shift, identity, X4, X5:'Callable'),
      f(canvas, 'ZERO':'Integer', (3, 3):'Point', X6:'Grid'),
      f(mapply, X5, X1:'ContainerContainer', X7:'FrozenSet'),
      f(shift, X7:'Patch', (1, 1):'Point', X8:'Patch'),
      f(paint, X6:'Grid', X8:'Object', OUT:'Grid')
    ].
l_solve('6455b5f5', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'ZERO':'Integer', X2:'Objects'),
      f(argmax, X1:'Container', size, X3:'Any'),
      f(valmin, X1:'Container', size, X4:'Integer'),
      f(sizefilter, X2:'Container', X4:'Integer', X5:'FrozenSet'),
      f(recolor, 'ONE':'Integer', X3:'Patch', X6:'Object'),
      f(merge, X5:'ContainerContainer', X7:'Container'),
      f(paint, IN:'Grid', X6:'Object', X8:'Grid'),
      f(fill, X8:'Grid', 'EIGHT':'Integer', X7:'Patch', OUT:'Grid')
    ].
l_solve(b8cdaf2b, IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(ofcolor, IN:'Grid', X1:'Integer', X2:'Indices'),
      f(shift, X2:'Patch', (-1, 0):'Point', X3:'Patch'),
      f(ulcorner, X3:'Patch', X4:'Point'),
      f(urcorner, X3:'Patch', X5:'Point'),
      f(shoot, X4:'Point', (-1, -1):'Point', X6:'Indices'),
      f(shoot, X5:'Point', (-1, 1):'Point', X7:'Indices'),
      f(combine, X6:'Container', X7:'Container', X8:'Container'),
      f(underfill,
        IN:'Grid',
        X1:'Integer',
        X8:'Patch',
        OUT:'Grid')
    ].
l_solve(bd4472b8, IN, OUT) :-
    [ f(width, IN:'Piece', X1:'Integer'),
      f(astuple, 'TWO':'Integer', X1:'Integer', X2:'Point'),
      f(crop, IN:'Grid', (0, 0):'Point', X2:'Point', X3:'Grid'),
      f(tophalf, X3:'Grid', X4:'Grid'),
      f(dmirror, X4:'Piece', X5:'Piece'),
      f(hupscale, X5:'Grid', X1:'Integer', X6:'Grid'),
      f(repeat, X6:'Any', 'TWO':'Integer', X7:'Tuple'),
      f(merge, X7:'ContainerContainer', X8:'Container'),
      f(vconcat, X3:'Grid', X8:'Grid', OUT:'Grid')
    ].
l_solve('4be741c5', IN, OUT) :-
    [ f(portrait, IN:'Piece', returns=X1),
      f(branch, condition=X1, dmirror:'Any', identity:'Any', X2:'Any'),
      f(branch, condition=X1, height:'Any', width:'Any', X3:'Any'),
      f(X3, IN, X4),
      f(astuple, 'ONE':'Integer', X4:'Integer', X5:'Point'),
      f(X2, IN, X6),
      f(crop, X6:'Grid', (0, 0):'Point', X5:'Point', X7:'Grid'),
      f(apply, dedupe, X7:'Container', X8:'Container'),
      f(X2, X8, OUT)
    ].
l_solve(bbc9ae5d, IN, OUT) :-
    [ f(width, IN:'Piece', X1:'Integer'),
      f(palette, IN:'Element', X2:'IntegerSet'),
      f(halve, X1:'Numerical', X3:'Numerical'),
      f(vupscale, IN:'Grid', X3:'Integer', X4:'Grid'),
      f(rbind, shoot, (1, 1):'Any', X5:'Callable'),
      f(other, X2:'Container', 'ZERO':'Any', X6:'Any'),
      f(ofcolor, X4:'Grid', X6:'Integer', X7:'Indices'),
      f(mapply, X5, X7:'ContainerContainer', X8:'FrozenSet'),
      f(fill, X4:'Grid', X6:'Integer', X8:'Patch', OUT:'Grid')
    ].
l_solve(d90796e8, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(sizefilter, X1:'Container', 'TWO':'Integer', X2:'FrozenSet'),
      f(lbind, contained, 'TWO':'Any', X3:'Callable'),
      f(compose, X3, palette, X4:'Callable'),
      f(mfilter, X2:'Container', X4, X5:'FrozenSet'),
      f(cover, IN:'Grid', X5:'Patch', X6:'Grid'),
      f(matcher, first, 'THREE':'Any', X7:'Callable'),
      f(sfilter, X5:'Container', X7, X8:'Container'),
      f(fill, X6:'Grid', 'EIGHT':'Integer', X8:'Patch', OUT:'Grid')
    ].
l_solve('2c608aff', IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X2:'Objects'),
      f(argmax, X2:'Container', size, X3:'Any'),
      f(toindices, X3:'Patch', X4:'Indices'),
      f(ofcolor, IN:'Grid', X1:'Integer', X5:'Indices'),
      f(prapply, connect, X4, X5, X6),
      f(fork, either, vline, hline, X7:'Callable'),
      f(mfilter, X6:'Container', X7, X8:'FrozenSet'),
      f(underfill,
        IN:'Grid',
        X1:'Integer',
        X8:'Patch',
        OUT:'Grid')
    ].
l_solve(f8b3ba0a, IN, OUT) :-
    [ f(compress, IN:'Grid', X1:'Grid'),
      f(astuple, 'THREE':'Integer', 'ONE':'Integer', X2:'Point'),
      f(palette, X1:'Element', X3:'IntegerSet'),
      f(lbind, colorcount, X1:'Any', X4:'Callable'),
      f(compose, invert, X4, X5:'Callable'),
      f(order, X3:'Container', X5, X6:'Tuple'),
      f(rbind, canvas, (1, 1):'Any', X7:'Callable'),
      f(apply, X7, X6:'Container', X8:'Container'),
      f(merge, X8:'ContainerContainer', X9:'Container'),
      f(crop, X9:'Grid', (1, 0):'Point', X2:'Point', OUT:'Grid')
    ].
l_solve('80af3007', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(first, X1:'Container', X2:'Any'),
      f(subgrid, X2:'Patch', IN:'Grid', X3:'Grid'),
      f(upscale, X3:'Element', 'THREE':'Integer', X4:'Element'),
      f(hconcat, X3:'Grid', X3:'Grid', X5:'Grid'),
      f(hconcat, X5:'Grid', X3:'Grid', X6:'Grid'),
      f(vconcat, X6:'Grid', X6:'Grid', X7:'Grid'),
      f(vconcat, X7:'Grid', X6:'Grid', X8:'Grid'),
      f(cellwise, X4:'Grid', X8:'Grid', 'ZERO':'Integer', X9:'Grid'),
      f(downscale, X9:'Grid', 'THREE':'Integer', OUT:'Grid')
    ].
l_solve('83302e8f', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'ZERO':'Integer', X2:'Objects'),
      f(sfilter, X2:'Container', square, X3:'Container'),
      f(difference, X2:'FrozenSet', X3:'FrozenSet', X4:'FrozenSet'),
      f(merge, X3:'ContainerContainer', X5:'Container'),
      f(recolor, 'THREE':'Integer', X5:'Patch', X6:'Object'),
      f(merge, X4:'ContainerContainer', X7:'Container'),
      f(recolor, 'FOUR':'Integer', X7:'Patch', X8:'Object'),
      f(paint, IN:'Grid', X6:'Object', X9:'Grid'),
      f(paint, X9:'Grid', X8:'Object', OUT:'Grid')
    ].
l_solve('1fad071e', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'ONE':'Integer', X2:'Objects'),
      f(sizefilter, X2:'Container', 'FOUR':'Integer', X3:'FrozenSet'),
      f(size, X3:'Container', X4:'Integer'),
      f(subtract, 'FIVE':'Numerical', X4:'Numerical', X5:'Numerical'),
      f(astuple, 'ONE':'Integer', X4:'Integer', X6:'Point'),
      f(canvas, 'ONE':'Integer', X6:'Point', X7:'Grid'),
      f(astuple, 'ONE':'Integer', X5:'Integer', X8:'Point'),
      f(canvas, 'ZERO':'Integer', X8:'Point', X9:'Grid'),
      f(hconcat, X7:'Grid', X9:'Grid', OUT:'Grid')
    ].
l_solve('11852cab', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(merge, X1:'ContainerContainer', X2:'Container'),
      f(hmirror, X2:'Piece', X3:'Piece'),
      f(vmirror, X2:'Piece', X4:'Piece'),
      f(dmirror, X2:'Piece', X5:'Piece'),
      f(cmirror, X2:'Piece', X6:'Piece'),
      f(paint, IN:'Grid', X3:'Object', X7:'Grid'),
      f(paint, X7:'Grid', X4:'Object', X8:'Grid'),
      f(paint, X8:'Grid', X5:'Object', X9:'Grid'),
      f(paint, X9:'Grid', X6:'Object', OUT:'Grid')
    ].
l_solve('3428a4f5', IN, OUT) :-
    [ f(tophalf, IN:'Grid', X1:'Grid'),
      f(bottomhalf, IN:'Grid', X2:'Grid'),
      f(astuple, 'SIX':'Integer', 'FIVE':'Integer', X3:'Point'),
      f(ofcolor, X1:'Grid', 'TWO':'Integer', X4:'Indices'),
      f(ofcolor, X2:'Grid', 'TWO':'Integer', X5:'Indices'),
      f(combine, X4:'Container', X5:'Container', X6:'Container'),
      f(intersection, X4:'FrozenSet', X5:'FrozenSet', X7:'FrozenSet'),
      f(difference, X6:'FrozenSet', X7:'FrozenSet', X8:'FrozenSet'),
      f(canvas, 'ZERO':'Integer', X3:'Point', X9:'Grid'),
      f(fill, X9:'Grid', 'THREE':'Integer', X8:'Patch', OUT:'Grid')
    ].
l_solve('178fcbfb', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(ofcolor, IN:'Grid', 'TWO':'Integer', X2:'Indices'),
      f(mapply, vfrontier, X2:'ContainerContainer', X3:'FrozenSet'),
      f(fill, IN:'Grid', 'TWO':'Integer', X3:'Patch', X4:'Grid'),
      f(colorfilter, X1:'Objects', 'TWO':'Integer', X5:'Objects'),
      f(difference, X1:'FrozenSet', X5:'FrozenSet', X6:'FrozenSet'),
      f(compose, hfrontier, center, X7:'Callable'),
      f(fork, recolor, color, X7, X8:'Callable'),
      f(mapply, X8, X6:'ContainerContainer', X9:'FrozenSet'),
      f(paint, X4:'Grid', X9:'Object', OUT:'Grid')
    ].
l_solve('3de23699', IN, OUT) :-
    [ f(fgpartition, IN:'Grid', X1:'Objects'),
      f(sizefilter, X1:'Container', 'FOUR':'Integer', X2:'FrozenSet'),
      f(first, X2:'Container', X3:'Any'),
      f(difference, X1:'FrozenSet', X2:'FrozenSet', X4:'FrozenSet'),
      f(first, X4:'Container', X5:'Any'),
      f(color, X3:'Object', X6:'Integer'),
      f(color, X5:'Object', X7:'Integer'),
      f(subgrid, X3:'Patch', IN:'Grid', X8:'Grid'),
      f(trim, X8:'Grid', X9:'Grid'),
      f(replace,
        X9:'Grid',
        X7:'Integer',
        X6:'Integer',
        OUT:'Grid')
    ].
l_solve('54d9e175', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(sizefilter, X1:'Container', 'ONE':'Integer', X2:'FrozenSet'),
      f(compose, neighbors, center, X3:'Callable'),
      f(fork, recolor, color, X3, X4:'Callable'),
      f(mapply, X4, X2:'ContainerContainer', X5:'FrozenSet'),
      f(paint, IN:'Grid', X5:'Object', X6:'Grid'),
      f(replace, X6:'Grid', 'ONE':'Integer', 'SIX':'Integer', X7:'Grid'),
      f(replace, X7:'Grid', 'TWO':'Integer', 'SEVEN':'Integer', X8:'Grid'),
      f(replace, X8:'Grid', 'THREE':'Integer', 'EIGHT':'Integer', X9:'Grid'),
      f(replace, X9:'Grid', 'FOUR':'Integer', 'NINE':'Integer', OUT:'Grid')
    ].
l_solve('5ad4f10b', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(argmax, X1:'Container', size, X2:'Any'),
      f(color, X2:'Object', X3:'Integer'),
      f(subgrid, X2:'Patch', IN:'Grid', X4:'Grid'),
      f(leastcolor, X4:'Element', X5:'Integer'),
      f(replace, X4:'Grid', X5:'Integer', 'ZERO':'Integer', X6:'Grid'),
      f(replace, X6:'Grid', X3:'Integer', X5:'Integer', X7:'Grid'),
      f(height, X7:'Piece', X8:'Integer'),
      f(divide, X8:'Numerical', 'THREE':'Numerical', X9:'Numerical'),
      f(downscale, X7:'Grid', X9:'Integer', OUT:'Grid')
    ].
l_solve('623ea044', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(first, X1:'Container', X2:'Any'),
      f(center, X2:'Patch', X3:'Point'),
      f(color, X2:'Object', X4:'Integer'),
      f(astuple, (1, 1):'Integer', (-1, -1):'Integer', X5:'Point'),
      f(astuple, (-1, 1):'Integer', (1, -1):'Integer', X6:'Point'),
      f(combine, X5:'Container', X6:'Container', X7:'Container'),
      f(lbind, shoot, X3:'Any', X8:'Callable'),
      f(mapply, X8, X7:'ContainerContainer', X9:'FrozenSet'),
      f(fill, IN:'Grid', X4:'Integer', X9:'Patch', OUT:'Grid')
    ].
l_solve('6b9890af', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(ofcolor, IN:'Grid', 'TWO':'Integer', X2:'Indices'),
      f(argmin, X1:'Container', size, X3:'Any'),
      f(subgrid, X2:'Patch', IN:'Grid', X4:'Grid'),
      f(width, X4:'Piece', X5:'Integer'),
      f(divide, X5:'Numerical', 'THREE':'Numerical', X6:'Numerical'),
      f(upscale, X3:'Element', X6:'Integer', X7:'Element'),
      f(normalize, X7:'Patch', X8:'Patch'),
      f(shift, X8:'Patch', (1, 1):'Point', X9:'Patch'),
      f(paint, X4:'Grid', X9:'Object', OUT:'Grid')
    ].
l_solve('794b24be', IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'ONE':'Integer', X1:'Indices'),
      f(size, X1:'Container', X2:'Integer'),
      f(decrement, X2:'Numerical', X3:'Numerical'),
      f(canvas, 'ZERO':'Integer', (3, 3):'Point', X4:'Grid'),
      f(tojvec, X3:'Integer', X5:'Point'),
      f(connect, (0, 0):'Point', X5:'Point', X6:'Indices'),
      f(equality, X2:'Any', 'FOUR':'Any', returns=X7),
      f(insert, (1, 1):'Any', X6:'FrozenSet', X8:'FrozenSet'),
      f(branch, condition=X7, X8:'Any', X6:'Any', X9:'Any'),
      f(fill, X4:'Grid', 'TWO':'Integer', X9:'Patch', OUT:'Grid')
    ].
l_solve('88a10436', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'FIVE':'Integer', X2:'Objects'),
      f(first, X2:'Container', X3:'Any'),
      f(center, X3:'Patch', X4:'Point'),
      f(difference, X1:'FrozenSet', X2:'FrozenSet', X5:'FrozenSet'),
      f(first, X5:'Container', X6:'Any'),
      f(normalize, X6:'Patch', X7:'Patch'),
      f(shift, X7:'Patch', X4:'Point', X8:'Patch'),
      f(shift, X8:'Patch', (-1, -1):'Point', X9:'Patch'),
      f(paint, IN:'Grid', X9:'Object', OUT:'Grid')
    ].
l_solve('88a62173', IN, OUT) :-
    [ f(lefthalf, IN:'Grid', X1:'Grid'),
      f(righthalf, IN:'Grid', X2:'Grid'),
      f(tophalf, X1:'Grid', X3:'Grid'),
      f(tophalf, X2:'Grid', X4:'Grid'),
      f(bottomhalf, X1:'Grid', X5:'Grid'),
      f(bottomhalf, X2:'Grid', X6:'Grid'),
      f(astuple, X3:'Integer', X4:'Integer', X7:'Point'),
      f(astuple, X5:'Integer', X6:'Integer', X8:'Point'),
      f(combine, X7:'Container', X8:'Container', X9:'Container'),
      f(leastcommon, X9:'Container', OUT:'Any')
    ].
l_solve(890034000000000.0, IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(ofcolor, IN:'Grid', X1:'Integer', X2:'Indices'),
      f(inbox, X2:'Patch', X3:'Indices'),
      f(recolor, 'ZERO':'Integer', X3:'Patch', X4:'Object'),
      f(occurrences, IN:'Grid', X4:'Object', X5:'Indices'),
      f(normalize, X2:'Patch', X6:'Patch'),
      f(shift, X6:'Patch', (-1, -1):'Point', X7:'Patch'),
      f(lbind, shift, X7:'Any', X8:'Callable'),
      f(mapply, X8, X5:'ContainerContainer', X9:'FrozenSet'),
      f(fill, IN:'Grid', X1:'Integer', X9:'Patch', OUT:'Grid')
    ].
l_solve('99b1bc43', IN, OUT) :-
    [ f(tophalf, IN:'Grid', X1:'Grid'),
      f(bottomhalf, IN:'Grid', X2:'Grid'),
      f(ofcolor, X1:'Grid', 'ZERO':'Integer', X3:'Indices'),
      f(ofcolor, X2:'Grid', 'ZERO':'Integer', X4:'Indices'),
      f(combine, X3:'Container', X4:'Container', X5:'Container'),
      f(intersection, X3:'FrozenSet', X4:'FrozenSet', X6:'FrozenSet'),
      f(difference, X5:'FrozenSet', X6:'FrozenSet', X7:'FrozenSet'),
      f(shape, X1:'Piece', X8:'Point'),
      f(canvas, 'ZERO':'Integer', X8:'Point', X9:'Grid'),
      f(fill, X9:'Grid', 'THREE':'Integer', X7:'Patch', OUT:'Grid')
    ].
l_solve(a9f96cdd, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'TWO':'Integer', X1:'Indices'),
      f(replace, IN:'Grid', 'TWO':'Integer', 'ZERO':'Integer', X2:'Grid'),
      f(shift, X1:'Patch', (-1, -1):'Point', X3:'Patch'),
      f(fill, X2:'Grid', 'THREE':'Integer', X3:'Patch', X4:'Grid'),
      f(shift, X1:'Patch', (-1, 1):'Point', X5:'Patch'),
      f(fill, X4:'Grid', 'SIX':'Integer', X5:'Patch', X6:'Grid'),
      f(shift, X1:'Patch', (1, -1):'Point', X7:'Patch'),
      f(fill, X6:'Grid', 'EIGHT':'Integer', X7:'Patch', X8:'Grid'),
      f(shift, X1:'Patch', (1, 1):'Point', X9:'Patch'),
      f(fill, X8:'Grid', 'SEVEN':'Integer', X9:'Patch', OUT:'Grid')
    ].
l_solve(af902bf9, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'FOUR':'Integer', X1:'Indices'),
      f(prapply, connect, X1, X1, X2),
      f(fork, either, vline, hline, X3:'Callable'),
      f(mfilter, X2:'Container', X3, X4:'FrozenSet'),
      f(underfill, IN:'Grid', 'NEG_ONE':'Integer', X4:'Patch', X5:'Grid'),
      f(objects,
        X5:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X6:'Objects'),
      f(compose, backdrop, inbox, X7:'Callable'),
      f(mapply, X7, X6:'ContainerContainer', X8:'FrozenSet'),
      f(fill, X5:'Grid', 'TWO':'Integer', X8:'Patch', X9:'Grid'),
      f(replace, X9:'Grid', 'NEG_ONE':'Integer', 'ZERO':'Integer', OUT:'Grid')
    ].
l_solve(b548a754, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(replace, IN:'Grid', 'EIGHT':'Integer', 'ZERO':'Integer', X2:'Grid'),
      f(leastcolor, X2:'Element', X3:'Integer'),
      f(replace, X2:'Grid', X3:'Integer', 'ZERO':'Integer', X4:'Grid'),
      f(leastcolor, X4:'Element', X5:'Integer'),
      f(merge, X1:'ContainerContainer', X6:'Container'),
      f(backdrop, X6:'Patch', X7:'Indices'),
      f(box, X6:'Patch', X8:'Indices'),
      f(fill, IN:'Grid', X3:'Integer', X7:'Patch', X9:'Grid'),
      f(fill, X9:'Grid', X5:'Integer', X8:'Patch', OUT:'Grid')
    ].
l_solve(bdad9b1f, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'TWO':'Integer', X1:'Indices'),
      f(ofcolor, IN:'Grid', 'EIGHT':'Integer', X2:'Indices'),
      f(center, X1:'Patch', X3:'Point'),
      f(center, X2:'Patch', X4:'Point'),
      f(hfrontier, X3:'Point', X5:'Indices'),
      f(vfrontier, X4:'Point', X6:'Indices'),
      f(intersection, X5:'FrozenSet', X6:'FrozenSet', X7:'FrozenSet'),
      f(fill, IN:'Grid', 'TWO':'Integer', X5:'Patch', X8:'Grid'),
      f(fill, X8:'Grid', 'EIGHT':'Integer', X6:'Patch', X9:'Grid'),
      f(fill, X9:'Grid', 'FOUR':'Integer', X7:'Patch', OUT:'Grid')
    ].
l_solve(c3e719e8, IN, OUT) :-
    [ f(mostcolor, IN:'Element', X1:'Integer'),
      f(hconcat, IN:'Grid', IN:'Grid', X2:'Grid'),
      f(upscale, IN:'Element', 'THREE':'Integer', X3:'Element'),
      f(ofcolor, X3:'Grid', X1:'Integer', X4:'Indices'),
      f(asindices, X3:'Grid', X5:'Indices'),
      f(difference, X5:'FrozenSet', X4:'FrozenSet', X6:'FrozenSet'),
      f(hconcat, X2:'Grid', IN:'Grid', X7:'Grid'),
      f(vconcat, X7:'Grid', X7:'Grid', X8:'Grid'),
      f(vconcat, X8:'Grid', X7:'Grid', X9:'Grid'),
      f(fill, X9:'Grid', 'ZERO':'Integer', X6:'Patch', OUT:'Grid')
    ].
l_solve(de1cd16c, IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X2:'Objects'),
      f(sizefilter, X2:'Container', 'ONE':'Integer', X3:'FrozenSet'),
      f(difference, X2:'FrozenSet', X3:'FrozenSet', X4:'FrozenSet'),
      f(rbind, subgrid, IN:'Any', X5:'Callable'),
      f(apply, X5, X4:'Container', X6:'Container'),
      f(rbind, colorcount, X1:'Any', X7:'Callable'),
      f(argmax, X6:'Container', X7, X8:'Any'),
      f(mostcolor, X8:'Element', X9:'Integer'),
      f(canvas, X9:'Integer', (1, 1):'Point', OUT:'Grid')
    ].
l_solve(d8c310e9, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(first, X1:'Container', X2:'Any'),
      f(hperiod, X2:'Object', X3:'Integer'),
      f(multiply, X3:'Numerical', 'THREE':'Numerical', X4:'Numerical'),
      f(tojvec, X3:'Integer', X5:'Point'),
      f(tojvec, X4:'Integer', X6:'Point'),
      f(shift, X2:'Patch', X5:'Point', X7:'Patch'),
      f(shift, X2:'Patch', X6:'Point', X8:'Patch'),
      f(paint, IN:'Grid', X7:'Object', X9:'Grid'),
      f(paint, X9:'Grid', X8:'Object', OUT:'Grid')
    ].
l_solve(a3325580, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(valmax, X1:'Container', size, X2:'Integer'),
      f(sizefilter, X1:'Container', X2:'Integer', X3:'FrozenSet'),
      f(order, X3:'Container', leftmost, X4:'Tuple'),
      f(apply, color, X4:'Container', X5:'Container'),
      f(astuple, 'ONE':'Integer', X2:'Integer', X6:'Point'),
      f(rbind, canvas, X6:'Any', X7:'Callable'),
      f(apply, X7, X5:'Container', X8:'Container'),
      f(merge, X8:'ContainerContainer', X9:'Container'),
      f(dmirror, X9:'Piece', OUT:'Piece')
    ].
l_solve('8eb1be9a', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(first, X1:'Container', X2:'Any'),
      f(interval, 'NEG_TWO':'Integer', 'FOUR':'Integer', 'ONE':'Integer', X3:'Tuple'),
      f(lbind, shift, X2:'Any', X4:'Callable'),
      f(height, X2:'Piece', X5:'Integer'),
      f(rbind, multiply, X5:'Any', X6:'Callable'),
      f(apply, X6, X3:'Container', X7:'Container'),
      f(apply, toivec, X7:'Container', X8:'Container'),
      f(mapply, X4, X8:'ContainerContainer', X9:'FrozenSet'),
      f(paint, IN:'Grid', X9:'Object', OUT:'Grid')
    ].
l_solve('321b1fc6', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'EIGHT':'Integer', X2:'Objects'),
      f(difference, X1:'FrozenSet', X2:'FrozenSet', X3:'FrozenSet'),
      f(first, X3:'Container', X4:'Any'),
      f(cover, IN:'Grid', X4:'Patch', X5:'Grid'),
      f(normalize, X4:'Patch', X6:'Patch'),
      f(lbind, shift, X6:'Any', X7:'Callable'),
      f(apply, ulcorner, X2:'Container', X8:'Container'),
      f(mapply, X7, X8:'ContainerContainer', X9:'FrozenSet'),
      f(paint, X5:'Grid', X9:'Object', OUT:'Grid')
    ].
l_solve('1caeab9d', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(ofcolor, IN:'Grid', 'ONE':'Integer', X2:'Indices'),
      f(lowermost, X2:'Patch', X3:'Integer'),
      f(lbind, subtract, X3:'Any', X4:'Callable'),
      f(chain, toivec, X4, lowermost, X5:'Callable'),
      f(fork, shift, identity, X5, X6:'Callable'),
      f(merge, X1:'ContainerContainer', X7:'Container'),
      f(cover, IN:'Grid', X7:'Patch', X8:'Grid'),
      f(mapply, X6, X1:'ContainerContainer', X9:'FrozenSet'),
      f(paint, X8:'Grid', X9:'Object', OUT:'Grid')
    ].
l_solve('77fdfe62', IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'EIGHT':'Integer', X1:'Indices'),
      f(subgrid, X1:'Patch', IN:'Grid', X2:'Grid'),
      f(replace, IN:'Grid', 'EIGHT':'Integer', 'ZERO':'Integer', X3:'Grid'),
      f(replace, X3:'Grid', 'ONE':'Integer', 'ZERO':'Integer', X4:'Grid'),
      f(compress, X4:'Grid', X5:'Grid'),
      f(width, X2:'Piece', X6:'Integer'),
      f(halve, X6:'Numerical', X7:'Numerical'),
      f(upscale, X5:'Element', X7:'Integer', X8:'Element'),
      f(ofcolor, X2:'Grid', 'ZERO':'Integer', X9:'Indices'),
      f(fill, X8:'Grid', 'ZERO':'Integer', X9:'Patch', OUT:'Grid')
    ].
l_solve(c0f76784, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'ZERO':'Integer', X2:'Objects'),
      f(sfilter, X2:'Container', square, X3:'Container'),
      f(sizefilter, X3:'Container', 'ONE':'Integer', X4:'FrozenSet'),
      f(merge, X4:'ContainerContainer', X5:'Container'),
      f(argmax, X3:'Container', size, X6:'Any'),
      f(merge, X3:'ContainerContainer', X7:'Container'),
      f(fill, IN:'Grid', 'SEVEN':'Integer', X7:'Patch', X8:'Grid'),
      f(fill, X8:'Grid', 'EIGHT':'Integer', X6:'Patch', X9:'Grid'),
      f(fill, X9:'Grid', 'SIX':'Integer', X5:'Patch', OUT:'Grid')
    ].
l_solve('1b60fb0c', IN, OUT) :-
    [ f(rot90, IN:'Grid', X1:'Grid'),
      f(ofcolor, IN:'Grid', 'ONE':'Integer', X2:'Indices'),
      f(ofcolor, X1:'Grid', 'ONE':'Integer', X3:'Indices'),
      f(neighbors, (0, 0):'Point', X4:'Indices'),
      f(mapply, neighbors, X4:'ContainerContainer', X5:'FrozenSet'),
      f(lbind, shift, X3:'Any', X6:'Callable'),
      f(apply, X6, X5:'Container', X7:'Container'),
      f(lbind, intersection, X2:'Any', X8:'Callable'),
      f(argmax, X7:'Container', X8, X9:'Any'),
      f(underfill, IN:'Grid', 'TWO':'Integer', X9:'Patch', OUT:'Grid')
    ].
l_solve(ddf7fa4f, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(sizefilter, X1:'Container', 'ONE':'Integer', X2:'FrozenSet'),
      f(colorfilter, X1:'Objects', 'FIVE':'Integer', X3:'Objects'),
      f(product, X2:'Container', X3:'Container', X4:'FrozenSet'),
      f(fork, vmatching, first, last, X5:'Callable'),
      f(sfilter, X4:'Container', X5, X6:'Container'),
      f(compose, color, first, X7:'Callable'),
      f(fork, recolor, X7, last, X8:'Callable'),
      f(mapply, X8, X6:'ContainerContainer', X9:'FrozenSet'),
      f(paint, IN:'Grid', X9:'Object', OUT:'Grid')
    ].
l_solve('47c1f68c', IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(vmirror, IN:'Piece', X2:'Piece'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X3:'Objects'),
      f(merge, X3:'ContainerContainer', X4:'Container'),
      f(mostcolor, X4:'Element', X5:'Integer'),
      f(cellwise, IN:'Grid', X2:'Grid', X1:'Integer', X6:'Grid'),
      f(hmirror, X6:'Piece', X7:'Piece'),
      f(cellwise, X6:'Grid', X7:'Grid', X1:'Integer', X8:'Grid'),
      f(compress, X8:'Grid', X9:'Grid'),
      f(replace,
        X9:'Grid',
        X1:'Integer',
        X5:'Integer',
        OUT:'Grid')
    ].
l_solve('6c434453', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(sizefilter, X1:'Container', 'EIGHT':'Integer', X2:'FrozenSet'),
      f(dneighbors, (1, 1):'Point', X3:'Indices'),
      f(insert, (1, 1):'Any', X3:'FrozenSet', X4:'FrozenSet'),
      f(merge, X2:'ContainerContainer', X5:'Container'),
      f(cover, IN:'Grid', X5:'Patch', X6:'Grid'),
      f(apply, ulcorner, X2:'Container', X7:'Container'),
      f(lbind, shift, X4:'Any', X8:'Callable'),
      f(mapply, X8, X7:'ContainerContainer', X9:'FrozenSet'),
      f(fill, X6:'Grid', 'TWO':'Integer', X9:'Patch', OUT:'Grid')
    ].
l_solve(23581191, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(fork, combine, vfrontier, hfrontier, X2:'Callable'),
      f(compose, X2, center, X3:'Callable'),
      f(fork, recolor, color, X3, X4:'Callable'),
      f(mapply, X4, X1:'ContainerContainer', X5:'FrozenSet'),
      f(paint, IN:'Grid', X5:'Object', X6:'Grid'),
      f(fork, intersection, first, last, X7:'Callable'),
      f(apply, X3, X1:'Container', X8:'Container'),
      f(X7, X8, X9),
      f(fill, X6:'Grid', 'TWO':'Integer', X9:'Patch', OUT:'Grid')
    ].
l_solve(c8cbb738, IN, OUT) :-
    [ f(mostcolor, IN:'Element', X1:'Integer'),
      f(fgpartition, IN:'Grid', X2:'Objects'),
      f(valmax, X2:'Container', shape, X3:'Integer'),
      f(canvas, X1:'Integer', X3:'Point', X4:'Grid'),
      f(apply, normalize, X2:'Container', X5:'Container'),
      f(lbind, subtract, X3:'Any', X6:'Callable'),
      f(chain, halve, X6, shape, X7:'Callable'),
      f(fork, shift, identity, X7, X8:'Callable'),
      f(mapply, X8, X5:'ContainerContainer', X9:'FrozenSet'),
      f(paint, X4:'Grid', X9:'Object', OUT:'Grid')
    ].
l_solve('3eda0437', IN, OUT) :-
    [ f(interval, 'TWO':'Integer', 'TEN':'Integer', 'ONE':'Integer', X1:'Tuple'),
      f(prapply, astuple, X1, X1, X2),
      f(lbind, canvas, 'ZERO':'Any', X3:'Callable'),
      f(lbind, occurrences, IN:'Any', X4:'Callable'),
      f(lbind, lbind, shift:'Any', X5:'Callable'),
      f(fork, apply, X5, X4, X6:'Callable'),
      f(chain, X6, asobject, X3, X7:'Callable'),
      f(mapply, X7, X2:'ContainerContainer', X8:'FrozenSet'),
      f(argmax, X8:'Container', size, X9:'Any'),
      f(fill, IN:'Grid', 'SIX':'Integer', X9:'Patch', OUT:'Grid')
    ].
l_solve(dc0a314f, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'THREE':'Integer', X1:'Indices'),
      f(replace, IN:'Grid', 'THREE':'Integer', 'ZERO':'Integer', X2:'Grid'),
      f(dmirror, X2:'Piece', X3:'Piece'),
      f(papply, pair, X2:'Tuple', X3:'Tuple', X4:'Tuple'),
      f(lbind, apply, maximum:'Any', X5:'Callable'),
      f(apply, X5, X4:'Container', X6:'Container'),
      f(cmirror, X6:'Piece', X7:'Piece'),
      f(papply, pair, X6:'Tuple', X7:'Tuple', X8:'Tuple'),
      f(apply, X5, X8:'Container', X9:'Container'),
      f(subgrid, X1:'Patch', X9:'Grid', OUT:'Grid')
    ].
l_solve(d4469b4b, IN, OUT) :-
    [ f(palette, IN:'Element', X1:'IntegerSet'),
      f(other, X1:'Container', 'ZERO':'Any', X2:'Any'),
      f(equality, X2:'Any', 'ONE':'Any', returns=X3),
      f(equality, X2:'Any', 'TWO':'Any', returns=X4),
      f(branch, condition=X3, (1, 1):'Any', (2, 2):'Any', X5:'Any'),
      f(branch, condition=X4, (0, 1):'Any', X5:'Any', X6:'Any'),
      f(fork, combine, vfrontier, hfrontier, X7:'Callable'),
      f(X7, X6, X8),
      f(canvas, 'ZERO':'Integer', (3, 3):'Point', X9:'Grid'),
      f(fill, X9:'Grid', 'FIVE':'Integer', X8:'Patch', OUT:'Grid')
    ].
l_solve('6ecd11f4', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(argmax, X1:'Container', size, X2:'Any'),
      f(argmin, X1:'Container', size, X3:'Any'),
      f(subgrid, X2:'Patch', IN:'Grid', X4:'Grid'),
      f(subgrid, X3:'Patch', IN:'Grid', X5:'Grid'),
      f(width, X4:'Piece', X6:'Integer'),
      f(width, X5:'Piece', X7:'Integer'),
      f(divide, X6:'Numerical', X7:'Numerical', X8:'Numerical'),
      f(downscale, X4:'Grid', X8:'Integer', X9:'Grid'),
      f(ofcolor, X9:'Grid', 'ZERO':'Integer', X10:'Indices'),
      f(fill, X5:'Grid', 'ZERO':'Integer', X10:'Patch', OUT:'Grid')
    ].
l_solve('760b3cac', IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'FOUR':'Integer', X1:'Indices'),
      f(ofcolor, IN:'Grid', 'EIGHT':'Integer', X2:'Indices'),
      f(ulcorner, X1:'Patch', X3:'Point'),
      f(index, IN:'Grid', X3:'Point', X4:'Integer'),
      f(equality, X4:'Any', 'FOUR':'Any', returns=X5),
      f(branch, condition=X5, 'NEG_ONE':'Any', 'ONE':'Any', X6:'Any'),
      f(multiply, X6:'Numerical', 'THREE':'Numerical', X7:'Numerical'),
      f(tojvec, X7:'Integer', X8:'Point'),
      f(vmirror, X2:'Piece', X9:'Piece'),
      f(shift, X9:'Patch', X8:'Point', X10:'Patch'),
      f(fill, IN:'Grid', 'EIGHT':'Integer', X10:'Patch', OUT:'Grid')
    ].
l_solve(c444b776, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'ZERO':'Integer', X2:'Objects'),
      f(argmin, X2:'Container', size, X3:'Any'),
      f(backdrop, X3:'Patch', X4:'Indices'),
      f(toobject, X4:'Patch', IN:'Grid', X5:'Object'),
      f(normalize, X5:'Patch', X6:'Patch'),
      f(lbind, shift, X6:'Any', X7:'Callable'),
      f(compose, X7, ulcorner, X8:'Callable'),
      f(remove, X3:'Any', X2:'Container', X9:'Container'),
      f(mapply, X8, X9:'ContainerContainer', X10:'FrozenSet'),
      f(paint, IN:'Grid', X10:'Object', OUT:'Grid')
    ].
l_solve(d4a91cb9, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'EIGHT':'Integer', X1:'Indices'),
      f(ofcolor, IN:'Grid', 'TWO':'Integer', X2:'Indices'),
      f(first, X1:'Container', X3:'Any'),
      f(first, X2:'Container', X4:'Any'),
      f(last, X3:'Container', X5:'Any'),
      f(first, X4:'Container', X6:'Any'),
      f(astuple, X6:'Integer', X5:'Integer', X7:'Point'),
      f(connect, X7:'Point', X3:'Point', X8:'Indices'),
      f(connect, X7:'Point', X4:'Point', X9:'Indices'),
      f(combine, X8:'Container', X9:'Container', X10:'Container'),
      f(underfill, IN:'Grid', 'FOUR':'Integer', X10:'Patch', OUT:'Grid')
    ].
l_solve(eb281b96, IN, OUT) :-
    [ f(height, IN:'Piece', X1:'Integer'),
      f(width, IN:'Piece', X2:'Integer'),
      f(decrement, X1:'Numerical', X3:'Numerical'),
      f(astuple, X3:'Integer', X2:'Integer', X4:'Point'),
      f(crop, IN:'Grid', (0, 0):'Point', X4:'Point', X5:'Grid'),
      f(hmirror, X5:'Piece', X6:'Piece'),
      f(vconcat, IN:'Grid', X6:'Grid', X7:'Grid'),
      f(double, X3:'Numerical', X8:'Numerical'),
      f(astuple, X8:'Integer', X2:'Integer', X9:'Point'),
      f(crop, X7:'Grid', (1, 0):'Point', X9:'Point', X10:'Grid'),
      f(vconcat, X7:'Grid', X10:'Grid', OUT:'Grid')
    ].
l_solve(ff28f65a, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'TWO':'Integer', X2:'Objects'),
      f(size, X2:'Container', X3:'Integer'),
      f(double, X3:'Numerical', X4:'Numerical'),
      f(interval, 'ZERO':'Integer', X4:'Integer', 'TWO':'Integer', X5:'Tuple'),
      f(apply, tojvec, X5:'Container', X6:'Container'),
      f(astuple, 'ONE':'Integer', 'NINE':'Integer', X7:'Point'),
      f(canvas, 'ZERO':'Integer', X7:'Point', X8:'Grid'),
      f(fill, X8:'Grid', 'ONE':'Integer', X6:'Patch', X9:'Grid'),
      f(hsplit, X9:'Grid', 'THREE':'Integer', X10:'Tuple'),
      f(merge, X10:'ContainerContainer', OUT:'Container')
    ].
l_solve('7e0986d6', IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(ofcolor, IN:'Grid', X1:'Integer', X2:'Indices'),
      f(replace, IN:'Grid', X1:'Integer', 'ZERO':'Integer', X3:'Grid'),
      f(leastcolor, X3:'Element', X4:'Integer'),
      f(rbind, colorcount, X4:'Any', X5:'Callable'),
      f(rbind, greater, 'ONE':'Any', X6:'Callable'),
      f(compose, X6, X5, X7:'Callable'),
      f(rbind, toobject, X3:'Any', X8:'Callable'),
      f(chain, X7, X8, dneighbors, X9:'Callable'),
      f(sfilter, X2:'Container', X9, X10:'Container'),
      f(fill, X3:'Grid', X4:'Integer', X10:'Patch', OUT:'Grid')
    ].
l_solve('09629e4f', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(rbind, subgrid, IN:'Any', X2:'Callable'),
      f(apply, X2, X1:'Container', X3:'Container'),
      f(argmin, X3:'Container', numcolors, X4:'Any'),
      f(upscale, X4:'Element', 'FOUR':'Integer', X5:'Element'),
      f(ofcolor, IN:'Grid', 'FIVE':'Integer', X6:'Indices'),
      f(shift, X6:'Patch', (1, 1):'Point', X7:'Patch'),
      f(fill, X5:'Grid', 'FIVE':'Integer', X7:'Patch', X8:'Grid'),
      f(shape, X8:'Piece', X9:'Point'),
      f(decrement, X9:'Numerical', X10:'Numerical'),
      f(crop, X8:'Grid', (1, 1):'Point', X10:'Point', OUT:'Grid')
    ].
l_solve(a85d4709, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'FIVE':'Integer', X1:'Indices'),
      f(lbind, matcher, last:'Any', X2:'Callable'),
      f(lbind, sfilter, X1:'Any', X3:'Callable'),
      f(lbind, mapply, hfrontier:'Any', X4:'Callable'),
      f(chain, X4, X3, X2, X5:'Callable'),
      f(X5, 'ZERO', X6),
      f(X5, 'TWO', X7),
      f(X5, 'ONE', X8),
      f(fill, IN:'Grid', 'TWO':'Integer', X6:'Patch', X9:'Grid'),
      f(fill, X9:'Grid', 'THREE':'Integer', X7:'Patch', X10:'Grid'),
      f(fill, X10:'Grid', 'FOUR':'Integer', X8:'Patch', OUT:'Grid')
    ].
l_solve(feca6190, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(size, X1:'Container', X2:'Integer'),
      f(multiply, X2:'Numerical', 'FIVE':'Numerical', X3:'Numerical'),
      f(astuple, X3:'Integer', X3:'Integer', X4:'Point'),
      f(canvas, 'ZERO':'Integer', X4:'Point', X5:'Grid'),
      f(rbind, shoot, (1, 1):'Any', X6:'Callable'),
      f(compose, X6, center, X7:'Callable'),
      f(fork, recolor, color, X7, X8:'Callable'),
      f(mapply, X8, X1:'ContainerContainer', X9:'FrozenSet'),
      f(paint, X5:'Grid', X9:'Object', X10:'Grid'),
      f(hmirror, X10:'Piece', OUT:'Piece')
    ].
l_solve(a68b268e, IN, OUT) :-
    [ f(tophalf, IN:'Grid', X1:'Grid'),
      f(bottomhalf, IN:'Grid', X2:'Grid'),
      f(lefthalf, X1:'Grid', X3:'Grid'),
      f(righthalf, X1:'Grid', X4:'Grid'),
      f(lefthalf, X2:'Grid', X5:'Grid'),
      f(righthalf, X2:'Grid', X6:'Grid'),
      f(ofcolor, X4:'Grid', 'FOUR':'Integer', X7:'Indices'),
      f(ofcolor, X3:'Grid', 'SEVEN':'Integer', X8:'Indices'),
      f(ofcolor, X5:'Grid', 'EIGHT':'Integer', X9:'Indices'),
      f(fill, X6:'Grid', 'EIGHT':'Integer', X9:'Patch', X10:'Grid'),
      f(fill, X10:'Grid', 'FOUR':'Integer', X7:'Patch', X11:'Grid'),
      f(fill, X11:'Grid', 'SEVEN':'Integer', X8:'Patch', OUT:'Grid')
    ].
l_solve(beb8660c, IN, OUT) :-
    [ f(shape, IN:'Piece', X1:'Point'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X2:'Objects'),
      f(compose, invert, size, X3:'Callable'),
      f(order, X2:'Container', X3, X4:'Tuple'),
      f(apply, normalize, X4:'Container', X5:'Container'),
      f(size, X5:'Container', X6:'Integer'),
      f(interval, 'ZERO':'Integer', X6:'Integer', 'ONE':'Integer', X7:'Tuple'),
      f(apply, toivec, X7:'Container', X8:'Container'),
      f(mpapply, shift, X5:'Tuple', X8:'Tuple', X9:'Tuple'),
      f(canvas, 'ZERO':'Integer', X1:'Point', X10:'Grid'),
      f(paint, X10:'Grid', X9:'Object', X11:'Grid'),
      f(rot180, X11:'Grid', OUT:'Grid')
    ].
l_solve('913fb3ed', IN, OUT) :-
    [ f(lbind, ofcolor, IN:'Any', X1:'Callable'),
      f(lbind, mapply, neighbors:'Any', X2:'Callable'),
      f(chain, X2, X1, last, X3:'Callable'),
      f(fork, recolor, first, X3, X4:'Callable'),
      f(astuple, 'SIX':'Integer', 'THREE':'Integer', X5:'Point'),
      f(astuple, 'FOUR':'Integer', 'EIGHT':'Integer', X6:'Point'),
      f(astuple, 'ONE':'Integer', 'TWO':'Integer', X7:'Point'),
      f(initset, X5:'Any', X8:'FrozenSet'),
      f(insert, X6:'Any', X8:'FrozenSet', X9:'FrozenSet'),
      f(insert, X7:'Any', X9:'FrozenSet', X10:'FrozenSet'),
      f(mapply, X4, X10:'ContainerContainer', X11:'FrozenSet'),
      f(paint, IN:'Grid', X11:'Object', OUT:'Grid')
    ].
l_solve('0962bcdd', IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(replace, IN:'Grid', 'ZERO':'Integer', X1:'Integer', X2:'Grid'),
      f(leastcolor, X2:'Element', X3:'Integer'),
      f(ofcolor, IN:'Grid', X3:'Integer', X4:'Indices'),
      f(mapply, dneighbors, X4:'ContainerContainer', X5:'FrozenSet'),
      f(fill, IN:'Grid', X3:'Integer', X5:'Patch', X6:'Grid'),
      f(objects,
        X6:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X7:'Objects'),
      f(fork, connect, ulcorner, lrcorner, X8:'Callable'),
      f(fork, connect, llcorner, urcorner, X9:'Callable'),
      f(fork, combine, X8, X9, X10:'Callable'),
      f(mapply, X10, X7:'ContainerContainer', X11:'FrozenSet'),
      f(fill, X6:'Grid', X1:'Integer', X11:'Patch', OUT:'Grid')
    ].
l_solve('3631a71a', IN, OUT) :-
    [ f(shape, IN:'Piece', X1:'Point'),
      f(replace, IN:'Grid', 'NINE':'Integer', 'ZERO':'Integer', X2:'Grid'),
      f(lbind, apply, maximum:'Any', X3:'Callable'),
      f(dmirror, X2:'Piece', X4:'Piece'),
      f(papply, pair, X2:'Tuple', X4:'Tuple', X5:'Tuple'),
      f(apply, X3, X5:'Container', X6:'Container'),
      f(subtract, X1:'Numerical', (2, 2):'Numerical', X7:'Numerical'),
      f(crop, X6:'Grid', (2, 2):'Point', X7:'Point', X8:'Grid'),
      f(vmirror, X8:'Piece', X9:'Piece'),
      f(objects,
        X9:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X10:'Objects'),
      f(merge, X10:'ContainerContainer', X11:'Container'),
      f(shift, X11:'Patch', (2, 2):'Point', X12:'Patch'),
      f(paint, X6:'Grid', X12:'Object', OUT:'Grid')
    ].
l_solve(5269061, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(neighbors, (0, 0):'Point', X2:'Indices'),
      f(mapply, neighbors, X2:'ContainerContainer', X3:'FrozenSet'),
      f(rbind, multiply, 'THREE':'Any', X4:'Callable'),
      f(apply, X4, X3:'Container', X5:'Container'),
      f(merge, X1:'ContainerContainer', X6:'Container'),
      f(lbind, shift, X6:'Any', X7:'Callable'),
      f(mapply, X7, X5:'ContainerContainer', X8:'FrozenSet'),
      f(shift, X8:'Patch', (-1, 1):'Point', X9:'Patch'),
      f(shift, X8:'Patch', (1, -1):'Point', X10:'Patch'),
      f(paint, IN:'Grid', X8:'Object', X11:'Grid'),
      f(paint, X11:'Grid', X9:'Object', X12:'Grid'),
      f(paint, X12:'Grid', X10:'Object', OUT:'Grid')
    ].
l_solve(95990924, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(apply, ulcorner, X1:'Container', X2:'Container'),
      f(apply, urcorner, X1:'Container', X3:'Container'),
      f(apply, llcorner, X1:'Container', X4:'Container'),
      f(apply, lrcorner, X1:'Container', X5:'Container'),
      f(shift, X2:'Patch', (-1, -1):'Point', X6:'Patch'),
      f(shift, X3:'Patch', (-1, 1):'Point', X7:'Patch'),
      f(shift, X4:'Patch', (1, -1):'Point', X8:'Patch'),
      f(shift, X5:'Patch', (1, 1):'Point', X9:'Patch'),
      f(fill, IN:'Grid', 'ONE':'Integer', X6:'Patch', X10:'Grid'),
      f(fill, X10:'Grid', 'TWO':'Integer', X7:'Patch', X11:'Grid'),
      f(fill, X11:'Grid', 'THREE':'Integer', X8:'Patch', X12:'Grid'),
      f(fill, X12:'Grid', 'FOUR':'Integer', X9:'Patch', OUT:'Grid')
    ].
l_solve(e509e548, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(rbind, subgrid, IN:'Any', X2:'Callable'),
      f(chain, palette, trim, X2, X3:'Callable'),
      f(lbind, contained, 'THREE':'Any', X4:'Callable'),
      f(compose, X4, X3, X5:'Callable'),
      f(fork, add, height, width, X6:'Callable'),
      f(compose, decrement, X6, X7:'Callable'),
      f(fork, equality, size, X7, X8:'Callable'),
      f(mfilter, X1:'Container', X5, X9:'FrozenSet'),
      f(mfilter, X1:'Container', X8, X10:'FrozenSet'),
      f(replace, IN:'Grid', 'THREE':'Integer', 'SIX':'Integer', X11:'Grid'),
      f(fill, X11:'Grid', 'TWO':'Integer', X9:'Patch', X12:'Grid'),
      f(fill, X12:'Grid', 'ONE':'Integer', X10:'Patch', OUT:'Grid')
    ].
l_solve(d43fd935, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(ofcolor, IN:'Grid', 'THREE':'Integer', X2:'Indices'),
      f(sizefilter, X1:'Container', 'ONE':'Integer', X3:'FrozenSet'),
      f(rbind, vmatching, X2:'Any', X4:'Callable'),
      f(rbind, hmatching, X2:'Any', X5:'Callable'),
      f(fork, either, X4, X5, X6:'Callable'),
      f(sfilter, X3:'Container', X6, X7:'Container'),
      f(rbind, gravitate, X2:'Any', X8:'Callable'),
      f(fork, add, center, X8, X9:'Callable'),
      f(fork, connect, center, X9, X10:'Callable'),
      f(fork, recolor, color, X10, X11:'Callable'),
      f(mapply, X11, X7:'ContainerContainer', X12:'FrozenSet'),
      f(paint, IN:'Grid', X12:'Object', OUT:'Grid')
    ].
l_solve(db3e9e38, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'SEVEN':'Integer', X1:'Indices'),
      f(lrcorner, X1:'Patch', X2:'Point'),
      f(shoot, X2:'Point', (-1, 1):'Point', X3:'Indices'),
      f(shoot, X2:'Point', (-1, -1):'Point', X4:'Indices'),
      f(combine, X3:'Container', X4:'Container', X5:'Container'),
      f(rbind, shoot, (-1, 0):'Any', X6:'Callable'),
      f(mapply, X6, X5:'ContainerContainer', X7:'FrozenSet'),
      f(last, X2:'Container', X8:'Any'),
      f(rbind, subtract, X8:'Any', X9:'Callable'),
      f(chain, even, X9, last, X10:'Callable'),
      f(fill, IN:'Grid', 'EIGHT':'Integer', X7:'Patch', X11:'Grid'),
      f(sfilter, X7:'Container', X10, X12:'Container'),
      f(fill, X11:'Grid', 'SEVEN':'Integer', X12:'Patch', OUT:'Grid')
    ].
l_solve(e73095fd, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'ZERO':'Integer', X2:'Objects'),
      f(fork, equality, toindices, backdrop, X3:'Callable'),
      f(sfilter, X2:'Container', X3, X4:'Container'),
      f(lbind, mapply, dneighbors:'Any', X5:'Callable'),
      f(chain, X5, corners, outbox, X6:'Callable'),
      f(fork, difference, X6, outbox, X7:'Callable'),
      f(ofcolor, IN:'Grid', 'FIVE':'Integer', X8:'Indices'),
      f(rbind, intersection, X8:'Any', X9:'Callable'),
      f(matcher, size, 'ZERO':'Any', X10:'Callable'),
      f(chain, X10, X9, X7, X11:'Callable'),
      f(mfilter, X4:'Container', X11, X12:'FrozenSet'),
      f(fill, IN:'Grid', 'FOUR':'Integer', X12:'Patch', OUT:'Grid')
    ].
l_solve('1bfc4729', IN, OUT) :-
    [ f(asindices, IN:'Grid', X1:'Indices'),
      f(tophalf, IN:'Grid', X2:'Grid'),
      f(bottomhalf, IN:'Grid', X3:'Grid'),
      f(leastcolor, X2:'Element', X4:'Integer'),
      f(leastcolor, X3:'Element', X5:'Integer'),
      f(ofcolor, X2:'Grid', X4:'Integer', X6:'Indices'),
      f(first, X6:'Container', X7:'Any'),
      f(hfrontier, X7:'Point', X8:'Indices'),
      f(box, X1:'Patch', X9:'Indices'),
      f(combine, X8:'Container', X9:'Container', X10:'Container'),
      f(fill, X2:'Grid', X4:'Integer', X10:'Patch', X11:'Grid'),
      f(hmirror, X11:'Piece', X12:'Piece'),
      f(replace,
        X12:'Grid',
        X4:'Integer',
        X5:'Integer',
        X13:'Grid'),
      f(vconcat, X11:'Grid', X13:'Grid', OUT:'Grid')
    ].
l_solve('93b581b8', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(first, X1:'Container', X2:'Any'),
      f(dmirror, X2:'Piece', X3:'Piece'),
      f(cmirror, X3:'Piece', X4:'Piece'),
      f(upscale, X4:'Element', 'THREE':'Integer', X5:'Element'),
      f(astuple, 'NEG_TWO':'Integer', 'NEG_TWO':'Integer', X6:'Point'),
      f(shift, X5:'Patch', X6:'Point', X7:'Patch'),
      f(underpaint, IN:'Grid', X7:'Object', X8:'Grid'),
      f(toindices, X2:'Patch', X9:'Indices'),
      f(mapply, vfrontier, X9:'ContainerContainer', X10:'FrozenSet'),
      f(mapply, hfrontier, X9:'ContainerContainer', X11:'FrozenSet'),
      f(combine, X10:'Container', X11:'Container', X12:'Container'),
      f(fill, X8:'Grid', 'ZERO':'Integer', X12:'Patch', X13:'Grid'),
      f(paint, X13:'Grid', X2:'Object', OUT:'Grid')
    ].
l_solve('9edfc990', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'ZERO':'Integer', X2:'Objects'),
      f(ofcolor, IN:'Grid', 'ONE':'Integer', X3:'Indices'),
      f(rbind, adjacent, X3:'Any', X4:'Callable'),
      f(mfilter, X2:'Container', X4, X5:'FrozenSet'),
      f(recolor, 'ONE':'Integer', X5:'Patch', X6:'Object'),
      f(paint, IN:'Grid', X6:'Object', X7:'Grid'),
      f(add, 'NINE':'Numerical', 'FOUR':'Numerical', X8:'Numerical'),
      f(astuple, 'SIX':'Integer', X8:'Integer', X9:'Point'),
      f(initset, X9:'Any', X10:'FrozenSet'),
      f(fill, X7:'Grid', 'ZERO':'Integer', X10:'Patch', X11:'Grid'),
      f(index, X7:'Grid', X9:'Point', X12:'Integer'),
      f(equality, X12:'Any', 'ONE':'Any', returns=X13),
      f(branch, condition=X13, X11:'Any', X7:'Any', OUT:'Any')
    ].
l_solve(a65b410d, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'TWO':'Integer', X1:'Indices'),
      f(urcorner, X1:'Patch', X2:'Point'),
      f(add, X2:'Numerical', (-1, 1):'Numerical', X3:'Numerical'),
      f(add, X2:'Numerical', (1, -1):'Numerical', X4:'Numerical'),
      f(shoot, X3:'Point', (-1, 1):'Point', X5:'Indices'),
      f(shoot, X4:'Point', (1, -1):'Point', X6:'Indices'),
      f(fill, IN:'Grid', 'THREE':'Integer', X5:'Patch', X7:'Grid'),
      f(fill, X7:'Grid', 'ONE':'Integer', X6:'Patch', X8:'Grid'),
      f(objects,
        X8:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X9:'Objects'),
      f(rbind, shoot, (0, -1):'Any', X10:'Callable'),
      f(compose, X10, urcorner, X11:'Callable'),
      f(fork, recolor, color, X11, X12:'Callable'),
      f(mapply, X12, X9:'ContainerContainer', X13:'FrozenSet'),
      f(paint, X8:'Grid', X13:'Object', OUT:'Grid')
    ].
l_solve('7447852a', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'ZERO':'Integer', X2:'Objects'),
      f(compose, last, center, X3:'Callable'),
      f(order, X2:'Container', X3, X4:'Tuple'),
      f(size, X4:'Container', X5:'Integer'),
      f(interval, 'ZERO':'Integer', X5:'Integer', 'THREE':'Integer', X6:'Tuple'),
      f(rbind, contained, X6:'Any', X7:'Callable'),
      f(compose, X7, last, X8:'Callable'),
      f(interval, 'ZERO':'Integer', X5:'Integer', 'ONE':'Integer', X9:'Tuple'),
      f(pair, X4:'Tuple', X9:'Tuple', X10:'TupleTuple'),
      f(sfilter, X10:'Container', X8, X11:'Container'),
      f(mapply, first, X11:'ContainerContainer', X12:'FrozenSet'),
      f(recolor, 'FOUR':'Integer', X12:'Patch', X13:'Object'),
      f(paint, IN:'Grid', X13:'Object', OUT:'Grid')
    ].
l_solve(97999447, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(apply, toindices, X1:'Container', X2:'Container'),
      f(rbind, shoot, (0, 1):'Any', X3:'Callable'),
      f(compose, X3, center, X4:'Callable'),
      f(fork, recolor, color, X4, X5:'Callable'),
      f(mapply, X5, X1:'ContainerContainer', X6:'FrozenSet'),
      f(paint, IN:'Grid', X6:'Object', X7:'Grid'),
      f(interval, 'ZERO':'Integer', 'FIVE':'Integer', 'ONE':'Integer', X8:'Tuple'),
      f(apply, double, X8:'Container', X9:'Container'),
      f(apply, increment, X9:'Container', X10:'Container'),
      f(apply, tojvec, X10:'Container', X11:'Container'),
      f(prapply, shift, X2, X11, X12),
      f(merge, X12:'ContainerContainer', X13:'Container'),
      f(fill, X7:'Grid', 'FIVE':'Integer', X13:'Patch', OUT:'Grid')
    ].
l_solve('91714a58', IN, OUT) :-
    [ f(shape, IN:'Piece', X1:'Point'),
      f(asindices, IN:'Grid', X2:'Indices'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X3:'Objects'),
      f(argmax, X3:'Container', size, X4:'Any'),
      f(mostcolor, X4:'Element', X5:'Integer'),
      f(canvas, 'ZERO':'Integer', X1:'Point', X6:'Grid'),
      f(paint, X6:'Grid', X4:'Object', X7:'Grid'),
      f(rbind, toobject, X7:'Any', X8:'Callable'),
      f(rbind, colorcount, X5:'Any', X9:'Callable'),
      f(chain, X9, X8, neighbors, X10:'Callable'),
      f(lbind, greater, 'THREE':'Any', X11:'Callable'),
      f(compose, X11, X10, X12:'Callable'),
      f(sfilter, X2:'Container', X12, X13:'Container'),
      f(fill, X7:'Grid', 'ZERO':'Integer', X13:'Patch', OUT:'Grid')
    ].
l_solve(a61ba2ce, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(lbind, index, IN:'Any', X2:'Callable'),
      f(matcher, X2, 'ZERO':'Any', X3:'Callable'),
      f(lbind, extract, X1:'Any', X4:'Callable'),
      f(rbind, subgrid, IN:'Any', X5:'Callable'),
      f(lbind, compose, X3:'Any', X6:'Callable'),
      f(chain, X5, X4, X6, X7:'Callable'),
      f(X7, ulcorner, X8),
      f(X7, urcorner, X9),
      f(X7, llcorner, X10),
      f(X7, lrcorner, X11),
      f(hconcat, X11:'Grid', X10:'Grid', X12:'Grid'),
      f(hconcat, X9:'Grid', X8:'Grid', X13:'Grid'),
      f(vconcat, X12:'Grid', X13:'Grid', OUT:'Grid')
    ].
l_solve('8e1813be', IN, OUT) :-
    [ f(replace, IN:'Grid', 'FIVE':'Integer', 'ZERO':'Integer', X1:'Grid'),
      f(objects,
        X1:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X2:'Objects'),
      f(first, X2:'Container', X3:'Any'),
      f(vline, X3:'Patch', returns=X4),
      f(branch, condition=X4, dmirror:'Any', identity:'Any', X5:'Any'),
      f(X5, X1, X6),
      f(objects,
        X6:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X7:'Objects'),
      f(order, X7:'Container', uppermost, X8:'Tuple'),
      f(apply, color, X8:'Container', X9:'Container'),
      f(dedupe, X9:'Tuple', X10:'Tuple'),
      f(size, X10:'Container', X11:'Integer'),
      f(rbind, repeat, X11:'Any', X12:'Callable'),
      f(apply, X12, X10:'Container', X13:'Container'),
      f(X5, X13, OUT)
    ].
l_solve(bc1d5164, IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(crop, IN:'Grid', (0, 0):'Point', (3, 3):'Point', X2:'Grid'),
      f(crop, IN:'Grid', (2, 0):'Point', (3, 3):'Point', X3:'Grid'),
      f(tojvec, 'FOUR':'Integer', X4:'Point'),
      f(crop, IN:'Grid', X4:'Point', (3, 3):'Point', X5:'Grid'),
      f(astuple, 'TWO':'Integer', 'FOUR':'Integer', X6:'Point'),
      f(crop, IN:'Grid', X6:'Point', (3, 3):'Point', X7:'Grid'),
      f(canvas, 'ZERO':'Integer', (3, 3):'Point', X8:'Grid'),
      f(rbind, ofcolor, X1:'Any', X9:'Callable'),
      f(astuple, X2:'Integer', X3:'Integer', X10:'Point'),
      f(astuple, X5:'Integer', X7:'Integer', X11:'Point'),
      f(combine, X10:'Container', X11:'Container', X12:'Container'),
      f(mapply, X9, X12:'ContainerContainer', X13:'FrozenSet'),
      f(fill, X8:'Grid', X1:'Integer', X13:'Patch', OUT:'Grid')
    ].
l_solve(ce602527, IN, OUT) :-
    [ f(vmirror, IN:'Piece', X1:'Piece'),
      f(fgpartition, X1:'Grid', X2:'Objects'),
      f(order, X2:'Container', size, X3:'Tuple'),
      f(last, X3:'Container', X4:'Any'),
      f(remove, X4:'Any', X3:'Container', X5:'Container'),
      f(compose, toindices, normalize, X6:'Callable'),
      f(rbind, upscale, 'TWO':'Any', X7:'Callable'),
      f(chain, toindices, X7, normalize, X8:'Callable'),
      f(X6, X4, X9),
      f(rbind, intersection, X9:'Any', X10:'Callable'),
      f(chain, size, X10, X8, X11:'Callable'),
      f(argmax, X5:'Container', X11, X12:'Any'),
      f(subgrid, X12:'Patch', X1:'Grid', X13:'Grid'),
      f(vmirror, X13:'Piece', OUT:'Piece')
    ].
l_solve('5c2c9af4', IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(ofcolor, IN:'Grid', X1:'Integer', X2:'Indices'),
      f(center, X2:'Patch', X3:'Point'),
      f(ulcorner, X2:'Patch', X4:'Point'),
      f(subtract, X3:'Numerical', X4:'Numerical', X5:'Numerical'),
      f(multiply, 'NEG_ONE':'Numerical', 'NINE':'Numerical', X6:'Numerical'),
      f(interval, 'ZERO':'Integer', 'NINE':'Integer', 'ONE':'Integer', X7:'Tuple'),
      f(interval, 'ZERO':'Integer', X6:'Integer', 'NEG_ONE':'Integer', X8:'Tuple'),
      f(lbind, multiply, X5:'Any', X9:'Callable'),
      f(apply, X9, X7:'Container', X10:'Container'),
      f(apply, X9, X8:'Container', X11:'Container'),
      f(pair, X10:'Tuple', X11:'Tuple', X12:'TupleTuple'),
      f(mapply, box, X12:'ContainerContainer', X13:'FrozenSet'),
      f(shift, X13:'Patch', X3:'Point', X14:'Patch'),
      f(fill, IN:'Grid', X1:'Integer', X14:'Patch', OUT:'Grid')
    ].
l_solve('75b8110e', IN, OUT) :-
    [ f(lefthalf, IN:'Grid', X1:'Grid'),
      f(righthalf, IN:'Grid', X2:'Grid'),
      f(tophalf, X1:'Grid', X3:'Grid'),
      f(bottomhalf, X1:'Grid', X4:'Grid'),
      f(tophalf, X2:'Grid', X5:'Grid'),
      f(bottomhalf, X2:'Grid', X6:'Grid'),
      f(rbind, ofcolor, 'ZERO':'Any', X7:'Callable'),
      f(fork, difference, asindices, X7, X8:'Callable'),
      f(fork, toobject, X8, identity, X9:'Callable'),
      f(X9, X5, X10),
      f(X9, X4, X11),
      f(X9, X6, X12),
      f(paint, X3:'Grid', X12:'Object', X13:'Grid'),
      f(paint, X13:'Grid', X11:'Object', X14:'Grid'),
      f(paint, X14:'Grid', X10:'Object', OUT:'Grid')
    ].
l_solve('941d9a10', IN, OUT) :-
    [ f(shape, IN:'Piece', X1:'Point'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X2:'Objects'),
      f(colorfilter, X2:'Objects', 'ZERO':'Integer', X3:'Objects'),
      f(apply, toindices, X3:'Container', X4:'Container'),
      f(lbind, lbind, contained:'Any', X5:'Callable'),
      f(lbind, extract, X4:'Any', X6:'Callable'),
      f(compose, X6, X5, X7:'Callable'),
      f(decrement, X1:'Numerical', X8:'Numerical'),
      f(astuple, 'FIVE':'Integer', 'FIVE':'Integer', X9:'Point'),
      f(X7, (0, 0), X10),
      f(X7, X8, X11),
      f(X7, X9, X12),
      f(fill, IN:'Grid', 'ONE':'Integer', X10:'Patch', X13:'Grid'),
      f(fill, X13:'Grid', 'THREE':'Integer', X11:'Patch', X14:'Grid'),
      f(fill, X14:'Grid', 'TWO':'Integer', X12:'Patch', OUT:'Grid')
    ].
l_solve(c3f564a4, IN, OUT) :-
    [ f(asindices, IN:'Grid', X1:'Indices'),
      f(dmirror, IN:'Piece', X2:'Piece'),
      f(invert, 'NINE':'Numerical', X3:'Numerical'),
      f(papply, pair, IN:'Tuple', X2:'Tuple', X4:'Tuple'),
      f(lbind, apply, maximum:'Any', X5:'Callable'),
      f(apply, X5, X4:'Container', X6:'Container'),
      f(ofcolor, X6:'Grid', 'ZERO':'Integer', X7:'Indices'),
      f(difference, X1:'FrozenSet', X7:'FrozenSet', X8:'FrozenSet'),
      f(toobject, X8:'Patch', X6:'Grid', X9:'Object'),
      f(interval, X3:'Integer', 'NINE':'Integer', 'ONE':'Integer', X10:'Tuple'),
      f(interval,
        'NINE':'Integer',
        X3:'Integer',
        'NEG_ONE':'Integer',
        X11:'Tuple'),
      f(pair, X10:'Tuple', X11:'Tuple', X12:'TupleTuple'),
      f(lbind, shift, X9:'Any', X13:'Callable'),
      f(mapply, X13, X12:'ContainerContainer', X14:'FrozenSet'),
      f(paint, X6:'Grid', X14:'Object', OUT:'Grid')
    ].
l_solve('1a07d186', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(sizefilter, X1:'Container', 'ONE':'Integer', X2:'FrozenSet'),
      f(difference, X1:'FrozenSet', X2:'FrozenSet', X3:'FrozenSet'),
      f(apply, color, X3:'Container', X4:'Container'),
      f(rbind, contained, X4:'Any', X5:'Callable'),
      f(compose, X5, color, X6:'Callable'),
      f(sfilter, X2:'Container', X6, X7:'Container'),
      f(lbind, colorfilter, X3:'Any', X8:'Callable'),
      f(chain, first, X8, color, X9:'Callable'),
      f(fork, gravitate, identity, X9, X10:'Callable'),
      f(fork, shift, identity, X10, X11:'Callable'),
      f(mapply, X11, X7:'ContainerContainer', X12:'FrozenSet'),
      f(merge, X2:'ContainerContainer', X13:'Container'),
      f(cover, IN:'Grid', X13:'Patch', X14:'Grid'),
      f(paint, X14:'Grid', X12:'Object', OUT:'Grid')
    ].
l_solve(d687bc17, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(sizefilter, X1:'Container', 'ONE':'Integer', X2:'FrozenSet'),
      f(difference, X1:'FrozenSet', X2:'FrozenSet', X3:'FrozenSet'),
      f(apply, color, X3:'Container', X4:'Container'),
      f(rbind, contained, X4:'Any', X5:'Callable'),
      f(compose, X5, color, X6:'Callable'),
      f(sfilter, X2:'Container', X6, X7:'Container'),
      f(lbind, colorfilter, X3:'Any', X8:'Callable'),
      f(chain, first, X8, color, X9:'Callable'),
      f(fork, gravitate, identity, X9, X10:'Callable'),
      f(fork, shift, identity, X10, X11:'Callable'),
      f(merge, X2:'ContainerContainer', X12:'Container'),
      f(mapply, X11, X7:'ContainerContainer', X13:'FrozenSet'),
      f(cover, IN:'Grid', X12:'Patch', X14:'Grid'),
      f(paint, X14:'Grid', X13:'Object', OUT:'Grid')
    ].
l_solve('9af7a82c', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(order, X1:'Container', size, X2:'Tuple'),
      f(valmax, X1:'Container', size, X3:'Integer'),
      f(rbind, astuple, 'ONE':'Any', X4:'Callable'),
      f(lbind, subtract, X3:'Any', X5:'Callable'),
      f(compose, X4, size, X6:'Callable'),
      f(chain, X4, X5, size, X7:'Callable'),
      f(fork, canvas, color, X6, X8:'Callable'),
      f(lbind, canvas, 'ZERO':'Any', X9:'Callable'),
      f(compose, X9, X7, X10:'Callable'),
      f(fork, vconcat, X8, X10, X11:'Callable'),
      f(compose, cmirror, X11, X12:'Callable'),
      f(apply, X12, X2:'Container', X13:'Container'),
      f(merge, X13:'ContainerContainer', X14:'Container'),
      f(cmirror, X14:'Piece', OUT:'Piece')
    ].
l_solve('6e19193c', IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X2:'Objects'),
      f(rbind, toobject, IN:'Any', X3:'Callable'),
      f(compose, first, delta, X4:'Callable'),
      f(rbind, colorcount, X1:'Any', X5:'Callable'),
      f(matcher, X5, 'TWO':'Any', X6:'Callable'),
      f(chain, X6, X3, dneighbors, X7:'Callable'),
      f(rbind, sfilter, X7:'Any', X8:'Callable'),
      f(chain, first, X8, toindices, X9:'Callable'),
      f(fork, subtract, X4, X9, X10:'Callable'),
      f(fork, shoot, X4, X10, X11:'Callable'),
      f(mapply, X11, X2:'ContainerContainer', X12:'FrozenSet'),
      f(fill, IN:'Grid', X1:'Integer', X12:'Patch', X13:'Grid'),
      f(mapply, delta, X2:'ContainerContainer', X14:'FrozenSet'),
      f(fill, X13:'Grid', 'ZERO':'Integer', X14:'Patch', OUT:'Grid')
    ].
l_solve(ef135b50, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'TWO':'Integer', X1:'Indices'),
      f(ofcolor, IN:'Grid', 'ZERO':'Integer', X2:'Indices'),
      f(product, X1:'Container', X1:'Container', X3:'FrozenSet'),
      f(power, first, 'TWO':'Integer', X4:'Callable'),
      f(compose, first, last, X5:'Callable'),
      f(fork, equality, X4, X5, X6:'Callable'),
      f(sfilter, X3:'Container', X6, X7:'Container'),
      f(fork, connect, first, last, X8:'Callable'),
      f(mapply, X8, X7:'ContainerContainer', X9:'FrozenSet'),
      f(intersection,
        X9:'FrozenSet',
        X2:'FrozenSet',
        X10:'FrozenSet'),
      f(fill, IN:'Grid', 'NINE':'Integer', X10:'Patch', X11:'Grid'),
      f(trim, X11:'Grid', X12:'Grid'),
      f(asobject, X12:'Grid', X13:'Object'),
      f(shift, X13:'Patch', (1, 1):'Point', X14:'Patch'),
      f(paint, IN:'Grid', X14:'Object', OUT:'Grid')
    ].
l_solve(cbded52d, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(sizefilter, X1:'Container', 'ONE':'Integer', X2:'FrozenSet'),
      f(product, X2:'Container', X2:'Container', X3:'FrozenSet'),
      f(fork, vmatching, first, last, X4:'Callable'),
      f(fork, hmatching, first, last, X5:'Callable'),
      f(fork, either, X4, X5, X6:'Callable'),
      f(sfilter, X3:'Container', X6, X7:'Container'),
      f(compose, center, first, X8:'Callable'),
      f(compose, center, last, X9:'Callable'),
      f(fork, connect, X8, X9, X10:'Callable'),
      f(chain, initset, center, X10, X11:'Callable'),
      f(compose, color, first, X12:'Callable'),
      f(fork, recolor, X12, X11, X13:'Callable'),
      f(mapply, X13, X7:'ContainerContainer', X14:'FrozenSet'),
      f(paint, IN:'Grid', X14:'Object', OUT:'Grid')
    ].
l_solve('8a004b2b', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(ofcolor, IN:'Grid', 'FOUR':'Integer', X2:'Indices'),
      f(subgrid, X2:'Patch', IN:'Grid', X3:'Grid'),
      f(argmax, X1:'Container', lowermost, X4:'Any'),
      f(normalize, X4:'Patch', X5:'Patch'),
      f(replace, X3:'Grid', 'FOUR':'Integer', 'ZERO':'Integer', X6:'Grid'),
      f(objects,
        X6:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X7:'Objects'),
      f(merge, X7:'ContainerContainer', X8:'Container'),
      f(width, X8:'Piece', X9:'Integer'),
      f(ulcorner, X8:'Patch', X10:'Point'),
      f(width, X4:'Piece', X11:'Integer'),
      f(divide, X9:'Numerical', X11:'Numerical', X12:'Numerical'),
      f(upscale, X5:'Element', X12:'Integer', X13:'Element'),
      f(shift, X13:'Patch', X10:'Point', X14:'Patch'),
      f(paint, X3:'Grid', X14:'Object', OUT:'Grid')
    ].
l_solve(e26a3af2, IN, OUT) :-
    [ f(rot90, IN:'Grid', X1:'Grid'),
      f(apply, mostcommon, IN:'Container', X2:'Container'),
      f(apply, mostcommon, X1:'Container', X3:'Container'),
      f(repeat, X2:'Any', 'ONE':'Integer', X4:'Tuple'),
      f(repeat, X3:'Any', 'ONE':'Integer', X5:'Tuple'),
      f(compose, size, dedupe, X6:'Callable'),
      f(X6, X2, X7),
      f(X6, X3, X8),
      f(greater, X8:'Integer', X7:'Integer', returns=X9),
      f(branch, condition=X9, height:'Any', width:'Any', X10:'Any'),
      f(X10, IN, X11),
      f(rot90, X4:'Grid', X12:'Grid'),
      f(branch, condition=X9, X5:'Any', X12:'Any', X13:'Any'),
      f(branch, condition=X9, vupscale:'Any', hupscale:'Any', X14:'Any'),
      f(X14, X13, X11, OUT)
    ].
l_solve('6cf79266', IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'ZERO':'Integer', X1:'Indices'),
      f(astuple, 'ZERO':'Integer', (0, 0):'Integer', X2:'Point'),
      f(initset, X2:'Any', X3:'FrozenSet'),
      f(upscale, X3:'Element', 'THREE':'Integer', X4:'Element'),
      f(toindices, X4:'Patch', X5:'Indices'),
      f(lbind, shift, X5:'Any', X6:'Callable'),
      f(rbind, difference, X1:'Any', X7:'Callable'),
      f(chain, size, X7, X6, X8:'Callable'),
      f(matcher, X8, 'ZERO':'Any', X9:'Callable'),
      f(lbind, add, (-1, -1):'Any', X10:'Callable'),
      f(chain, flip, X9, X10, X11:'Callable'),
      f(fork, both, X9, X11, X12:'Callable'),
      f(sfilter, X1:'Container', X12, X13:'Container'),
      f(mapply, X6, X13:'ContainerContainer', X14:'FrozenSet'),
      f(fill, IN:'Grid', 'ONE':'Integer', X14:'Patch', OUT:'Grid')
    ].
l_solve(a87f7484, IN, OUT) :-
    [ f(palette, IN:'Element', X1:'IntegerSet'),
      f(dmirror, IN:'Piece', X2:'Piece'),
      f(portrait, IN:'Piece', returns=X3),
      f(branch, condition=X3, X2:'Any', IN:'Any', X4:'Any'),
      f(size, X1:'Container', X5:'Integer'),
      f(decrement, X5:'Numerical', X6:'Numerical'),
      f(hsplit, X4:'Grid', X6:'Integer', X7:'Tuple'),
      f(rbind, ofcolor, 'ZERO':'Any', X8:'Callable'),
      f(apply, X8, X7:'Container', X9:'Container'),
      f(mostcommon, X9:'Container', X10:'Any'),
      f(matcher, X8, X10:'Any', X11:'Callable'),
      f(compose, flip, X11, X12:'Callable'),
      f(extract, X7:'Container', X12, X13:'Any'),
      f(dmirror, X13:'Piece', X14:'Piece'),
      f(branch, condition=X3, X14:'Any', X13:'Any', OUT:'Any')
    ].
l_solve('4093f84a', IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(replace, IN:'Grid', X1:'Integer', 'FIVE':'Integer', X2:'Grid'),
      f(rot270, X2:'Grid', X3:'Grid'),
      f(ofcolor, IN:'Grid', 'FIVE':'Integer', X4:'Indices'),
      f(portrait, X4:'Piece', returns=X5),
      f(branch, condition=X5, X2:'Any', X3:'Any', X6:'Any'),
      f(lefthalf, X6:'Grid', X7:'Grid'),
      f(righthalf, X6:'Grid', X8:'Grid'),
      f(rbind, order, identity:'Any', X9:'Callable'),
      f(rbind, order, invert:'Any', X10:'Callable'),
      f(apply, X9, X7:'Container', X11:'Container'),
      f(apply, X10, X8:'Container', X12:'Container'),
      f(hconcat, X11:'Grid', X12:'Grid', X13:'Grid'),
      f(rot90, X13:'Grid', X14:'Grid'),
      f(branch, condition=X5, X13:'Any', X14:'Any', OUT:'Any')
    ].
l_solve(ba26e723, IN, OUT) :-
    [ f(width, IN:'Piece', X1:'Integer'),
      f(hsplit, IN:'Grid', X1:'Integer', X2:'Tuple'),
      f(interval, 'ZERO':'Integer', X1:'Integer', 'ONE':'Integer', X3:'Tuple'),
      f(rbind, divide, 'THREE':'Any', X4:'Callable'),
      f(rbind, multiply, 'THREE':'Any', X5:'Callable'),
      f(compose, X5, X4, X6:'Callable'),
      f(fork, equality, identity, X6, X7:'Callable'),
      f(apply, X7, X3:'Container', X8:'Container'),
      f(rbind, ofcolor, 'FOUR':'Any', X9:'Callable'),
      f(apply, X9, X2:'Container', X10:'Container'),
      f(apply, tojvec, X3:'Container', X11:'Container'),
      f(papply, shift, X10:'Tuple', X11:'Tuple', X12:'Tuple'),
      f(pair, X8:'Tuple', X12:'Tuple', X13:'TupleTuple'),
      f(sfilter, X13:'Container', first, X14:'Container'),
      f(mapply, last, X14:'ContainerContainer', X15:'FrozenSet'),
      f(fill, IN:'Grid', 'SIX':'Integer', X15:'Patch', OUT:'Grid')
    ].
l_solve('4612dd53', IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'ONE':'Integer', X1:'Indices'),
      f(box, X1:'Patch', X2:'Indices'),
      f(fill, IN:'Grid', 'TWO':'Integer', X2:'Patch', X3:'Grid'),
      f(subgrid, X1:'Patch', X3:'Grid', X4:'Grid'),
      f(ofcolor, X4:'Grid', 'ONE':'Integer', X5:'Indices'),
      f(mapply, vfrontier, X5:'ContainerContainer', X6:'FrozenSet'),
      f(mapply, hfrontier, X5:'ContainerContainer', X7:'FrozenSet'),
      f(size, X6:'Container', X8:'Integer'),
      f(size, X7:'Container', X9:'Integer'),
      f(greater, X8:'Integer', X9:'Integer', returns=X10),
      f(branch, condition=X10, X7:'Any', X6:'Any', X11:'Any'),
      f(fill, X4:'Grid', 'TWO':'Integer', X11:'Patch', X12:'Grid'),
      f(ofcolor, X12:'Grid', 'TWO':'Integer', X13:'Indices'),
      f(ulcorner, X1:'Patch', X14:'Point'),
      f(shift, X13:'Patch', X14:'Point', X15:'Patch'),
      f(underfill, IN:'Grid', 'TWO':'Integer', X15:'Patch', OUT:'Grid')
    ].
l_solve('29c11459', IN, OUT) :-
    [ f(lefthalf, IN:'Grid', X1:'Grid'),
      f(righthalf, IN:'Grid', X2:'Grid'),
      f(objects,
        X2:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X3:'Objects'),
      f(objects,
        X1:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X4:'Objects'),
      f(compose, hfrontier, center, X5:'Callable'),
      f(fork, recolor, color, X5, X6:'Callable'),
      f(mapply, X6, X4:'ContainerContainer', X7:'FrozenSet'),
      f(paint, X1:'Grid', X7:'Object', X8:'Grid'),
      f(mapply, X6, X3:'ContainerContainer', X9:'FrozenSet'),
      f(paint, IN:'Grid', X9:'Object', X10:'Grid'),
      f(objects,
        X8:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X11:'Objects'),
      f(apply, urcorner, X11:'Container', X12:'Container'),
      f(shift, X12:'Patch', (0, 1):'Point', X13:'Patch'),
      f(merge, X11:'ContainerContainer', X14:'Container'),
      f(paint, X10:'Grid', X14:'Object', X15:'Grid'),
      f(fill, X15:'Grid', 'FIVE':'Integer', X13:'Patch', OUT:'Grid')
    ].
l_solve('963e52fc', IN, OUT) :-
    [ f(width, IN:'Piece', X1:'Integer'),
      f(asobject, IN:'Grid', X2:'Object'),
      f(hperiod, X2:'Object', X3:'Integer'),
      f(height, X2:'Piece', X4:'Integer'),
      f(astuple, X4:'Integer', X3:'Integer', X5:'Point'),
      f(ulcorner, X2:'Patch', X6:'Point'),
      f(crop, IN:'Grid', X6:'Point', X5:'Point', X7:'Grid'),
      f(rot90, X7:'Grid', X8:'Grid'),
      f(double, X1:'Numerical', X9:'Numerical'),
      f(divide, X9:'Numerical', X3:'Numerical', X10:'Numerical'),
      f(increment, X10:'Numerical', X11:'Numerical'),
      f(repeat, X8:'Any', X11:'Integer', X12:'Tuple'),
      f(merge, X12:'ContainerContainer', X13:'Container'),
      f(rot270, X13:'Grid', X14:'Grid'),
      f(astuple, X4:'Integer', X9:'Integer', X15:'Point'),
      f(crop, X14:'Grid', (0, 0):'Point', X15:'Point', OUT:'Grid')
    ].
l_solve(ae3edfdc, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(replace, IN:'Grid', 'THREE':'Integer', 'ZERO':'Integer', X2:'Grid'),
      f(replace, X2:'Grid', 'SEVEN':'Integer', 'ZERO':'Integer', X3:'Grid'),
      f(lbind, colorfilter, X1:'Any', X4:'Callable'),
      f(lbind, rbind, gravitate:'Any', X5:'Callable'),
      f(chain, X5, first, X4, X6:'Callable'),
      f(X6, 'TWO', X7),
      f(X6, 'ONE', X8),
      f(X4, 'THREE', X9),
      f(X4, 'SEVEN', X10),
      f(fork, shift, identity, X7, X11:'Callable'),
      f(fork, shift, identity, X8, X12:'Callable'),
      f(mapply, X11, X9:'ContainerContainer', X13:'FrozenSet'),
      f(mapply, X12, X10:'ContainerContainer', X14:'FrozenSet'),
      f(paint, X3:'Grid', X13:'Object', X15:'Grid'),
      f(paint, X15:'Grid', X14:'Object', OUT:'Grid')
    ].
l_solve('1f0c79e5', IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'TWO':'Integer', X1:'Indices'),
      f(replace, IN:'Grid', 'TWO':'Integer', 'ZERO':'Integer', X2:'Grid'),
      f(leastcolor, X2:'Element', X3:'Integer'),
      f(ofcolor, X2:'Grid', X3:'Integer', X4:'Indices'),
      f(combine, X1:'Container', X4:'Container', X5:'Container'),
      f(recolor, X3:'Integer', X5:'Patch', X6:'Object'),
      f(compose, decrement, double, X7:'Callable'),
      f(ulcorner, X5:'Patch', X8:'Point'),
      f(invert, X8:'Numerical', X9:'Numerical'),
      f(shift, X1:'Patch', X9:'Point', X10:'Patch'),
      f(apply, X7, X10:'Container', X11:'Container'),
      f(interval, 'ZERO':'Integer', 'NINE':'Integer', 'ONE':'Integer', X12:'Tuple'),
      f(prapply, multiply, X11, X12, X13),
      f(lbind, shift, X6:'Any', X14:'Callable'),
      f(mapply, X14, X13:'ContainerContainer', X15:'FrozenSet'),
      f(paint, IN:'Grid', X15:'Object', OUT:'Grid')
    ].
l_solve('56dc2b01', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'THREE':'Integer', X2:'Objects'),
      f(first, X2:'Container', X3:'Any'),
      f(ofcolor, IN:'Grid', 'TWO':'Integer', X4:'Indices'),
      f(gravitate, X3:'Patch', X4:'Patch', X5:'Point'),
      f(first, X5:'Container', X6:'Any'),
      f(equality, X6:'Any', 'ZERO':'Any', returns=X7),
      f(branch, condition=X7, width:'Any', height:'Any', X8:'Any'),
      f(X8, X3, X9),
      f(gravitate, X4:'Patch', X3:'Patch', X10:'Point'),
      f(sign, X10:'Numerical', X11:'Numerical'),
      f(multiply, X11:'Numerical', X9:'Numerical', X12:'Numerical'),
      f(crement, X12:'Numerical', X13:'Numerical'),
      f(recolor, 'EIGHT':'Integer', X4:'Patch', X14:'Object'),
      f(shift, X14:'Patch', X13:'Point', X15:'Patch'),
      f(paint, IN:'Grid', X15:'Object', X16:'Grid'),
      f(move, X16:'Grid', X3:'Object', X5:'Point', OUT:'Grid')
    ].
l_solve(e48d4e1a, IN, OUT) :-
    [ f(shape, IN:'Piece', X1:'Point'),
      f(ofcolor, IN:'Grid', 'FIVE':'Integer', X2:'Indices'),
      f(fill, IN:'Grid', 'ZERO':'Integer', X2:'Patch', X3:'Grid'),
      f(leastcolor, X3:'Element', X4:'Integer'),
      f(size, X2:'Container', X5:'Integer'),
      f(ofcolor, IN:'Grid', X4:'Integer', X6:'Indices'),
      f(rbind, toobject, IN:'Any', X7:'Callable'),
      f(rbind, colorcount, X4:'Any', X8:'Callable'),
      f(chain, X8, X7, dneighbors, X9:'Callable'),
      f(matcher, X9, 'FOUR':'Any', X10:'Callable'),
      f(extract, X6:'Container', X10, X11:'Any'),
      f(multiply, (1, -1):'Numerical', X5:'Numerical', X12:'Numerical'),
      f(add, X12:'Numerical', X11:'Numerical', X13:'Numerical'),
      f(canvas, 'ZERO':'Integer', X1:'Point', X14:'Grid'),
      f(fork, combine, vfrontier, hfrontier, X15:'Callable'),
      f(X15, X13, X16),
      f(fill, X14:'Grid', X4:'Integer', X16:'Patch', OUT:'Grid')
    ].
l_solve('6773b310', IN, OUT) :-
    [ f(compress, IN:'Grid', X1:'Grid'),
      f(neighbors, (0, 0):'Point', X2:'Indices'),
      f(insert, (0, 0):'Any', X2:'FrozenSet', X3:'FrozenSet'),
      f(rbind, multiply, 'THREE':'Any', X4:'Callable'),
      f(apply, X4, X3:'Container', X5:'Container'),
      f(astuple, 'FOUR':'Integer', 'FOUR':'Integer', X6:'Point'),
      f(shift, X5:'Patch', X6:'Point', X7:'Patch'),
      f(fork, insert, identity, neighbors, X8:'Callable'),
      f(apply, X8, X7:'Container', X9:'Container'),
      f(rbind, toobject, X1:'Any', X10:'Callable'),
      f(apply, X10, X9:'Container', X11:'Container'),
      f(rbind, colorcount, 'SIX':'Any', X12:'Callable'),
      f(matcher, X12, 'TWO':'Any', X13:'Callable'),
      f(mfilter, X11:'Container', X13, X14:'FrozenSet'),
      f(fill, X1:'Grid', 'ONE':'Integer', X14:'Patch', X15:'Grid'),
      f(replace, X15:'Grid', 'SIX':'Integer', 'ZERO':'Integer', X16:'Grid'),
      f(downscale, X16:'Grid', 'THREE':'Integer', OUT:'Grid')
    ].
l_solve('780d0b14', IN, OUT) :-
    [ f(asindices, IN:'Grid', X1:'Indices'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X2:'Objects'),
      f(rbind, greater, 'TWO':'Any', X3:'Callable'),
      f(compose, X3, size, X4:'Callable'),
      f(sfilter, X2:'Container', X4, X5:'Container'),
      f(totuple, X5:'FrozenSet', X6:'Tuple'),
      f(apply, color, X6:'Container', X7:'Container'),
      f(apply, center, X6:'Container', X8:'Container'),
      f(pair, X7:'Tuple', X8:'Tuple', X9:'TupleTuple'),
      f(fill, IN:'Grid', 'ZERO':'Integer', X1:'Patch', X10:'Grid'),
      f(paint, X10:'Grid', X9:'Object', X11:'Grid'),
      f(rbind, greater, 'ONE':'Any', X12:'Callable'),
      f(compose, dedupe, totuple, X13:'Callable'),
      f(chain, X12, size, X13, X14:'Callable'),
      f(sfilter, X11:'Container', X14, X15:'Container'),
      f(rot90, X15:'Grid', X16:'Grid'),
      f(sfilter, X16:'Container', X14, X17:'Container'),
      f(rot270, X17:'Grid', OUT:'Grid')
    ].
l_solve('2204b7a8', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(lbind, sfilter, X1:'Any', X2:'Callable'),
      f(compose, size, X2, X3:'Callable'),
      f(X3, vline, X4),
      f(X3, hline, X5),
      f(greater, X4:'Integer', X5:'Integer', returns=X6),
      f(branch, condition=X6, lefthalf:'Any', tophalf:'Any', X7:'Any'),
      f(branch, condition=X6, righthalf:'Any', bottomhalf:'Any', X8:'Any'),
      f(branch, condition=X6, hconcat:'Any', vconcat:'Any', X9:'Any'),
      f(X7, IN, X10),
      f(X8, IN, X11),
      f(index, X10:'Grid', (0, 0):'Point', X12:'Integer'),
      f(shape, X11:'Piece', X13:'Point'),
      f(decrement, X13:'Numerical', X14:'Numerical'),
      f(index, X11:'Grid', X14:'Point', X15:'Integer'),
      f(replace, X10:'Grid', 'THREE':'Integer', X12:'Integer', X16:'Grid'),
      f(replace, X11:'Grid', 'THREE':'Integer', X15:'Integer', X17:'Grid'),
      f(X9, X16, X17, OUT)
    ].
l_solve(d9f24cd1, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'TWO':'Integer', X1:'Indices'),
      f(ofcolor, IN:'Grid', 'FIVE':'Integer', X2:'Indices'),
      f(prapply, connect, X1, X2, X3),
      f(mfilter, X3:'Container', vline, X4:'FrozenSet'),
      f(underfill, IN:'Grid', 'TWO':'Integer', X4:'Patch', X5:'Grid'),
      f(matcher, numcolors, 'TWO':'Any', X6:'Callable'),
      f(objects,
        X5:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X7:'Objects'),
      f(sfilter, X7:'Container', X6, X8:'Container'),
      f(difference, X7:'FrozenSet', X8:'FrozenSet', X9:'FrozenSet'),
      f(colorfilter, X9:'Objects', 'TWO':'Integer', X10:'Objects'),
      f(mapply, toindices, X10:'ContainerContainer', X11:'FrozenSet'),
      f(apply, urcorner, X8:'Container', X12:'Container'),
      f(shift, X12:'Patch', (1, 1):'Point', X13:'Patch'),
      f(rbind, shoot, (-1, 0):'Any', X14:'Callable'),
      f(mapply, X14, X13:'ContainerContainer', X15:'FrozenSet'),
      f(fill, X5:'Grid', 'TWO':'Integer', X15:'Patch', X16:'Grid'),
      f(mapply, vfrontier, X11:'ContainerContainer', X17:'FrozenSet'),
      f(fill, X16:'Grid', 'TWO':'Integer', X17:'Patch', OUT:'Grid')
    ].
l_solve(b782dc8a, IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X2:'Objects'),
      f(ofcolor, IN:'Grid', X1:'Integer', X3:'Indices'),
      f(first, X3:'Container', X4:'Any'),
      f(dneighbors, X4:'Point', X5:'Indices'),
      f(toobject, X5:'Patch', IN:'Grid', X6:'Object'),
      f(mostcolor, X6:'Element', X7:'Integer'),
      f(ofcolor, IN:'Grid', X7:'Integer', X8:'Indices'),
      f(colorfilter, X2:'Objects', 'ZERO':'Integer', X9:'Objects'),
      f(rbind, adjacent, X8:'Any', X10:'Callable'),
      f(mfilter, X9:'Container', X10, X11:'FrozenSet'),
      f(toindices, X11:'Patch', X12:'Indices'),
      f(rbind, manhattan, X3:'Any', X13:'Callable'),
      f(chain, even, X13, initset, X14:'Callable'),
      f(sfilter, X12:'Container', X14, X15:'Container'),
      f(difference,
        X12:'FrozenSet',
        X15:'FrozenSet',
        X16:'FrozenSet'),
      f(fill, IN:'Grid', X1:'Integer', X15:'Patch', X17:'Grid'),
      f(fill, X17:'Grid', X7:'Integer', X16:'Patch', OUT:'Grid')
    ].
l_solve('673ef223', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(ofcolor, IN:'Grid', 'EIGHT':'Integer', X2:'Indices'),
      f(replace, IN:'Grid', 'EIGHT':'Integer', 'FOUR':'Integer', X3:'Grid'),
      f(colorfilter, X1:'Objects', 'TWO':'Integer', X4:'Objects'),
      f(argmin, X1:'Container', uppermost, X5:'Any'),
      f(apply, uppermost, X4:'Container', X6:'Container'),
      f(fork, subtract, maximum, minimum, X7:'Callable'),
      f(X7, X6, X8),
      f(toivec, X8:'Integer', X9:'Point'),
      f(leftmost, X5:'Patch', X10:'Integer'),
      f(equality, X10:'Any', 'ZERO':'Any', returns=X11),
      f(branch, condition=X11, (0, -1):'Any', (0, 1):'Any', X12:'Any'),
      f(rbind, shoot, X12:'Any', X13:'Callable'),
      f(mapply, X13, X2:'ContainerContainer', X14:'FrozenSet'),
      f(underfill, X3:'Grid', 'EIGHT':'Integer', X14:'Patch', X15:'Grid'),
      f(shift, X2:'Patch', X9:'Point', X16:'Patch'),
      f(mapply, hfrontier, X16:'ContainerContainer', X17:'FrozenSet'),
      f(underfill, X15:'Grid', 'EIGHT':'Integer', X17:'Patch', OUT:'Grid')
    ].
l_solve(f5b8619d, IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(width, IN:'Piece', X2:'Integer'),
      f(height, IN:'Piece', X3:'Integer'),
      f(righthalf, IN:'Grid', X4:'Grid'),
      f(halve, X2:'Numerical', X5:'Numerical'),
      f(even, X2:'Integer', returns=X6),
      f(branch, condition=X6, identity:'Any', increment:'Any', X7:'Any'),
      f(X7, X5, X8),
      f(astuple, X3:'Integer', X8:'Integer', X9:'Point'),
      f(crop, IN:'Grid', (0, 0):'Point', X9:'Point', X10:'Grid'),
      f(vconcat, X10:'Grid', X10:'Grid', X11:'Grid'),
      f(vconcat, X4:'Grid', X4:'Grid', X12:'Grid'),
      f(hconcat, X12:'Grid', X11:'Grid', X13:'Grid'),
      f(hconcat, X11:'Grid', X13:'Grid', X14:'Grid'),
      f(hconcat, X14:'Grid', X12:'Grid', X15:'Grid'),
      f(ofcolor, X15:'Grid', X1:'Integer', X16:'Indices'),
      f(mapply, vfrontier, X16:'ContainerContainer', X17:'FrozenSet'),
      f(underfill, X15:'Grid', 'EIGHT':'Integer', X17:'Patch', OUT:'Grid')
    ].
l_solve(f8c80d96, IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X2:'Objects'),
      f(colorfilter, X2:'Objects', X1:'Integer', X3:'Objects'),
      f(argmax, X3:'Container', size, X4:'Any'),
      f(argmin, X2:'Container', width, X5:'Any'),
      f(size, X5:'Container', X6:'Integer'),
      f(equality, X6:'Any', 'ONE':'Any', returns=X7),
      f(branch, condition=X7, identity:'Any', outbox:'Any', X8:'Any'),
      f(chain, outbox, outbox, X8, X9:'Callable'),
      f(power, X9, 'TWO':'Integer', X10:'Callable'),
      f(power, X9, 'THREE':'Integer', X11:'Callable'),
      f(X9, X4, X12),
      f(X10, X4, X13),
      f(X11, X4, X14),
      f(fill, IN:'Grid', X1:'Integer', X12:'Patch', X15:'Grid'),
      f(fill, X15:'Grid', X1:'Integer', X13:'Patch', X16:'Grid'),
      f(fill, X16:'Grid', X1:'Integer', X14:'Patch', X17:'Grid'),
      f(replace, X17:'Grid', 'ZERO':'Integer', 'FIVE':'Integer', OUT:'Grid')
    ].
l_solve(ecdecbb3, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'TWO':'Integer', X2:'Objects'),
      f(colorfilter, X1:'Objects', 'EIGHT':'Integer', X3:'Objects'),
      f(product, X2:'Container', X3:'Container', X4:'FrozenSet'),
      f(fork, gravitate, first, last, X5:'Callable'),
      f(compose, crement, X5, X6:'Callable'),
      f(compose, center, first, X7:'Callable'),
      f(fork, add, X7, X6, X8:'Callable'),
      f(fork, connect, X7, X8, X9:'Callable'),
      f(apply, X9, X4:'Container', X10:'Container'),
      f(lbind, greater, 'EIGHT':'Any', X11:'Callable'),
      f(compose, X11, size, X12:'Callable'),
      f(mfilter, X10:'Container', X12, X13:'FrozenSet'),
      f(fill, IN:'Grid', 'TWO':'Integer', X13:'Patch', X14:'Grid'),
      f(apply, X8, X4:'Container', X15:'Container'),
      f(intersection,
        X13:'FrozenSet',
        X15:'FrozenSet',
        X16:'FrozenSet'),
      f(mapply, neighbors, X16:'ContainerContainer', X17:'FrozenSet'),
      f(fill, X14:'Grid', 'EIGHT':'Integer', X17:'Patch', OUT:'Grid')
    ].
l_solve(e5062a87, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'TWO':'Integer', X1:'Indices'),
      f(recolor, 'ZERO':'Integer', X1:'Patch', X2:'Object'),
      f(normalize, X2:'Patch', X3:'Patch'),
      f(occurrences, IN:'Grid', X2:'Object', X4:'Indices'),
      f(lbind, shift, X3:'Any', X5:'Callable'),
      f(apply, X5, X4:'Container', X6:'Container'),
      f(astuple, 'ONE':'Integer', 'THREE':'Integer', X7:'Point'),
      f(astuple, 'FIVE':'Integer', 'ONE':'Integer', X8:'Point'),
      f(astuple, 'TWO':'Integer', 'SIX':'Integer', X9:'Point'),
      f(initset, X7:'Any', X10:'FrozenSet'),
      f(insert, X8:'Any', X10:'FrozenSet', X11:'FrozenSet'),
      f(insert, X9:'Any', X11:'FrozenSet', X12:'FrozenSet'),
      f(rbind, contained, X12:'Any', X13:'Callable'),
      f(chain, flip, X13, ulcorner, X14:'Callable'),
      f(sfilter, X6:'Container', X14, X15:'Container'),
      f(merge, X15:'ContainerContainer', X16:'Container'),
      f(recolor, 'TWO':'Integer', X16:'Patch', X17:'Object'),
      f(paint, IN:'Grid', X17:'Object', OUT:'Grid')
    ].
l_solve(a8d7556c, IN, OUT) :-
    [ f(initset, (0, 0):'Any', X1:'FrozenSet'),
      f(recolor, 'ZERO':'Integer', X1:'Patch', X2:'Object'),
      f(upscale, X2:'Element', 'TWO':'Integer', X3:'Element'),
      f(occurrences, IN:'Grid', X3:'Object', X4:'Indices'),
      f(lbind, shift, X3:'Any', X5:'Callable'),
      f(mapply, X5, X4:'ContainerContainer', X6:'FrozenSet'),
      f(fill, IN:'Grid', 'TWO':'Integer', X6:'Patch', X7:'Grid'),
      f(add, 'SIX':'Numerical', 'SIX':'Numerical', X8:'Numerical'),
      f(astuple, 'EIGHT':'Integer', X8:'Integer', X9:'Point'),
      f(index, X7:'Grid', X9:'Point', X10:'Integer'),
      f(equality, X10:'Any', 'TWO':'Any', returns=X11),
      f(initset, X9:'Any', X12:'FrozenSet'),
      f(add, X9:'Numerical', (1, 0):'Numerical', X13:'Numerical'),
      f(insert, X13:'Any', X12:'FrozenSet', X14:'FrozenSet'),
      f(toobject, X14:'Patch', X7:'Grid', X15:'Object'),
      f(toobject, X14:'Patch', IN:'Grid', X16:'Object'),
      f(branch, condition=X11, X16:'Any', X15:'Any', X17:'Any'),
      f(paint, X7:'Grid', X17:'Object', OUT:'Grid')
    ].
l_solve('4938f0c2', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(ofcolor, IN:'Grid', 'TWO':'Integer', X2:'Indices'),
      f(vmirror, X2:'Piece', X3:'Piece'),
      f(height, X2:'Piece', X4:'Integer'),
      f(width, X2:'Piece', X5:'Integer'),
      f(toivec, X4:'Integer', X6:'Point'),
      f(tojvec, X5:'Integer', X7:'Point'),
      f(add, X7:'Numerical', (0, 2):'Numerical', X8:'Numerical'),
      f(add, X6:'Numerical', (2, 0):'Numerical', X9:'Numerical'),
      f(shift, X3:'Patch', X8:'Point', X10:'Patch'),
      f(fill, IN:'Grid', 'TWO':'Integer', X10:'Patch', X11:'Grid'),
      f(ofcolor, X11:'Grid', 'TWO':'Integer', X12:'Indices'),
      f(hmirror, X12:'Piece', X13:'Piece'),
      f(shift, X13:'Patch', X9:'Point', X14:'Patch'),
      f(fill, X11:'Grid', 'TWO':'Integer', X14:'Patch', X15:'Grid'),
      f(size, X1:'Container', X16:'Integer'),
      f(greater, X16:'Integer', 'FOUR':'Integer', returns=X17),
      f(branch, condition=X17, IN:'Any', X15:'Any', OUT:'Any')
    ].
l_solve('834ec97d', IN, OUT) :-
    [ f(asindices, IN:'Grid', X1:'Indices'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X2:'Objects'),
      f(first, X2:'Container', X3:'Any'),
      f(shift, X3:'Patch', (1, 0):'Point', X4:'Patch'),
      f(fill, IN:'Grid', 'ZERO':'Integer', X3:'Patch', X5:'Grid'),
      f(paint, X5:'Grid', X4:'Object', X6:'Grid'),
      f(uppermost, X4:'Patch', X7:'Integer'),
      f(leftmost, X4:'Patch', X8:'Integer'),
      f(subtract, X8:'Numerical', 'TEN':'Numerical', X9:'Numerical'),
      f(add, X8:'Numerical', 'TEN':'Numerical', X10:'Numerical'),
      f(interval,
        X9:'Integer',
        X10:'Integer',
        'TWO':'Integer',
        X11:'Tuple'),
      f(lbind, greater, X7:'Any', X12:'Callable'),
      f(compose, X12, first, X13:'Callable'),
      f(rbind, contained, X11:'Any', X14:'Callable'),
      f(compose, X14, last, X15:'Callable'),
      f(sfilter, X1:'Container', X13, X16:'Container'),
      f(sfilter, X16:'Container', X15, X17:'Container'),
      f(fill, X6:'Grid', 'FOUR':'Integer', X17:'Patch', OUT:'Grid')
    ].
l_solve('846bdb03', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(rbind, colorcount, 'FOUR':'Any', X2:'Callable'),
      f(matcher, X2, 'ZERO':'Any', X3:'Callable'),
      f(extract, X1:'Container', X3, X4:'Any'),
      f(remove, X4:'Any', X1:'Container', X5:'Container'),
      f(merge, X5:'ContainerContainer', X6:'Container'),
      f(subgrid, X6:'Patch', IN:'Grid', X7:'Grid'),
      f(index, X7:'Grid', (1, 0):'Point', X8:'Integer'),
      f(subgrid, X4:'Patch', IN:'Grid', X9:'Grid'),
      f(lefthalf, X9:'Grid', X10:'Grid'),
      f(palette, X10:'Element', X11:'IntegerSet'),
      f(other, X11:'Container', 'ZERO':'Any', X12:'Any'),
      f(equality, X8:'Any', X12:'Any', returns=X13),
      f(branch, condition=X13, identity:'Any', vmirror:'Any', X14:'Any'),
      f(X14, X4, X15),
      f(normalize, X15:'Patch', X16:'Patch'),
      f(shift, X16:'Patch', (1, 1):'Point', X17:'Patch'),
      f(paint, X7:'Grid', X17:'Object', OUT:'Grid')
    ].
l_solve('90f3ed37', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(order, X1:'Container', uppermost, X2:'Tuple'),
      f(first, X2:'Container', X3:'Any'),
      f(remove, X3:'Any', X2:'Container', X4:'Container'),
      f(normalize, X3:'Patch', X5:'Patch'),
      f(lbind, shift, X5:'Any', X6:'Callable'),
      f(compose, X6, ulcorner, X7:'Callable'),
      f(interval, 'TWO':'Integer', 'NEG_ONE':'Integer', 'NEG_ONE':'Integer', X8:'Tuple'),
      f(apply, tojvec, X8:'Container', X9:'Container'),
      f(rbind, apply, X9:'Any', X10:'Callable'),
      f(lbind, compose, size:'Any', X11:'Callable'),
      f(lbind, lbind, intersection:'Any', X12:'Callable'),
      f(compose, X11, X12, X13:'Callable'),
      f(lbind, lbind, shift:'Any', X14:'Callable'),
      f(chain, X10, X14, X7, X15:'Callable'),
      f(fork, argmax, X15, X13, X16:'Callable'),
      f(mapply, X16, X4:'ContainerContainer', X17:'FrozenSet'),
      f(underfill, IN:'Grid', 'ONE':'Integer', X17:'Patch', OUT:'Grid')
    ].
l_solve('8403a5d5', IN, OUT) :-
    [ f(asindices, IN:'Grid', X1:'Indices'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X2:'Objects'),
      f(first, X2:'Container', X3:'Any'),
      f(color, X3:'Object', X4:'Integer'),
      f(leftmost, X3:'Patch', X5:'Integer'),
      f(interval, X5:'Integer', 'TEN':'Integer', 'TWO':'Integer', X6:'Tuple'),
      f(rbind, contained, X6:'Any', X7:'Callable'),
      f(compose, X7, last, X8:'Callable'),
      f(sfilter, X1:'Container', X8, X9:'Container'),
      f(increment, X5:'Numerical', X10:'Numerical'),
      f(add, X5:'Numerical', 'THREE':'Numerical', X11:'Numerical'),
      f(interval, X10:'Integer', 'TEN':'Integer', 'FOUR':'Integer', X12:'Tuple'),
      f(interval, X11:'Integer', 'TEN':'Integer', 'FOUR':'Integer', X13:'Tuple'),
      f(lbind, astuple, 'NINE':'Any', X14:'Callable'),
      f(apply, tojvec, X12:'Container', X15:'Container'),
      f(apply, X14, X13:'Container', X16:'Container'),
      f(fill, IN:'Grid', X4:'Integer', X9:'Patch', X17:'Grid'),
      f(fill, X17:'Grid', 'FIVE':'Integer', X15:'Patch', X18:'Grid'),
      f(fill, X18:'Grid', 'FIVE':'Integer', X16:'Patch', OUT:'Grid')
    ].
l_solve(91413438, IN, OUT) :-
    [ f(colorcount, IN:'Element', 'ZERO':'Integer', X1:'Integer'),
      f(subtract, 'NINE':'Numerical', X1:'Numerical', X2:'Numerical'),
      f(multiply, X1:'Numerical', 'THREE':'Numerical', X3:'Numerical'),
      f(multiply, X3:'Numerical', X1:'Numerical', X4:'Numerical'),
      f(subtract, X4:'Numerical', 'THREE':'Numerical', X5:'Numerical'),
      f(astuple, 'THREE':'Integer', X5:'Integer', X6:'Point'),
      f(canvas, 'ZERO':'Integer', X6:'Point', X7:'Grid'),
      f(hconcat, IN:'Grid', X7:'Grid', X8:'Grid'),
      f(objects,
        X8:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X9:'Objects'),
      f(first, X9:'Container', X10:'Any'),
      f(lbind, shift, X10:'Any', X11:'Callable'),
      f(compose, X11, tojvec, X12:'Callable'),
      f(interval, 'ZERO':'Integer', X2:'Integer', 'ONE':'Integer', X13:'Tuple'),
      f(rbind, multiply, 'THREE':'Any', X14:'Callable'),
      f(apply, X14, X13:'Container', X15:'Container'),
      f(mapply, X12, X15:'ContainerContainer', X16:'FrozenSet'),
      f(paint, X8:'Grid', X16:'Object', X17:'Grid'),
      f(hsplit, X17:'Grid', X1:'Integer', X18:'Tuple'),
      f(merge, X18:'ContainerContainer', OUT:'Container')
    ].
l_solve('539a4f51', IN, OUT) :-
    [ f(shape, IN:'Piece', X1:'Point'),
      f(index, IN:'Grid', (0, 0):'Point', X2:'Integer'),
      f(colorcount, IN:'Element', 'ZERO':'Integer', X3:'Integer'),
      f(decrement, X1:'Numerical', X4:'Numerical'),
      f(positive, X3:'Integer', returns=X5),
      f(branch, condition=X5, X4:'Any', X1:'Any', X6:'Any'),
      f(crop, IN:'Grid', (0, 0):'Point', X6:'Point', X7:'Grid'),
      f(width, X7:'Piece', X8:'Integer'),
      f(astuple, 'ONE':'Integer', X8:'Integer', X9:'Point'),
      f(crop, X7:'Grid', (0, 0):'Point', X9:'Point', X10:'Grid'),
      f(vupscale, X10:'Grid', X8:'Integer', X11:'Grid'),
      f(dmirror, X11:'Piece', X12:'Piece'),
      f(hconcat, X7:'Grid', X11:'Grid', X13:'Grid'),
      f(hconcat, X12:'Grid', X7:'Grid', X14:'Grid'),
      f(vconcat, X13:'Grid', X14:'Grid', X15:'Grid'),
      f(asobject, X15:'Grid', X16:'Object'),
      f(multiply, (1, 1):'Numerical', 'TEN':'Numerical', X17:'Numerical'),
      f(canvas, X2:'Integer', X17:'Point', X18:'Grid'),
      f(paint, X18:'Grid', X16:'Object', OUT:'Grid')
    ].
l_solve('5daaa586', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'ZERO':'Integer', X2:'Objects'),
      f(rbind, bordering, IN:'Any', X3:'Callable'),
      f(compose, flip, X3, X4:'Callable'),
      f(extract, X2:'Container', X4, X5:'Any'),
      f(outbox, X5:'Patch', X6:'Indices'),
      f(subgrid, X6:'Patch', IN:'Grid', X7:'Grid'),
      f(fgpartition, X7:'Grid', X8:'Objects'),
      f(argmax, X8:'Container', size, X9:'Any'),
      f(color, X9:'Object', X10:'Integer'),
      f(toindices, X9:'Patch', X11:'Indices'),
      f(prapply, connect, X11, X11, X12),
      f(mfilter, X12:'Container', vline, X13:'FrozenSet'),
      f(mfilter, X12:'Container', hline, X14:'FrozenSet'),
      f(size, X13:'Container', X15:'Integer'),
      f(size, X14:'Container', X16:'Integer'),
      f(greater, X15:'Integer', X16:'Integer', returns=X17),
      f(branch, condition=X17, X13:'Any', X14:'Any', X18:'Any'),
      f(fill, X7:'Grid', X10:'Integer', X18:'Patch', OUT:'Grid')
    ].
l_solve('3bdb4ada', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(totuple, X1:'FrozenSet', X2:'Tuple'),
      f(compose, increment, ulcorner, X3:'Callable'),
      f(compose, decrement, lrcorner, X4:'Callable'),
      f(apply, X3, X2:'Container', X5:'Container'),
      f(apply, X4, X2:'Container', X6:'Container'),
      f(papply, connect, X5:'Tuple', X6:'Tuple', X7:'Tuple'),
      f(apply, last, X5:'Container', X8:'Container'),
      f(compose, last, first, X9:'Callable'),
      f(power, last, 'TWO':'Integer', X10:'Callable'),
      f(fork, subtract, X9, X10, X11:'Callable'),
      f(compose, even, X11, X12:'Callable'),
      f(lbind, rbind, astuple:'Any', X13:'Callable'),
      f(lbind, compose, X12:'Any', X14:'Callable'),
      f(compose, X14, X13, X15:'Callable'),
      f(fork, sfilter, first, X15, X16:'Callable'),
      f(pair, X7:'Tuple', X8:'Tuple', X17:'TupleTuple'),
      f(mapply, X16, X17:'ContainerContainer', X18:'FrozenSet'),
      f(fill, IN:'Grid', 'ZERO':'Integer', X18:'Patch', OUT:'Grid')
    ].
l_solve(ec883f72, IN, OUT) :-
    [ f(palette, IN:'Element', X1:'IntegerSet'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X2:'Objects'),
      f(fork, multiply, height, width, X3:'Callable'),
      f(argmax, X2:'Container', X3, X4:'Any'),
      f(color, X4:'Object', X5:'Integer'),
      f(remove, 'ZERO':'Any', X1:'Container', X6:'Container'),
      f(other, X6:'Container', X5:'Any', X7:'Any'),
      f(lrcorner, X4:'Patch', X8:'Point'),
      f(llcorner, X4:'Patch', X9:'Point'),
      f(urcorner, X4:'Patch', X10:'Point'),
      f(ulcorner, X4:'Patch', X11:'Point'),
      f(shoot, X8:'Point', (1, 1):'Point', X12:'Indices'),
      f(shoot, X9:'Point', (1, -1):'Point', X13:'Indices'),
      f(shoot, X10:'Point', (-1, 1):'Point', X14:'Indices'),
      f(shoot, X11:'Point', (-1, -1):'Point', X15:'Indices'),
      f(combine, X12:'Container', X13:'Container', X16:'Container'),
      f(combine, X14:'Container', X15:'Container', X17:'Container'),
      f(combine, X16:'Container', X17:'Container', X18:'Container'),
      f(underfill,
        IN:'Grid',
        X7:'Integer',
        X18:'Patch',
        OUT:'Grid')
    ].
l_solve('2bee17df', IN, OUT) :-
    [ f(height, IN:'Piece', X1:'Integer'),
      f(rot90, IN:'Grid', X2:'Grid'),
      f(subtract, X1:'Numerical', 'TWO':'Numerical', X3:'Numerical'),
      f(interval, 'ZERO':'Integer', X1:'Integer', 'ONE':'Integer', X4:'Tuple'),
      f(rbind, colorcount, 'ZERO':'Any', X5:'Callable'),
      f(matcher, X5, X3:'Any', X6:'Callable'),
      f(rbind, vsplit, X1:'Any', X7:'Callable'),
      f(lbind, apply, X6:'Any', X8:'Callable'),
      f(compose, X8, X7, X9:'Callable'),
      f(X9, IN, X10),
      f(pair, X4:'Tuple', X10:'Tuple', X11:'TupleTuple'),
      f(sfilter, X11:'Container', last, X12:'Container'),
      f(mapply, hfrontier, X12:'ContainerContainer', X13:'FrozenSet'),
      f(X9, X2, X14),
      f(pair, X14:'Tuple', X4:'Tuple', X15:'TupleTuple'),
      f(sfilter, X15:'Container', first, X16:'Container'),
      f(mapply, vfrontier, X16:'ContainerContainer', X17:'FrozenSet'),
      f(astuple, X13:'Integer', X17:'Integer', X18:'Point'),
      f(merge, X18:'ContainerContainer', X19:'Container'),
      f(underfill, IN:'Grid', 'THREE':'Integer', X19:'Patch', OUT:'Grid')
    ].
l_solve(e8dc4411, IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(ofcolor, IN:'Grid', 'ZERO':'Integer', X2:'Indices'),
      f(ofcolor, IN:'Grid', X1:'Integer', X3:'Indices'),
      f(position, X2:'Patch', X3:'Patch', X4:'Point'),
      f(fork, connect, ulcorner, lrcorner, X5:'Callable'),
      f(X5, X2, X6),
      f(intersection, X2:'FrozenSet', X6:'FrozenSet', X7:'FrozenSet'),
      f(equality, X6:'Any', X7:'Any', returns=X8),
      f(fork, subtract, identity, crement, X9:'Callable'),
      f(fork, add, identity, X9, X10:'Callable'),
      f(branch, condition=X8, identity:'Any', X10:'Any', X11:'Any'),
      f(shape, X2:'Piece', X12:'Point'),
      f(multiply, X12:'Numerical', X4:'Numerical', X13:'Numerical'),
      f(apply, X11, X13:'Container', X14:'Container'),
      f(interval, 'ONE':'Integer', 'FIVE':'Integer', 'ONE':'Integer', X15:'Tuple'),
      f(lbind, multiply, X14:'Any', X16:'Callable'),
      f(apply, X16, X15:'Container', X17:'Container'),
      f(lbind, shift, X2:'Any', X18:'Callable'),
      f(mapply, X18, X17:'ContainerContainer', X19:'FrozenSet'),
      f(fill, IN:'Grid', X1:'Integer', X19:'Patch', OUT:'Grid')
    ].
l_solve(e40b9e2f, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(neighbors, (0, 0):'Point', X2:'Indices'),
      f(mapply, neighbors, X2:'ContainerContainer', X3:'FrozenSet'),
      f(first, X1:'Container', X4:'Any'),
      f(lbind, intersection, X4:'Any', X5:'Callable'),
      f(compose, hmirror, vmirror, X6:'Callable'),
      f(X6, X4, X7),
      f(lbind, shift, X7:'Any', X8:'Callable'),
      f(apply, X8, X3:'Container', X9:'Container'),
      f(argmax, X9:'Container', X5, X10:'Any'),
      f(paint, IN:'Grid', X10:'Object', X11:'Grid'),
      f(objects,
        X11:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X12:'Objects'),
      f(first, X12:'Container', X13:'Any'),
      f(compose, size, X5, X14:'Callable'),
      f(compose, vmirror, dmirror, X15:'Callable'),
      f(X15, X13, X16),
      f(lbind, shift, X16:'Any', X17:'Callable'),
      f(apply, X17, X3:'Container', X18:'Container'),
      f(argmax, X18:'Container', X14, X19:'Any'),
      f(paint, X11:'Grid', X19:'Object', OUT:'Grid')
    ].
l_solve(29623171, IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(interval, 'ZERO':'Integer', 'NINE':'Integer', 'FOUR':'Integer', X2:'Tuple'),
      f(product, X2:'Container', X2:'Container', X3:'FrozenSet'),
      f(rbind, add, 'THREE':'Any', X4:'Callable'),
      f(rbind, interval, 'ONE':'Any', X5:'Callable'),
      f(fork, X5, identity, X4, X6:'Callable'),
      f(compose, X6, first, X7:'Callable'),
      f(compose, X6, last, X8:'Callable'),
      f(fork, product, X7, X8, X9:'Callable'),
      f(rbind, colorcount, X1:'Any', X10:'Callable'),
      f(rbind, toobject, IN:'Any', X11:'Callable'),
      f(compose, X10, X11, X12:'Callable'),
      f(apply, X9, X3:'Container', X13:'Container'),
      f(valmax, X13:'Container', X12, X14:'Integer'),
      f(matcher, X12, X14:'Any', X15:'Callable'),
      f(compose, flip, X15, X16:'Callable'),
      f(mfilter, X13:'Container', X15, X17:'FrozenSet'),
      f(mfilter, X13:'Container', X16, X18:'FrozenSet'),
      f(fill, IN:'Grid', X1:'Integer', X17:'Patch', X19:'Grid'),
      f(fill, X19:'Grid', 'ZERO':'Integer', X18:'Patch', OUT:'Grid')
    ].
l_solve(a2fd1cf0, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'TWO':'Integer', X1:'Indices'),
      f(ofcolor, IN:'Grid', 'THREE':'Integer', X2:'Indices'),
      f(uppermost, X1:'Patch', X3:'Integer'),
      f(leftmost, X1:'Patch', X4:'Integer'),
      f(uppermost, X2:'Patch', X5:'Integer'),
      f(leftmost, X2:'Patch', X6:'Integer'),
      f(astuple, X3:'Integer', X5:'Integer', X7:'Point'),
      f(minimum, X7:'IntegerSet', X8:'Integer'),
      f(maximum, X7:'IntegerSet', X9:'Integer'),
      f(astuple, X8:'Integer', X6:'Integer', X10:'Point'),
      f(astuple, X9:'Integer', X6:'Integer', X11:'Point'),
      f(connect, X10:'Point', X11:'Point', X12:'Indices'),
      f(astuple, X4:'Integer', X6:'Integer', X13:'Point'),
      f(minimum, X13:'IntegerSet', X14:'Integer'),
      f(maximum, X13:'IntegerSet', X15:'Integer'),
      f(astuple, X3:'Integer', X14:'Integer', X16:'Point'),
      f(astuple, X3:'Integer', X15:'Integer', X17:'Point'),
      f(connect, X16:'Point', X17:'Point', X18:'Indices'),
      f(combine, X12:'Container', X18:'Container', X19:'Container'),
      f(underfill, IN:'Grid', 'EIGHT':'Integer', X19:'Patch', OUT:'Grid')
    ].
l_solve(b0c4d837, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'FIVE':'Integer', X1:'Indices'),
      f(ofcolor, IN:'Grid', 'EIGHT':'Integer', X2:'Indices'),
      f(height, X1:'Piece', X3:'Integer'),
      f(decrement, X3:'Numerical', X4:'Numerical'),
      f(height, X2:'Piece', X5:'Integer'),
      f(subtract, X4:'Numerical', X5:'Numerical', X6:'Numerical'),
      f(astuple, 'ONE':'Integer', X6:'Integer', X7:'Point'),
      f(canvas, 'EIGHT':'Integer', X7:'Point', X8:'Grid'),
      f(subtract, 'SIX':'Numerical', X6:'Numerical', X9:'Numerical'),
      f(astuple, 'ONE':'Integer', X9:'Integer', X10:'Point'),
      f(canvas, 'ZERO':'Integer', X10:'Point', X11:'Grid'),
      f(hconcat, X8:'Grid', X11:'Grid', X12:'Grid'),
      f(hsplit, X12:'Grid', 'TWO':'Integer', X13:'Tuple'),
      f(first, X13:'Container', X14:'Any'),
      f(last, X13:'Container', X15:'Any'),
      f(vmirror, X15:'Piece', X16:'Piece'),
      f(vconcat, X14:'Grid', X16:'Grid', X17:'Grid'),
      f(astuple, 'ONE':'Integer', 'THREE':'Integer', X18:'Point'),
      f(canvas, 'ZERO':'Integer', X18:'Point', X19:'Grid'),
      f(vconcat, X17:'Grid', X19:'Grid', OUT:'Grid')
    ].
l_solve('8731374e', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(argmax, X1:'Container', size, X2:'Any'),
      f(subgrid, X2:'Patch', IN:'Grid', X3:'Grid'),
      f(height, X3:'Piece', X4:'Integer'),
      f(width, X3:'Piece', X5:'Integer'),
      f(vsplit, X3:'Grid', X4:'Integer', X6:'Tuple'),
      f(lbind, greater, 'FOUR':'Any', X7:'Callable'),
      f(compose, X7, numcolors, X8:'Callable'),
      f(sfilter, X6:'Container', X8, X9:'Container'),
      f(merge, X9:'ContainerContainer', X10:'Container'),
      f(rot90, X10:'Grid', X11:'Grid'),
      f(vsplit, X11:'Grid', X5:'Integer', X12:'Tuple'),
      f(sfilter, X12:'Container', X8, X13:'Container'),
      f(merge, X13:'ContainerContainer', X14:'Container'),
      f(rot270, X14:'Grid', X15:'Grid'),
      f(leastcolor, X15:'Element', X16:'Integer'),
      f(ofcolor, X15:'Grid', X16:'Integer', X17:'Indices'),
      f(fork, combine, vfrontier, hfrontier, X18:'Callable'),
      f(mapply, X18, X17:'ContainerContainer', X19:'FrozenSet'),
      f(fill, X15:'Grid', X16:'Integer', X19:'Patch', OUT:'Grid')
    ].
l_solve('272f95fa', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'ZERO':'Integer', X2:'Objects'),
      f(apply, toindices, X2:'Container', X3:'Container'),
      f(rbind, bordering, IN:'Any', X4:'Callable'),
      f(compose, flip, X4, X5:'Callable'),
      f(extract, X3:'Container', X5, X6:'Any'),
      f(remove, X6:'Any', X3:'Container', X7:'Container'),
      f(lbind, vmatching, X6:'Any', X8:'Callable'),
      f(lbind, hmatching, X6:'Any', X9:'Callable'),
      f(sfilter, X7:'Container', X8, X10:'Container'),
      f(sfilter, X7:'Container', X9, X11:'Container'),
      f(argmin, X10:'Container', uppermost, X12:'Any'),
      f(argmax, X10:'Container', uppermost, X13:'Any'),
      f(argmin, X11:'Container', leftmost, X14:'Any'),
      f(argmax, X11:'Container', leftmost, X15:'Any'),
      f(fill, IN:'Grid', 'SIX':'Integer', X6:'Patch', X16:'Grid'),
      f(fill, X16:'Grid', 'TWO':'Integer', X12:'Patch', X17:'Grid'),
      f(fill, X17:'Grid', 'ONE':'Integer', X13:'Patch', X18:'Grid'),
      f(fill, X18:'Grid', 'FOUR':'Integer', X14:'Patch', X19:'Grid'),
      f(fill, X19:'Grid', 'THREE':'Integer', X15:'Patch', OUT:'Grid')
    ].
l_solve(db93a21d, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(ofcolor, IN:'Grid', 'NINE':'Integer', X2:'Indices'),
      f(colorfilter, X1:'Objects', 'NINE':'Integer', X3:'Objects'),
      f(rbind, shoot, (1, 0):'Any', X4:'Callable'),
      f(mapply, X4, X2:'ContainerContainer', X5:'FrozenSet'),
      f(underfill, IN:'Grid', 'ONE':'Integer', X5:'Patch', X6:'Grid'),
      f(compose, halve, width, X7:'Callable'),
      f(rbind, greater, 'ONE':'Any', X8:'Callable'),
      f(compose, X8, X7, X9:'Callable'),
      f(matcher, X7, 'THREE':'Any', X10:'Callable'),
      f(power, outbox, 'TWO':'Integer', X11:'Callable'),
      f(power, outbox, 'THREE':'Integer', X12:'Callable'),
      f(mapply, outbox, X3:'ContainerContainer', X13:'FrozenSet'),
      f(sfilter, X3:'Container', X9, X14:'Container'),
      f(sfilter, X3:'Container', X10, X15:'Container'),
      f(mapply, X11, X14:'ContainerContainer', X16:'FrozenSet'),
      f(mapply, X12, X15:'ContainerContainer', X17:'FrozenSet'),
      f(fill, X6:'Grid', 'THREE':'Integer', X13:'Patch', X18:'Grid'),
      f(fill, X18:'Grid', 'THREE':'Integer', X16:'Patch', X19:'Grid'),
      f(fill, X19:'Grid', 'THREE':'Integer', X17:'Patch', OUT:'Grid')
    ].
l_solve('53b68214', IN, OUT) :-
    [ f(width, IN:'Piece', X1:'Integer'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X2:'Objects'),
      f(first, X2:'Container', X3:'Any'),
      f(vperiod, X3:'Object', X4:'Integer'),
      f(toivec, X4:'Integer', X5:'Point'),
      f(interval, 'ZERO':'Integer', 'NINE':'Integer', 'ONE':'Integer', X6:'Tuple'),
      f(lbind, multiply, X5:'Any', X7:'Callable'),
      f(apply, X7, X6:'Container', X8:'Container'),
      f(lbind, shift, X3:'Any', X9:'Callable'),
      f(mapply, X9, X8:'ContainerContainer', X10:'FrozenSet'),
      f(astuple, X1:'Integer', X1:'Integer', X11:'Point'),
      f(portrait, X3:'Piece', returns=X12),
      f(shape, X3:'Piece', X13:'Point'),
      f(add, (1, 0):'Numerical', X13:'Numerical', X14:'Numerical'),
      f(decrement, X14:'Numerical', X15:'Numerical'),
      f(shift, X3:'Patch', X15:'Point', X16:'Patch'),
      f(branch, condition=X12, X10:'Any', X16:'Any', X17:'Any'),
      f(canvas, 'ZERO':'Integer', X11:'Point', X18:'Grid'),
      f(paint, X18:'Grid', X3:'Object', X19:'Grid'),
      f(paint, X19:'Grid', X17:'Object', OUT:'Grid')
    ].
l_solve(d6ad076f, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(argmin, X1:'Container', size, X2:'Any'),
      f(argmax, X1:'Container', size, X3:'Any'),
      f(vmatching, X2:'Patch', X3:'Patch', returns=X4),
      f(branch, condition=X4, (1, 0):'Any', (0, 1):'Any', X5:'Any'),
      f(branch, condition=X4, uppermost:'Any', leftmost:'Any', X6:'Any'),
      f(valmax, X1:'Container', X6, X7:'Integer'),
      f(X6, X2, X8),
      f(equality, X7:'Any', X8:'Any', returns=X9),
      f(branch, condition=X9, 'NEG_ONE':'Any', 'ONE':'Any', X10:'Any'),
      f(multiply, X5:'Numerical', X10:'Numerical', X11:'Numerical'),
      f(inbox, X2:'Patch', X12:'Indices'),
      f(rbind, shoot, X11:'Any', X13:'Callable'),
      f(mapply, X13, X12:'ContainerContainer', X14:'FrozenSet'),
      f(underfill, IN:'Grid', 'EIGHT':'Integer', X14:'Patch', X15:'Grid'),
      f(objects,
        X15:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X16:'Objects'),
      f(colorfilter, X16:'Objects', 'EIGHT':'Integer', X17:'Objects'),
      f(rbind, bordering, IN:'Any', X18:'Callable'),
      f(mfilter, X17:'Container', X18, X19:'FrozenSet'),
      f(cover, X15:'Grid', X19:'Patch', OUT:'Grid')
    ].
l_solve('6cdd2623', IN, OUT) :-
    [ f(leastcolor, IN:'Element', X1:'Integer'),
      f(height, IN:'Piece', X2:'Integer'),
      f(width, IN:'Piece', X3:'Integer'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X4:'Objects'),
      f(merge, X4:'ContainerContainer', X5:'Container'),
      f(cover, IN:'Grid', X5:'Patch', X6:'Grid'),
      f(ofcolor, IN:'Grid', X1:'Integer', X7:'Indices'),
      f(prapply, connect, X7, X7, X8),
      f(merge, X8:'ContainerContainer', X9:'Container'),
      f(decrement, X2:'Numerical', X10:'Numerical'),
      f(decrement, X3:'Numerical', X11:'Numerical'),
      f(lbind, greater, X10:'Any', X12:'Callable'),
      f(lbind, greater, X11:'Any', X13:'Callable'),
      f(fork, both, positive, X12, X14:'Callable'),
      f(compose, X14, first, X15:'Callable'),
      f(fork, both, positive, X13, X16:'Callable'),
      f(compose, X16, last, X17:'Callable'),
      f(fork, both, X15, X17, X18:'Callable'),
      f(sfilter, X9:'Container', X18, X19:'Container'),
      f(fill, X6:'Grid', X1:'Integer', X19:'Patch', X20:'Grid'),
      f(fill, X20:'Grid', X1:'Integer', X7:'Patch', OUT:'Grid')
    ].
l_solve(a3df8b1e, IN, OUT) :-
    [ f(shape, IN:'Piece', X1:'Point'),
      f(ofcolor, IN:'Grid', 'ONE':'Integer', X2:'Indices'),
      f(first, X2:'Container', X3:'Any'),
      f(shoot, X3:'Point', (-1, 1):'Point', X4:'Indices'),
      f(fill, IN:'Grid', 'ONE':'Integer', X4:'Patch', X5:'Grid'),
      f(ofcolor, X5:'Grid', 'ONE':'Integer', X6:'Indices'),
      f(urcorner, X6:'Patch', X7:'Point'),
      f(shoot, X7:'Point', (-1, -1):'Point', X8:'Indices'),
      f(fill, X5:'Grid', 'ONE':'Integer', X8:'Patch', X9:'Grid'),
      f(objects,
        X9:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X10:'Objects'),
      f(first, X10:'Container', X11:'Any'),
      f(subgrid, X11:'Patch', X9:'Grid', X12:'Grid'),
      f(shape, X12:'Piece', X13:'Point'),
      f(subtract, X13:'Numerical', (1, 0):'Numerical', X14:'Numerical'),
      f(crop, X12:'Grid', (1, 0):'Point', X14:'Point', X15:'Grid'),
      f(vconcat, X15:'Grid', X15:'Grid', X16:'Grid'),
      f(vconcat, X16:'Grid', X16:'Grid', X17:'Grid'),
      f(vconcat, X17:'Grid', X17:'Grid', X18:'Grid'),
      f(hmirror, X18:'Piece', X19:'Piece'),
      f(crop, X19:'Grid', (0, 0):'Point', X1:'Point', X20:'Grid'),
      f(hmirror, X20:'Piece', OUT:'Piece')
    ].
l_solve('8d510a79', IN, OUT) :-
    [ f(height, IN:'Piece', X1:'Integer'),
      f(halve, X1:'Numerical', X2:'Numerical'),
      f(ofcolor, IN:'Grid', 'ONE':'Integer', X3:'Indices'),
      f(ofcolor, IN:'Grid', 'TWO':'Integer', X4:'Indices'),
      f(ofcolor, IN:'Grid', 'FIVE':'Integer', X5:'Indices'),
      f(rbind, gravitate, X5:'Any', X6:'Callable'),
      f(compose, X6, initset, X7:'Callable'),
      f(fork, add, identity, X7, X8:'Callable'),
      f(fork, connect, identity, X8, X9:'Callable'),
      f(mapply, X9, X4:'ContainerContainer', X10:'FrozenSet'),
      f(fill, IN:'Grid', 'TWO':'Integer', X10:'Patch', X11:'Grid'),
      f(rbind, greater, X2:'Any', X12:'Callable'),
      f(compose, X12, first, X13:'Callable'),
      f(sfilter, X3:'Container', X13, X14:'Container'),
      f(difference, X3:'FrozenSet', X14:'FrozenSet', X15:'FrozenSet'),
      f(rbind, shoot, (-1, 0):'Any', X16:'Callable'),
      f(rbind, shoot, (1, 0):'Any', X17:'Callable'),
      f(mapply, X16, X15:'ContainerContainer', X18:'FrozenSet'),
      f(mapply, X17, X14:'ContainerContainer', X19:'FrozenSet'),
      f(combine, X18:'Container', X19:'Container', X20:'Container'),
      f(fill, X11:'Grid', 'ONE':'Integer', X20:'Patch', OUT:'Grid')
    ].
l_solve(cdecee7f, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(astuple, 'ONE':'Integer', 'THREE':'Integer', X2:'Point'),
      f(size, X1:'Container', X3:'Integer'),
      f(order, X1:'Container', leftmost, X4:'Tuple'),
      f(apply, color, X4:'Container', X5:'Container'),
      f(rbind, canvas, (1, 1):'Any', X6:'Callable'),
      f(apply, X6, X5:'Container', X7:'Container'),
      f(merge, X7:'ContainerContainer', X8:'Container'),
      f(dmirror, X8:'Piece', X9:'Piece'),
      f(subtract, 'NINE':'Numerical', X3:'Numerical', X10:'Numerical'),
      f(astuple, 'ONE':'Integer', X10:'Integer', X11:'Point'),
      f(canvas, 'ZERO':'Integer', X11:'Point', X12:'Grid'),
      f(hconcat, X9:'Grid', X12:'Grid', X13:'Grid'),
      f(hsplit, X13:'Grid', 'THREE':'Integer', X14:'Tuple'),
      f(merge, X14:'ContainerContainer', X15:'Container'),
      f(crop, X15:'Grid', (0, 0):'Point', X2:'Point', X16:'Grid'),
      f(crop, X15:'Grid', (1, 0):'Point', X2:'Point', X17:'Grid'),
      f(crop, X15:'Grid', (2, 0):'Point', X2:'Point', X18:'Grid'),
      f(vmirror, X17:'Piece', X19:'Piece'),
      f(vconcat, X16:'Grid', X19:'Grid', X20:'Grid'),
      f(vconcat, X20:'Grid', X18:'Grid', OUT:'Grid')
    ].
l_solve('3345333e', IN, OUT) :-
    [ f(mostcolor, IN:'Element', X1:'Integer'),
      f(asindices, IN:'Grid', X2:'Indices'),
      f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X3:'Objects'),
      f(first, X3:'Container', X4:'Any'),
      f(mostcolor, X4:'Element', X5:'Integer'),
      f(matcher, first, X5:'Any', X6:'Callable'),
      f(sfilter, X4:'Container', X6, X7:'Container'),
      f(toindices, X7:'Patch', X8:'Indices'),
      f(ulcorner, X7:'Patch', X9:'Point'),
      f(difference, X2:'FrozenSet', X8:'FrozenSet', X10:'FrozenSet'),
      f(fill, IN:'Grid', X1:'Integer', X10:'Patch', X11:'Grid'),
      f(subgrid, X7:'Patch', X11:'Grid', X12:'Grid'),
      f(vmirror, X12:'Piece', X13:'Piece'),
      f(ofcolor, X13:'Grid', X5:'Integer', X14:'Indices'),
      f(last, X9:'Container', X15:'Any'),
      f(even, X15:'Integer', returns=X16),
      f(invert, X16:'Numerical', X17:'Numerical'),
      f(tojvec, X17:'Integer', X18:'Point'),
      f(add, X9:'Numerical', X18:'Numerical', X19:'Numerical'),
      f(shift, X14:'Patch', X19:'Point', X20:'Patch'),
      f(fill, X11:'Grid', X5:'Integer', X20:'Patch', OUT:'Grid')
    ].
l_solve(b190f7f5, IN, OUT) :-
    [ f(dmirror, IN:'Piece', X1:'Piece'),
      f(portrait, IN:'Piece', returns=X2),
      f(branch, condition=X2, X1:'Any', IN:'Any', X3:'Any'),
      f(lefthalf, X3:'Grid', X4:'Grid'),
      f(righthalf, X3:'Grid', X5:'Grid'),
      f(palette, X4:'Element', X6:'IntegerSet'),
      f(contained, 'EIGHT':'Any', X6:'Container', returns=X7),
      f(branch, condition=X7, X4:'Any', X5:'Any', X8:'Any'),
      f(branch, condition=X7, X5:'Any', X4:'Any', X9:'Any'),
      f(width, X9:'Piece', X10:'Integer'),
      f(upscale, X9:'Element', X10:'Integer', X11:'Element'),
      f(repeat, X8:'Any', X10:'Integer', X12:'Tuple'),
      f(merge, X12:'ContainerContainer', X13:'Container'),
      f(dmirror, X13:'Piece', X14:'Piece'),
      f(repeat, X14:'Any', X10:'Integer', X15:'Tuple'),
      f(merge, X15:'ContainerContainer', X16:'Container'),
      f(dmirror, X16:'Piece', X17:'Piece'),
      f(ofcolor, X17:'Grid', 'ZERO':'Integer', X18:'Indices'),
      f(fill, X11:'Grid', 'ZERO':'Integer', X18:'Patch', X19:'Grid'),
      f(dmirror, X19:'Piece', X20:'Piece'),
      f(branch, condition=X2, X20:'Any', X19:'Any', OUT:'Any')
    ].
l_solve(caa06a1f, IN, OUT) :-
    [ f(asobject, IN:'Grid', X1:'Object'),
      f(shape, IN:'Piece', X2:'Point'),
      f(decrement, X2:'Numerical', X3:'Numerical'),
      f(index, IN:'Grid', X3:'Point', X4:'Integer'),
      f(double, X2:'Numerical', X5:'Numerical'),
      f(canvas, X4:'Integer', X5:'Point', X6:'Grid'),
      f(paint, X6:'Grid', X1:'Object', X7:'Grid'),
      f(objects,
        X7:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X8:'Objects'),
      f(first, X8:'Container', X9:'Any'),
      f(shift, X9:'Patch', (0, -1):'Point', X10:'Patch'),
      f(vperiod, X10:'Object', X11:'Integer'),
      f(hperiod, X10:'Object', X12:'Integer'),
      f(neighbors, (0, 0):'Point', X13:'Indices'),
      f(lbind, mapply, neighbors:'Any', X14:'Callable'),
      f(power, X14, 'TWO':'Integer', X15:'Callable'),
      f(X15, X13, X16),
      f(astuple, X11:'Integer', X12:'Integer', X17:'Point'),
      f(lbind, multiply, X17:'Any', X18:'Callable'),
      f(apply, X18, X16:'Container', X19:'Container'),
      f(lbind, shift, X10:'Any', X20:'Callable'),
      f(mapply, X20, X19:'ContainerContainer', X21:'FrozenSet'),
      f(paint, IN:'Grid', X21:'Object', OUT:'Grid')
    ].
l_solve(e21d9049, IN, OUT) :-
    [ f(asindices, IN:'Grid', X1:'Indices'),
      f(leastcolor, IN:'Element', X2:'Integer'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X3:'Objects'),
      f(ofcolor, IN:'Grid', X2:'Integer', X4:'Indices'),
      f(merge, X3:'ContainerContainer', X5:'Container'),
      f(shape, X5:'Piece', X6:'Point'),
      f(neighbors, (0, 0):'Point', X7:'Indices'),
      f(lbind, mapply, neighbors:'Any', X8:'Callable'),
      f(power, X8, 'TWO':'Integer', X9:'Callable'),
      f(X9, X7, X10),
      f(lbind, multiply, X6:'Any', X11:'Callable'),
      f(lbind, shift, X5:'Any', X12:'Callable'),
      f(apply, X11, X10:'Container', X13:'Container'),
      f(mapply, X12, X13:'ContainerContainer', X14:'FrozenSet'),
      f(lbind, hmatching, X4:'Any', X15:'Callable'),
      f(lbind, vmatching, X4:'Any', X16:'Callable'),
      f(fork, either, X15, X16, X17:'Callable'),
      f(compose, X17, initset, X18:'Callable'),
      f(paint, IN:'Grid', X14:'Object', X19:'Grid'),
      f(sfilter, X1:'Container', X18, X20:'Container'),
      f(difference, X1:'FrozenSet', X20:'FrozenSet', X21:'FrozenSet'),
      f(cover, X19:'Grid', X21:'Patch', OUT:'Grid')
    ].
l_solve(d89b689b, IN, OUT) :-
    [ f(asindices, IN:'Grid', X1:'Indices'),
      f(ofcolor, IN:'Grid', 'EIGHT':'Integer', X2:'Indices'),
      f(replace, IN:'Grid', 'EIGHT':'Integer', 'ZERO':'Integer', X3:'Grid'),
      f(vsplit, X3:'Grid', 'TWO':'Integer', X4:'Tuple'),
      f(rbind, order, leftmost:'Any', X5:'Callable'),
      f(lbind, apply, color:'Any', X6:'Callable'),
      f(rbind, repeat, 'ONE':'Any', X7:'Callable'),
      f(chain, X7, X6, X5, X8:'Callable'),
      f(matcher, first, 'ZERO':'Any', X9:'Callable'),
      f(compose, flip, X9, X10:'Callable'),
      f(rbind, sfilter, X10:'Any', X11:'Callable'),
      f(lbind, apply, initset:'Any', X12:'Callable'),
      f(chain, X12, X11, asobject, X13:'Callable'),
      f(compose, X8, X13, X14:'Callable'),
      f(apply, X14, X4:'Container', X15:'Container'),
      f(merge, X15:'ContainerContainer', X16:'Container'),
      f(ulcorner, X2:'Patch', X17:'Point'),
      f(asindices, X16:'Grid', X18:'Indices'),
      f(toobject, X18:'Patch', X16:'Grid', X19:'Object'),
      f(shift, X19:'Patch', X17:'Point', X20:'Patch'),
      f(cover, IN:'Grid', X1:'Patch', X21:'Grid'),
      f(paint, X21:'Grid', X20:'Object', OUT:'Grid')
    ].
l_solve('746b3537', IN, OUT) :-
    [ f(rot90, IN:'Grid', X1:'Grid'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X2:'Objects'),
      f(sfilter, X2:'Container', vline, X3:'Container'),
      f(compose, positive, size, X4:'Callable'),
      f(X4, X3, X5),
      f(branch, condition=X5, X1:'Any', IN:'Any', X6:'Any'),
      f(height, X6:'Piece', X7:'Integer'),
      f(astuple, X7:'Integer', 'ONE':'Integer', X8:'Point'),
      f(crop, X6:'Grid', (0, 0):'Point', X8:'Point', X9:'Grid'),
      f(objects,
        X9:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X10:'Objects'),
      f(asindices, X9:'Grid', X11:'Indices'),
      f(apply, center, X10:'Container', X12:'Container'),
      f(difference,
        X11:'FrozenSet',
        X12:'FrozenSet',
        X13:'FrozenSet'),
      f(fill, X9:'Grid', 'ZERO':'Integer', X13:'Patch', X14:'Grid'),
      f(vsplit, X14:'Grid', X7:'Integer', X15:'Tuple'),
      f(canvas, 'ZERO':'Integer', (1, 1):'Point', X16:'Grid'),
      f(rbind, equality, X16:'Any', X17:'Callable'),
      f(compose, flip, X17, X18:'Callable'),
      f(sfilter, X15:'Container', X18, X19:'Container'),
      f(merge, X19:'ContainerContainer', X20:'Container'),
      f(dmirror, X20:'Piece', X21:'Piece'),
      f(branch, condition=X5, X21:'Any', X20:'Any', OUT:'Any')
    ].
l_solve(63613498, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(crop, IN:'Grid', (0, 0):'Point', (3, 3):'Point', X2:'Grid'),
      f(partition, X2:'Grid', X3:'Objects'),
      f(colorfilter, X3:'Objects', 'ZERO':'Integer', X4:'Objects'),
      f(difference, X3:'FrozenSet', X4:'FrozenSet', X5:'FrozenSet'),
      f(first, X5:'Container', X6:'Any'),
      f(toindices, X6:'Patch', X7:'Indices'),
      f(ulcorner, X7:'Patch', X8:'Point'),
      f(invert, X8:'Numerical', X9:'Numerical'),
      f(shift, X7:'Patch', X9:'Point', X10:'Patch'),
      f(totuple, X1:'FrozenSet', X11:'Tuple'),
      f(apply, toindices, X11:'Container', X12:'Container'),
      f(apply, normalize, X12:'Container', X13:'Container'),
      f(pair, X11:'Tuple', X13:'Tuple', X14:'TupleTuple'),
      f(matcher, last, X10:'Any', X15:'Callable'),
      f(sfilter, X14:'Container', X15, X16:'Container'),
      f(matcher, first, X6:'Any', X17:'Callable'),
      f(compose, flip, X17, X18:'Callable'),
      f(extract, X16:'Container', X18, X19:'Any'),
      f(first, X19:'Container', X20:'Any'),
      f(recolor, 'FIVE':'Integer', X20:'Patch', X21:'Object'),
      f(paint, IN:'Grid', X21:'Object', OUT:'Grid')
    ].
l_solve('06df4c85', IN, OUT) :-
    [ f(partition, IN:'Grid', X1:'Objects'),
      f(mostcolor, IN:'Element', X2:'Integer'),
      f(ofcolor, IN:'Grid', X2:'Integer', X3:'Indices'),
      f(colorfilter, X1:'Objects', 'ZERO':'Integer', X4:'Objects'),
      f(argmax, X1:'Container', size, X5:'Any'),
      f(difference, X1:'FrozenSet', X4:'FrozenSet', X6:'FrozenSet'),
      f(remove, X5:'Any', X6:'Container', X7:'Container'),
      f(merge, X7:'ContainerContainer', X8:'Container'),
      f(product, X8:'Container', X8:'Container', X9:'FrozenSet'),
      f(power, first, 'TWO':'Integer', X10:'Callable'),
      f(compose, first, last, X11:'Callable'),
      f(fork, equality, X10, X11, X12:'Callable'),
      f(sfilter, X9:'Container', X12, X13:'Container'),
      f(compose, last, first, X14:'Callable'),
      f(power, last, 'TWO':'Integer', X15:'Callable'),
      f(fork, connect, X14, X15, X16:'Callable'),
      f(fork, recolor, color, X16, X17:'Callable'),
      f(apply, X17, X13:'Container', X18:'Container'),
      f(fork, either, vline, hline, X19:'Callable'),
      f(mfilter, X18:'Container', X19, X20:'FrozenSet'),
      f(paint, IN:'Grid', X20:'Object', X21:'Grid'),
      f(fill, X21:'Grid', X2:'Integer', X3:'Patch', OUT:'Grid')
    ].
l_solve(f9012d9b, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(ofcolor, IN:'Grid', 'ZERO':'Integer', X2:'Indices'),
      f(lbind, contained, 'ZERO':'Any', X3:'Callable'),
      f(chain, flip, X3, palette, X4:'Callable'),
      f(mfilter, X1:'Container', X4, X5:'FrozenSet'),
      f(vsplit, IN:'Grid', 'TWO':'Integer', X6:'Tuple'),
      f(hsplit, IN:'Grid', 'TWO':'Integer', X7:'Tuple'),
      f(extract, X6:'Container', X4, X8:'Any'),
      f(extract, X7:'Container', X4, X9:'Any'),
      f(asobject, X8:'Grid', X10:'Object'),
      f(asobject, X9:'Grid', X11:'Object'),
      f(vperiod, X10:'Object', X12:'Integer'),
      f(hperiod, X11:'Object', X13:'Integer'),
      f(neighbors, (0, 0):'Point', X14:'Indices'),
      f(mapply, neighbors, X14:'ContainerContainer', X15:'FrozenSet'),
      f(astuple, X12:'Integer', X13:'Integer', X16:'Point'),
      f(rbind, multiply, X16:'Any', X17:'Callable'),
      f(apply, X17, X15:'Container', X18:'Container'),
      f(lbind, shift, X5:'Any', X19:'Callable'),
      f(mapply, X19, X18:'ContainerContainer', X20:'FrozenSet'),
      f(paint, IN:'Grid', X20:'Object', X21:'Grid'),
      f(subgrid, X2:'Patch', X21:'Grid', OUT:'Grid')
    ].
l_solve('4522001f', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(first, X1:'Container', X2:'Any'),
      f(toindices, X2:'Patch', X3:'Indices'),
      f(contained, (0, 2):'Any', X3:'Container', returns=X4),
      f(contained, (2, 2):'Any', X3:'Container', returns=X5),
      f(contained, (2, 0):'Any', X3:'Container', returns=X6),
      f(astuple, 'NINE':'Integer', 'NINE':'Integer', X7:'Point'),
      f(canvas, 'ZERO':'Integer', X7:'Point', X8:'Grid'),
      f(astuple, 'THREE':'Integer', (0, 0):'Integer', X9:'Point'),
      f(initset, X9:'Any', X10:'FrozenSet'),
      f(upscale, X10:'Element', 'TWO':'Integer', X11:'Element'),
      f(upscale, X11:'Element', 'TWO':'Integer', X12:'Element'),
      f(shape, X12:'Piece', X13:'Point'),
      f(shift, X12:'Patch', X13:'Point', X14:'Patch'),
      f(combine, X12:'Container', X14:'Container', X15:'Container'),
      f(paint, X8:'Grid', X15:'Object', X16:'Grid'),
      f(rot90, X16:'Grid', X17:'Grid'),
      f(rot180, X16:'Grid', X18:'Grid'),
      f(rot270, X16:'Grid', X19:'Grid'),
      f(branch, condition=X4, X17:'Any', X16:'Any', X20:'Any'),
      f(branch, condition=X5, X18:'Any', X20:'Any', X21:'Any'),
      f(branch, condition=X6, X19:'Any', X21:'Any', OUT:'Any')
    ].
l_solve(a48eeaf7, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(ofcolor, IN:'Grid', 'TWO':'Integer', X2:'Indices'),
      f(colorfilter, X1:'Objects', 'FIVE':'Integer', X3:'Objects'),
      f(rbind, gravitate, X2:'Any', X4:'Callable'),
      f(fork, shift, identity, X4, X5:'Callable'),
      f(mapply, X5, X3:'ContainerContainer', X6:'FrozenSet'),
      f(paint, IN:'Grid', X6:'Object', X7:'Grid'),
      f(rbind, vmatching, X2:'Any', X8:'Callable'),
      f(rbind, hmatching, X2:'Any', X9:'Callable'),
      f(fork, either, X8, X9, X10:'Callable'),
      f(sfilter, X3:'Container', X10, X11:'Container'),
      f(merge, X3:'ContainerContainer', X12:'Container'),
      f(cover, X7:'Grid', X12:'Patch', X13:'Grid'),
      f(difference, X3:'FrozenSet', X11:'FrozenSet', X14:'FrozenSet'),
      f(rbind, position, X2:'Any', X15:'Callable'),
      f(rbind, manhattan, X2:'Any', X16:'Callable'),
      f(compose, halve, X16, X17:'Callable'),
      f(fork, multiply, X17, X15, X18:'Callable'),
      f(fork, subtract, X18, X15, X19:'Callable'),
      f(fork, shift, identity, X19, X20:'Callable'),
      f(mapply, X20, X14:'ContainerContainer', X21:'FrozenSet'),
      f(paint, X13:'Grid', X21:'Object', OUT:'Grid')
    ].
l_solve(eb5a1d5d, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(argmin, X1:'Container', size, X2:'Any'),
      f(color, X2:'Object', X3:'Integer'),
      f(compose, invert, width, X4:'Callable'),
      f(order, X1:'Container', X4, X5:'Tuple'),
      f(apply, color, X5:'Container', X6:'Container'),
      f(size, X5:'Container', X7:'Integer'),
      f(double, X7:'Numerical', X8:'Numerical'),
      f(decrement, X8:'Numerical', X9:'Numerical'),
      f(interval, 'ZERO':'Integer', X7:'Integer', 'ONE':'Integer', X10:'Tuple'),
      f(pair, X10:'Tuple', X10:'Tuple', X11:'TupleTuple'),
      f(decrement, X9:'Numerical', X12:'Numerical'),
      f(interval,
        X12:'Integer',
        'ZERO':'Integer',
        'NEG_TWO':'Integer',
        X13:'Tuple'),
      f(papply, add, X13:'Tuple', X10:'Tuple', X14:'Tuple'),
      f(order, X14:'Container', invert, X15:'Tuple'),
      f(pair, X15:'Tuple', X15:'Tuple', X16:'TupleTuple'),
      f(pair, X11:'Tuple', X16:'Tuple', X17:'TupleTuple'),
      f(apply, box, X17:'Container', X18:'Container'),
      f(mpapply, recolor, X6:'Tuple', X18:'Tuple', X19:'Tuple'),
      f(astuple, X9:'Integer', X9:'Integer', X20:'Point'),
      f(canvas, 'ZERO':'Integer', X20:'Point', X21:'Grid'),
      f(paint, X21:'Grid', X19:'Object', X22:'Grid'),
      f(replace, X22:'Grid', 'ZERO':'Integer', X3:'Integer', OUT:'Grid')
    ].
l_solve(e179c5f4, IN, OUT) :-
    [ f(height, IN:'Piece', X1:'Integer'),
      f(ofcolor, IN:'Grid', 'ONE':'Integer', X2:'Indices'),
      f(first, X2:'Container', X3:'Any'),
      f(shoot, X3:'Point', (-1, 1):'Point', X4:'Indices'),
      f(fill, IN:'Grid', 'ONE':'Integer', X4:'Patch', X5:'Grid'),
      f(ofcolor, X5:'Grid', 'ONE':'Integer', X6:'Indices'),
      f(urcorner, X6:'Patch', X7:'Point'),
      f(shoot, X7:'Point', (-1, -1):'Point', X8:'Indices'),
      f(fill, X5:'Grid', 'ONE':'Integer', X8:'Patch', X9:'Grid'),
      f(ofcolor, X9:'Grid', 'ONE':'Integer', X10:'Indices'),
      f(subgrid, X10:'Patch', X9:'Grid', X11:'Grid'),
      f(height, X11:'Piece', X12:'Integer'),
      f(width, X11:'Piece', X13:'Integer'),
      f(decrement, X12:'Numerical', X14:'Numerical'),
      f(astuple, X14:'Integer', X13:'Integer', X15:'Point'),
      f(ulcorner, X10:'Patch', X16:'Point'),
      f(crop, X9:'Grid', X16:'Point', X15:'Point', X17:'Grid'),
      f(repeat, X17:'Any', 'NINE':'Integer', X18:'Tuple'),
      f(merge, X18:'ContainerContainer', X19:'Container'),
      f(astuple, X1:'Integer', X13:'Integer', X20:'Point'),
      f(crop, X19:'Grid', (0, 0):'Point', X20:'Point', X21:'Grid'),
      f(hmirror, X21:'Piece', X22:'Piece'),
      f(replace, X22:'Grid', 'ZERO':'Integer', 'EIGHT':'Integer', OUT:'Grid')
    ].
l_solve('228f6490', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'ZERO':'Integer', X2:'Objects'),
      f(rbind, bordering, IN:'Any', X3:'Callable'),
      f(compose, flip, X3, X4:'Callable'),
      f(sfilter, X2:'Container', X4, X5:'Container'),
      f(first, X5:'Container', X6:'Any'),
      f(last, X5:'Container', X7:'Any'),
      f(difference, X1:'FrozenSet', X2:'FrozenSet', X8:'FrozenSet'),
      f(compose, normalize, toindices, X9:'Callable'),
      f(X9, X6, X10),
      f(X9, X7, X11),
      f(matcher, X9, X10:'Any', X12:'Callable'),
      f(matcher, X9, X11:'Any', X13:'Callable'),
      f(extract, X8:'Container', X12, X14:'Any'),
      f(extract, X8:'Container', X13, X15:'Any'),
      f(ulcorner, X6:'Patch', X16:'Point'),
      f(ulcorner, X7:'Patch', X17:'Point'),
      f(ulcorner, X14:'Patch', X18:'Point'),
      f(ulcorner, X15:'Patch', X19:'Point'),
      f(subtract, X16:'Numerical', X18:'Numerical', X20:'Numerical'),
      f(subtract, X17:'Numerical', X19:'Numerical', X21:'Numerical'),
      f(move, IN:'Grid', X14:'Object', X20:'Point', X22:'Grid'),
      f(move, X22:'Grid', X15:'Object', X21:'Point', OUT:'Grid')
    ].
l_solve('995c5fa3', IN, OUT) :-
    [ f(hsplit, IN:'Grid', 'THREE':'Integer', X1:'Tuple'),
      f(astuple, 'TWO':'Integer', 'ONE':'Integer', X2:'Point'),
      f(rbind, ofcolor, 'ZERO':'Any', X3:'Callable'),
      f(compose, ulcorner, X3, X4:'Callable'),
      f(compose, size, X3, X5:'Callable'),
      f(matcher, X5, 'ZERO':'Any', X6:'Callable'),
      f(matcher, X4, (1, 1):'Any', X7:'Callable'),
      f(matcher, X4, (1, 0):'Any', X8:'Callable'),
      f(matcher, X4, X2:'Any', X9:'Callable'),
      f(rbind, multiply, 'THREE':'Any', X10:'Callable'),
      f(power, double, 'TWO':'Integer', X11:'Callable'),
      f(compose, double, X6, X12:'Callable'),
      f(chain, X11, double, X7, X13:'Callable'),
      f(compose, X10, X8, X14:'Callable'),
      f(compose, X11, X9, X15:'Callable'),
      f(fork, add, X12, X13, X16:'Callable'),
      f(fork, add, X14, X15, X17:'Callable'),
      f(fork, add, X16, X17, X18:'Callable'),
      f(rbind, canvas, (1, 1):'Any', X19:'Callable'),
      f(compose, X19, X18, X20:'Callable'),
      f(apply, X20, X1:'Container', X21:'Container'),
      f(merge, X21:'ContainerContainer', X22:'Container'),
      f(hupscale, X22:'Grid', 'THREE':'Integer', OUT:'Grid')
    ].
l_solve(d06dbe63, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'EIGHT':'Integer', X1:'Indices'),
      f(center, X1:'Patch', X2:'Point'),
      f(connect, (0, 0):'Point', (1, 0):'Point', X3:'Indices'),
      f(connect, (0, 0):'Point', (0, 2):'Point', X4:'Indices'),
      f(combine, X3:'Container', X4:'Container', X5:'Container'),
      f(subtract, X2:'Numerical', (2, 0):'Numerical', X6:'Numerical'),
      f(shift, X5:'Patch', X6:'Point', X7:'Patch'),
      f(astuple, 'NEG_TWO':'Integer', 'TWO':'Integer', X8:'Point'),
      f(interval, 'ZERO':'Integer', 'FIVE':'Integer', 'ONE':'Integer', X9:'Tuple'),
      f(lbind, multiply, X8:'Any', X10:'Callable'),
      f(apply, X10, X9:'Container', X11:'Container'),
      f(lbind, shift, X7:'Any', X12:'Callable'),
      f(mapply, X12, X11:'ContainerContainer', X13:'FrozenSet'),
      f(fill, IN:'Grid', 'FIVE':'Integer', X13:'Patch', X14:'Grid'),
      f(rot180, X14:'Grid', X15:'Grid'),
      f(ofcolor, X15:'Grid', 'EIGHT':'Integer', X16:'Indices'),
      f(center, X16:'Patch', X17:'Point'),
      f(subtract, X17:'Numerical', X6:'Numerical', X18:'Numerical'),
      f(shift, X13:'Patch', X18:'Point', X19:'Patch'),
      f(toivec, 'NEG_TWO':'Integer', X20:'Point'),
      f(shift, X19:'Patch', X20:'Point', X21:'Patch'),
      f(fill, X15:'Grid', 'FIVE':'Integer', X21:'Patch', X22:'Grid'),
      f(rot180, X22:'Grid', OUT:'Grid')
    ].
l_solve('36fdfd69', IN, OUT) :-
    [ f(upscale, IN:'Element', 'TWO':'Integer', X1:'Element'),
      f(ofcolor, X1:'Grid', 'TWO':'Integer', X2:'Indices'),
      f(mapply, neighbors, X2:'ContainerContainer', X3:'FrozenSet'),
      f(difference, X3:'FrozenSet', X2:'FrozenSet', X4:'FrozenSet'),
      f(fill, X1:'Grid', 'FOUR':'Integer', X4:'Patch', X5:'Grid'),
      f(objects,
        X5:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X6:'Objects'),
      f(colorfilter, X6:'Objects', 'FOUR':'Integer', X7:'Objects'),
      f(totuple, X7:'FrozenSet', X8:'Tuple'),
      f(rbind, subgrid, X5:'Any', X9:'Callable'),
      f(apply, X9, X8:'Container', X10:'Container'),
      f(apply, ulcorner, X8:'Container', X11:'Container'),
      f(rbind, ofcolor, 'TWO':'Any', X12:'Callable'),
      f(apply, X12, X10:'Container', X13:'Container'),
      f(papply, subgrid, X13:'Tuple', X10:'Tuple', X14:'Tuple'),
      f(rbind, downscale, 'TWO':'Any', X15:'Callable'),
      f(apply, X15, X14:'Container', X16:'Container'),
      f(apply, asindices, X16:'Container', X17:'Container'),
      f(apply, increment, X11:'Container', X18:'Container'),
      f(apply, halve, X18:'Container', X19:'Container'),
      f(mpapply, shift, X17:'Tuple', X19:'Tuple', X20:'Tuple'),
      f(fill, IN:'Grid', 'FOUR':'Integer', X20:'Patch', X21:'Grid'),
      f(ofcolor, IN:'Grid', 'TWO':'Integer', X22:'Indices'),
      f(fill, X21:'Grid', 'TWO':'Integer', X22:'Patch', OUT:'Grid')
    ].
l_solve('0a938d79', IN, OUT) :-
    [ f(portrait, IN:'Piece', returns=X1),
      f(branch, condition=X1, dmirror:'Any', identity:'Any', X2:'Any'),
      f(X2, IN, X3),
      f(objects,
        X3:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X4:'Objects'),
      f(argmin, X4:'Container', leftmost, X5:'Any'),
      f(argmax, X4:'Container', leftmost, X6:'Any'),
      f(color, X5:'Object', X7:'Integer'),
      f(color, X6:'Object', X8:'Integer'),
      f(leftmost, X5:'Patch', X9:'Integer'),
      f(leftmost, X6:'Patch', X10:'Integer'),
      f(subtract, X10:'Numerical', X9:'Numerical', X11:'Numerical'),
      f(double, X11:'Numerical', X12:'Numerical'),
      f(multiply, 'THREE':'Numerical', 'TEN':'Numerical', X13:'Numerical'),
      f(interval,
        X9:'Integer',
        X13:'Integer',
        X12:'Integer',
        X14:'Tuple'),
      f(interval,
        X10:'Integer',
        X13:'Integer',
        X12:'Integer',
        X15:'Tuple'),
      f(compose, vfrontier, tojvec, X16:'Callable'),
      f(mapply, X16, X14:'ContainerContainer', X17:'FrozenSet'),
      f(mapply, X16, X15:'ContainerContainer', X18:'FrozenSet'),
      f(recolor, X7:'Integer', X17:'Patch', X19:'Object'),
      f(recolor, X8:'Integer', X18:'Patch', X20:'Object'),
      f(combine, X19:'Container', X20:'Container', X21:'Container'),
      f(paint, X3:'Grid', X21:'Object', X22:'Grid'),
      f(X2, X22, OUT)
    ].
l_solve('045e512c', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(apply, size, X1:'Container', X2:'Container'),
      f(maximum, X2:'IntegerSet', X3:'Integer'),
      f(lbind, greater, X3:'Any', X4:'Callable'),
      f(compose, X4, size, X5:'Callable'),
      f(sfilter, X1:'Container', X5, X6:'Container'),
      f(difference, X1:'FrozenSet', X6:'FrozenSet', X7:'FrozenSet'),
      f(first, X7:'Container', X8:'Any'),
      f(interval, 'ONE':'Integer', 'FOUR':'Integer', 'ONE':'Integer', X9:'Tuple'),
      f(lbind, multiply, 'FOUR':'Any', X10:'Callable'),
      f(apply, X10, X9:'Container', X11:'Container'),
      f(product, X6:'Container', X11:'Container', X12:'FrozenSet'),
      f(totuple, X12:'FrozenSet', X13:'Tuple'),
      f(apply, first, X13:'Container', X14:'Container'),
      f(apply, last, X13:'Container', X15:'Container'),
      f(apply, color, X14:'Container', X16:'Container'),
      f(lbind, position, X8:'Any', X17:'Callable'),
      f(apply, X17, X14:'Container', X18:'Container'),
      f(papply, multiply, X18:'Tuple', X15:'Tuple', X19:'Tuple'),
      f(rbind, recolor, X8:'Any', X20:'Callable'),
      f(apply, X20, X16:'Container', X21:'Container'),
      f(mpapply, shift, X21:'Tuple', X19:'Tuple', X22:'Tuple'),
      f(paint, IN:'Grid', X22:'Object', X23:'Grid'),
      f(paint, X23:'Grid', X8:'Object', OUT:'Grid')
    ].
l_solve(82819916, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(index, IN:'Grid', (1, 0):'Point', X2:'Integer'),
      f(argmax, X1:'Container', size, X3:'Any'),
      f(remove, X3:'Any', X1:'Container', X4:'Container'),
      f(matcher, first, X2:'Any', X5:'Callable'),
      f(sfilter, X3:'Container', X5, X6:'Container'),
      f(difference, X3:'FrozenSet', X6:'FrozenSet', X7:'FrozenSet'),
      f(lbind, shift, X6:'Any', X8:'Callable'),
      f(lbind, shift, X7:'Any', X9:'Callable'),
      f(totuple, X4:'FrozenSet', X10:'Tuple'),
      f(apply, ulcorner, X10:'Container', X11:'Container'),
      f(apply, urcorner, X10:'Container', X12:'Container'),
      f(apply, first, X11:'Container', X13:'Container'),
      f(apply, decrement, X13:'Container', X14:'Container'),
      f(apply, toivec, X14:'Container', X15:'Container'),
      f(lbind, index, IN:'Any', X16:'Callable'),
      f(apply, X16, X11:'Container', X17:'Container'),
      f(apply, X16, X12:'Container', X18:'Container'),
      f(apply, X8, X15:'Container', X19:'Container'),
      f(apply, X9, X15:'Container', X20:'Container'),
      f(mpapply, recolor, X17:'Tuple', X19:'Tuple', X21:'Tuple'),
      f(mpapply, recolor, X18:'Tuple', X20:'Tuple', X22:'Tuple'),
      f(paint, IN:'Grid', X21:'Object', X23:'Grid'),
      f(paint, X23:'Grid', X22:'Object', OUT:'Grid')
    ].
l_solve('99fa7670', IN, OUT) :-
    [ f(shape, IN:'Piece', X1:'Point'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X2:'Objects'),
      f(rbind, shoot, (0, 1):'Any', X3:'Callable'),
      f(compose, X3, center, X4:'Callable'),
      f(fork, recolor, color, X4, X5:'Callable'),
      f(mapply, X5, X2:'ContainerContainer', X6:'FrozenSet'),
      f(paint, IN:'Grid', X6:'Object', X7:'Grid'),
      f(add, X1:'Numerical', (1, -1):'Numerical', X8:'Numerical'),
      f(initset, X8:'Any', X9:'FrozenSet'),
      f(recolor, 'ZERO':'Integer', X9:'Patch', X10:'Object'),
      f(objects,
        X7:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X11:'Objects'),
      f(insert, X10:'Any', X11:'FrozenSet', X12:'FrozenSet'),
      f(order, X12:'Container', uppermost, X13:'Tuple'),
      f(first, X13:'Container', X14:'Any'),
      f(remove, X10:'Any', X13:'Container', X15:'Container'),
      f(remove, X14:'Any', X13:'Container', X16:'Container'),
      f(compose, lrcorner, first, X17:'Callable'),
      f(compose, lrcorner, last, X18:'Callable'),
      f(fork, connect, X17, X18, X19:'Callable'),
      f(compose, color, first, X20:'Callable'),
      f(fork, recolor, X20, X19, X21:'Callable'),
      f(pair, X15:'Tuple', X16:'Tuple', X22:'TupleTuple'),
      f(mapply, X21, X22:'ContainerContainer', X23:'FrozenSet'),
      f(underpaint, X7:'Grid', X23:'Object', OUT:'Grid')
    ].
l_solve('72322fa7', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(matcher, numcolors, 'ONE':'Any', X2:'Callable'),
      f(sfilter, X1:'Container', X2, X3:'Container'),
      f(difference, X1:'FrozenSet', X3:'FrozenSet', X4:'FrozenSet'),
      f(lbind, matcher, first:'Any', X5:'Callable'),
      f(compose, X5, mostcolor, X6:'Callable'),
      f(fork, sfilter, identity, X6, X7:'Callable'),
      f(fork, difference, identity, X7, X8:'Callable'),
      f(lbind, occurrences, IN:'Any', X9:'Callable'),
      f(compose, X9, X7, X10:'Callable'),
      f(compose, X9, X8, X11:'Callable'),
      f(compose, ulcorner, X8, X12:'Callable'),
      f(fork, subtract, ulcorner, X12, X13:'Callable'),
      f(lbind, rbind, add:'Any', X14:'Callable'),
      f(compose, X14, X13, X15:'Callable'),
      f(fork, apply, X15, X11, X16:'Callable'),
      f(lbind, lbind, shift:'Any', X17:'Callable'),
      f(compose, X17, normalize, X18:'Callable'),
      f(fork, mapply, X18, X10, X19:'Callable'),
      f(fork, mapply, X18, X16, X20:'Callable'),
      f(mapply, X19, X4:'ContainerContainer', X21:'FrozenSet'),
      f(mapply, X20, X4:'ContainerContainer', X22:'FrozenSet'),
      f(paint, IN:'Grid', X21:'Object', X23:'Grid'),
      f(paint, X23:'Grid', X22:'Object', OUT:'Grid')
    ].
l_solve('855e0971', IN, OUT) :-
    [ f(rot90, IN:'Grid', X1:'Grid'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X2:'Objects'),
      f(sizefilter, X2:'Container', 'ONE':'Integer', X3:'FrozenSet'),
      f(difference, X2:'FrozenSet', X3:'FrozenSet', X4:'FrozenSet'),
      f(first, X4:'Container', X5:'Any'),
      f(portrait, X5:'Piece', returns=X6),
      f(branch, condition=X6, X1:'Any', IN:'Any', X7:'Any'),
      f(objects,
        X7:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X8:'Objects'),
      f(sizefilter, X8:'Container', 'ONE':'Integer', X9:'FrozenSet'),
      f(difference, X8:'FrozenSet', X9:'FrozenSet', X10:'FrozenSet'),
      f(rbind, subgrid, X7:'Any', X11:'Callable'),
      f(rbind, ofcolor, 'ZERO':'Any', X12:'Callable'),
      f(lbind, mapply, vfrontier:'Any', X13:'Callable'),
      f(chain, X13, X12, X11, X14:'Callable'),
      f(lbind, recolor, 'ZERO':'Any', X15:'Callable'),
      f(compose, X15, X14, X16:'Callable'),
      f(fork, paint, X11, X16, X17:'Callable'),
      f(fork, toobject, asindices, identity, X18:'Callable'),
      f(compose, X18, X17, X19:'Callable'),
      f(fork, shift, X19, ulcorner, X20:'Callable'),
      f(mapply, X20, X10:'ContainerContainer', X21:'FrozenSet'),
      f(paint, X7:'Grid', X21:'Object', X22:'Grid'),
      f(rot270, X22:'Grid', X23:'Grid'),
      f(branch, condition=X6, X23:'Any', X22:'Any', OUT:'Any')
    ].
l_solve(a78176bb, IN, OUT) :-
    [ f(palette, IN:'Element', X1:'IntegerSet'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X2:'Objects'),
      f(remove, 'ZERO':'Any', X1:'Container', X3:'Container'),
      f(other, X3:'Container', 'FIVE':'Any', X4:'Any'),
      f(colorfilter, X2:'Objects', 'FIVE':'Integer', X5:'Objects'),
      f(lbind, index, IN:'Any', X6:'Callable'),
      f(compose, X6, urcorner, X7:'Callable'),
      f(matcher, X7, 'FIVE':'Any', X8:'Callable'),
      f(sfilter, X5:'Container', X8, X9:'Container'),
      f(difference, X5:'FrozenSet', X9:'FrozenSet', X10:'FrozenSet'),
      f(apply, urcorner, X9:'Container', X11:'Container'),
      f(apply, llcorner, X10:'Container', X12:'Container'),
      f(rbind, add, (-1, 1):'Any', X13:'Callable'),
      f(rbind, add, (1, -1):'Any', X14:'Callable'),
      f(apply, X13, X11:'Container', X15:'Container'),
      f(apply, X14, X12:'Container', X16:'Container'),
      f(rbind, shoot, (1, 1):'Any', X17:'Callable'),
      f(rbind, shoot, (-1, -1):'Any', X18:'Callable'),
      f(fork, combine, X17, X18, X19:'Callable'),
      f(mapply, X19, X15:'ContainerContainer', X20:'FrozenSet'),
      f(mapply, X19, X16:'ContainerContainer', X21:'FrozenSet'),
      f(combine, X20:'Container', X21:'Container', X22:'Container'),
      f(fill, IN:'Grid', X4:'Integer', X22:'Patch', X23:'Grid'),
      f(replace, X23:'Grid', 'FIVE':'Integer', 'ZERO':'Integer', OUT:'Grid')
    ].
l_solve('952a094c', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(first, X1:'Container', X2:'Any'),
      f(inbox, X2:'Patch', X3:'Indices'),
      f(toobject, X3:'Patch', IN:'Grid', X4:'Object'),
      f(dmirror, X4:'Piece', X5:'Piece'),
      f(cmirror, X5:'Piece', X6:'Piece'),
      f(paint, IN:'Grid', X6:'Object', X7:'Grid'),
      f(lbind, index, X7:'Any', X8:'Callable'),
      f(fork, astuple, X8, identity, X9:'Callable'),
      f(compose, initset, X9, X10:'Callable'),
      f(double, (-1, -1):'Numerical', X11:'Numerical'),
      f(astuple, 'NEG_TWO':'Integer', 'TWO':'Integer', X12:'Point'),
      f(astuple, 'TWO':'Integer', 'NEG_TWO':'Integer', X13:'Point'),
      f(ulcorner, X3:'Patch', X14:'Point'),
      f(urcorner, X3:'Patch', X15:'Point'),
      f(llcorner, X3:'Patch', X16:'Point'),
      f(lrcorner, X3:'Patch', X17:'Point'),
      f(X10, X14, X18),
      f(move, X7:'Grid', X18:'Object', X11:'Point', X19:'Grid'),
      f(X10, X15, X20),
      f(move, X19:'Grid', X20:'Object', X12:'Point', X21:'Grid'),
      f(X10, X16, X22),
      f(move, X21:'Grid', X22:'Object', X13:'Point', X23:'Grid'),
      f(X10, X17, X24),
      f(move, X23:'Grid', X24:'Object', (2, 2):'Point', OUT:'Grid')
    ].
l_solve('6d58a25d', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(argmax, X1:'Container', size, X2:'Any'),
      f(remove, X2:'Any', X1:'Container', X3:'Container'),
      f(first, X3:'Container', X4:'Any'),
      f(color, X4:'Object', X5:'Integer'),
      f(leftmost, X2:'Patch', X6:'Integer'),
      f(rightmost, X2:'Patch', X7:'Integer'),
      f(center, X2:'Patch', X8:'Point'),
      f(first, X8:'Container', X9:'Any'),
      f(rbind, greater, X6:'Any', X10:'Callable'),
      f(compose, X10, leftmost, X11:'Callable'),
      f(lbind, greater, X7:'Any', X12:'Callable'),
      f(compose, X12, rightmost, X13:'Callable'),
      f(rbind, greater, X9:'Any', X14:'Callable'),
      f(compose, X14, lowermost, X15:'Callable'),
      f(fork, both, X11, X13, X16:'Callable'),
      f(fork, both, X16, X15, X17:'Callable'),
      f(sfilter, X3:'Container', X17, X18:'Container'),
      f(mapply, toindices, X18:'ContainerContainer', X19:'FrozenSet'),
      f(apply, last, X19:'Container', X20:'Container'),
      f(lbind, astuple, X9:'Any', X21:'Callable'),
      f(apply, X21, X20:'Container', X22:'Container'),
      f(rbind, shoot, (1, 0):'Any', X23:'Callable'),
      f(mapply, X23, X22:'ContainerContainer', X24:'FrozenSet'),
      f(underfill,
        IN:'Grid',
        X5:'Integer',
        X24:'Patch',
        OUT:'Grid')
    ].
l_solve('6aa20dc0', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(argmax, X1:'Container', numcolors, X2:'Any'),
      f(normalize, X2:'Patch', X3:'Patch'),
      f(lbind, matcher, first:'Any', X4:'Callable'),
      f(compose, X4, mostcolor, X5:'Callable'),
      f(fork, sfilter, identity, X5, X6:'Callable'),
      f(fork, difference, identity, X6, X7:'Callable'),
      f(lbind, rbind, upscale:'Any', X8:'Callable'),
      f(interval, 'ONE':'Integer', 'FOUR':'Integer', 'ONE':'Integer', X9:'Tuple'),
      f(apply, X8, X9:'Container', X10:'Container'),
      f(initset, identity:'Any', X11:'FrozenSet'),
      f(insert, vmirror:'Any', X11:'FrozenSet', X12:'FrozenSet'),
      f(insert, hmirror:'Any', X12:'FrozenSet', X13:'FrozenSet'),
      f(insert, cmirror:'Any', X13:'FrozenSet', X14:'FrozenSet'),
      f(insert, dmirror:'Any', X14:'FrozenSet', X15:'FrozenSet'),
      f(fork, compose, first, last, X16:'Callable'),
      f(lbind, occurrences, IN:'Any', X17:'Callable'),
      f(lbind, lbind, shift:'Any', X18:'Callable'),
      f(compose, X17, X7, X19:'Callable'),
      f(product, X15:'Container', X10:'Container', X20:'FrozenSet'),
      f(apply, X16, X20:'Container', X21:'Container'),
      f(rapply, X21:'Container', X3:'Any', X22:'Container'),
      f(fork, mapply, X18, X19, X23:'Callable'),
      f(mapply, X23, X22:'ContainerContainer', X24:'FrozenSet'),
      f(paint, IN:'Grid', X24:'Object', OUT:'Grid')
    ].
l_solve(e6721834, IN, OUT) :-
    [ f(portrait, IN:'Piece', returns=X1),
      f(branch, condition=X1, vsplit:'Any', hsplit:'Any', X2:'Any'),
      f(X2, IN, 'TWO', X3),
      f(order, X3:'Container', numcolors, X4:'Tuple'),
      f(first, X4:'Container', X5:'Any'),
      f(last, X4:'Container', X6:'Any'),
      f(objects,
        X6:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X7:'Objects'),
      f(merge, X7:'ContainerContainer', X8:'Container'),
      f(mostcolor, X8:'Element', X9:'Integer'),
      f(matcher, first, X9:'Any', X10:'Callable'),
      f(compose, flip, X10, X11:'Callable'),
      f(rbind, sfilter, X11:'Any', X12:'Callable'),
      f(lbind, occurrences, X5:'Any', X13:'Callable'),
      f(compose, X13, X12, X14:'Callable'),
      f(chain, positive, size, X14, X15:'Callable'),
      f(sfilter, X7:'Container', X15, X16:'Container'),
      f(chain, first, X13, X12, X17:'Callable'),
      f(compose, ulcorner, X12, X18:'Callable'),
      f(fork, subtract, X17, X18, X19:'Callable'),
      f(fork, shift, identity, X19, X20:'Callable'),
      f(apply, X20, X16:'Container', X21:'Container'),
      f(compose, decrement, width, X22:'Callable'),
      f(chain, positive, decrement, X22, X23:'Callable'),
      f(mfilter, X21:'Container', X23, X24:'FrozenSet'),
      f(paint, X5:'Grid', X24:'Object', OUT:'Grid')
    ].
l_solve('447fd412', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(argmax, X1:'Container', numcolors, X2:'Any'),
      f(normalize, X2:'Patch', X3:'Patch'),
      f(lbind, matcher, first:'Any', X4:'Callable'),
      f(compose, X4, mostcolor, X5:'Callable'),
      f(fork, sfilter, identity, X5, X6:'Callable'),
      f(fork, difference, identity, X6, X7:'Callable'),
      f(lbind, rbind, upscale:'Any', X8:'Callable'),
      f(interval, 'ONE':'Integer', 'FOUR':'Integer', 'ONE':'Integer', X9:'Tuple'),
      f(apply, X8, X9:'Container', X10:'Container'),
      f(lbind, recolor, 'ZERO':'Any', X11:'Callable'),
      f(compose, X11, outbox, X12:'Callable'),
      f(fork, combine, identity, X12, X13:'Callable'),
      f(lbind, occurrences, IN:'Any', X14:'Callable'),
      f(lbind, rbind, subtract:'Any', X15:'Callable'),
      f(lbind, apply, increment:'Any', X16:'Callable'),
      f(lbind, lbind, shift:'Any', X17:'Callable'),
      f(chain, X15, ulcorner, X7, X18:'Callable'),
      f(chain, X14, X13, X7, X19:'Callable'),
      f(fork, apply, X18, X19, X20:'Callable'),
      f(compose, X16, X20, X21:'Callable'),
      f(fork, mapply, X17, X21, X22:'Callable'),
      f(rapply, X10:'Container', X3:'Any', X23:'Container'),
      f(mapply, X22, X23:'ContainerContainer', X24:'FrozenSet'),
      f(paint, IN:'Grid', X24:'Object', OUT:'Grid')
    ].
l_solve('2bcee788', IN, OUT) :-
    [ f(mostcolor, IN:'Element', X1:'Integer'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X2:'Objects'),
      f(replace, IN:'Grid', X1:'Integer', 'THREE':'Integer', X3:'Grid'),
      f(argmax, X2:'Container', size, X4:'Any'),
      f(argmin, X2:'Container', size, X5:'Any'),
      f(position, X4:'Patch', X5:'Patch', X6:'Point'),
      f(first, X6:'Container', X7:'Any'),
      f(last, X6:'Container', X8:'Any'),
      f(subgrid, X4:'Patch', X3:'Grid', X9:'Grid'),
      f(hline, X5:'Patch', returns=X10),
      f(hmirror, X9:'Piece', X11:'Piece'),
      f(vmirror, X9:'Piece', X12:'Piece'),
      f(branch, condition=X10, X11:'Any', X12:'Any', X13:'Any'),
      f(branch, condition=X10, X7:'Any', 'ZERO':'Any', X14:'Any'),
      f(branch, condition=X10, 'ZERO':'Any', X8:'Any', X15:'Any'),
      f(asobject, X13:'Grid', X16:'Object'),
      f(matcher, first, 'THREE':'Any', X17:'Callable'),
      f(compose, flip, X17, X18:'Callable'),
      f(sfilter, X16:'Container', X18, X19:'Container'),
      f(ulcorner, X4:'Patch', X20:'Point'),
      f(shape, X4:'Piece', X21:'Point'),
      f(astuple, X14:'Integer', X15:'Integer', X22:'Point'),
      f(multiply, X21:'Numerical', X22:'Numerical', X23:'Numerical'),
      f(add, X20:'Numerical', X23:'Numerical', X24:'Numerical'),
      f(shift, X19:'Patch', X24:'Point', X25:'Patch'),
      f(paint, X3:'Grid', X25:'Object', OUT:'Grid')
    ].
l_solve('776ffc46', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'FIVE':'Integer', X2:'Objects'),
      f(totuple, X2:'FrozenSet', X3:'Tuple'),
      f(rbind, subgrid, IN:'Any', X4:'Callable'),
      f(apply, X4, X3:'Container', X5:'Container'),
      f(multiply, 'FOUR':'Numerical', 'SIX':'Numerical', X6:'Numerical'),
      f(rbind, ofcolor, 'FIVE':'Any', X7:'Callable'),
      f(compose, size, X7, X8:'Callable'),
      f(matcher, X8, X6:'Any', X9:'Callable'),
      f(extract, X5:'Container', X9, X10:'Any'),
      f(astuple, 'FIVE':'Integer', 'FIVE':'Integer', X11:'Point'),
      f(crop, X10:'Grid', (1, 1):'Point', X11:'Point', X12:'Grid'),
      f(objects,
        X12:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X13:'Objects'),
      f(first, X13:'Container', X14:'Any'),
      f(color, X14:'Object', X15:'Integer'),
      f(normalize, X14:'Patch', X16:'Patch'),
      f(toindices, X16:'Patch', X17:'Indices'),
      f(totuple, X1:'FrozenSet', X18:'Tuple'),
      f(apply, normalize, X18:'Container', X19:'Container'),
      f(apply, toindices, X19:'Container', X20:'Container'),
      f(pair, X18:'Tuple', X20:'Tuple', X21:'TupleTuple'),
      f(matcher, last, X17:'Any', X22:'Callable'),
      f(sfilter, X21:'Container', X22, X23:'Container'),
      f(mapply, first, X23:'ContainerContainer', X24:'FrozenSet'),
      f(recolor, X15:'Integer', X24:'Patch', X25:'Object'),
      f(paint, IN:'Grid', X25:'Object', OUT:'Grid')
    ].
l_solve(f35d900a, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(merge, X1:'ContainerContainer', X2:'Container'),
      f(hmirror, X2:'Piece', X3:'Piece'),
      f(compose, neighbors, last, X4:'Callable'),
      f(fork, recolor, first, X4, X5:'Callable'),
      f(mapply, X5, X3:'ContainerContainer', X6:'FrozenSet'),
      f(paint, IN:'Grid', X6:'Object', X7:'Grid'),
      f(outbox, X2:'Patch', X8:'Indices'),
      f(ulcorner, X8:'Patch', X9:'Point'),
      f(subgrid, X8:'Patch', X7:'Grid', X10:'Grid'),
      f(dneighbors, (0, 0):'Point', X11:'Indices'),
      f(rbind, multiply, 'FOUR':'Any', X12:'Callable'),
      f(apply, double, X11:'Container', X13:'Container'),
      f(apply, X12, X11:'Container', X14:'Container'),
      f(apply, increment, X13:'Container', X15:'Container'),
      f(apply, increment, X14:'Container', X16:'Container'),
      f(combine, X15:'Container', X16:'Container', X17:'Container'),
      f(underfill, X10:'Grid', 'FIVE':'Integer', X17:'Patch', X18:'Grid'),
      f(vmirror, X18:'Piece', X19:'Piece'),
      f(underfill, X19:'Grid', 'FIVE':'Integer', X17:'Patch', X20:'Grid'),
      f(ofcolor, X20:'Grid', 'FIVE':'Integer', X21:'Indices'),
      f(hmirror, X20:'Piece', X22:'Piece'),
      f(underfill, X22:'Grid', 'FIVE':'Integer', X21:'Patch', X23:'Grid'),
      f(ofcolor, X23:'Grid', 'FIVE':'Integer', X24:'Indices'),
      f(shift, X24:'Patch', X9:'Point', X25:'Patch'),
      f(fill, X7:'Grid', 'FIVE':'Integer', X25:'Patch', OUT:'Grid')
    ].
l_solve('0dfd9992', IN, OUT) :-
    [ f(height, IN:'Piece', X1:'Integer'),
      f(width, IN:'Piece', X2:'Integer'),
      f(partition, IN:'Grid', X3:'Objects'),
      f(colorfilter, X3:'Objects', 'ZERO':'Integer', X4:'Objects'),
      f(difference, X3:'FrozenSet', X4:'FrozenSet', X5:'FrozenSet'),
      f(merge, X5:'ContainerContainer', X6:'Container'),
      f(astuple, X1:'Integer', 'ONE':'Integer', X7:'Point'),
      f(astuple, 'ONE':'Integer', X2:'Integer', X8:'Point'),
      f(decrement, X1:'Numerical', X9:'Numerical'),
      f(decrement, X2:'Numerical', X10:'Numerical'),
      f(toivec, X10:'Integer', X11:'Point'),
      f(tojvec, X9:'Integer', X12:'Point'),
      f(crop, IN:'Grid', X11:'Point', X8:'Point', X13:'Grid'),
      f(crop, IN:'Grid', X12:'Point', X7:'Point', X14:'Grid'),
      f(asobject, X14:'Grid', X15:'Object'),
      f(asobject, X13:'Grid', X16:'Object'),
      f(vperiod, X15:'Object', X17:'Integer'),
      f(hperiod, X16:'Object', X18:'Integer'),
      f(astuple, X17:'Integer', X18:'Integer', X19:'Point'),
      f(lbind, multiply, X19:'Any', X20:'Callable'),
      f(neighbors, (0, 0):'Point', X21:'Indices'),
      f(mapply, neighbors, X21:'ContainerContainer', X22:'FrozenSet'),
      f(apply, X20, X22:'Container', X23:'Container'),
      f(lbind, shift, X6:'Any', X24:'Callable'),
      f(mapply, X24, X23:'ContainerContainer', X25:'FrozenSet'),
      f(paint, IN:'Grid', X25:'Object', OUT:'Grid')
    ].
l_solve('29ec7d0e', IN, OUT) :-
    [ f(height, IN:'Piece', X1:'Integer'),
      f(width, IN:'Piece', X2:'Integer'),
      f(partition, IN:'Grid', X3:'Objects'),
      f(colorfilter, X3:'Objects', 'ZERO':'Integer', X4:'Objects'),
      f(difference, X3:'FrozenSet', X4:'FrozenSet', X5:'FrozenSet'),
      f(merge, X5:'ContainerContainer', X6:'Container'),
      f(astuple, X1:'Integer', 'ONE':'Integer', X7:'Point'),
      f(astuple, 'ONE':'Integer', X2:'Integer', X8:'Point'),
      f(decrement, X1:'Numerical', X9:'Numerical'),
      f(decrement, X2:'Numerical', X10:'Numerical'),
      f(toivec, X10:'Integer', X11:'Point'),
      f(tojvec, X9:'Integer', X12:'Point'),
      f(crop, IN:'Grid', X11:'Point', X8:'Point', X13:'Grid'),
      f(crop, IN:'Grid', X12:'Point', X7:'Point', X14:'Grid'),
      f(asobject, X14:'Grid', X15:'Object'),
      f(asobject, X13:'Grid', X16:'Object'),
      f(vperiod, X15:'Object', X17:'Integer'),
      f(hperiod, X16:'Object', X18:'Integer'),
      f(astuple, X17:'Integer', X18:'Integer', X19:'Point'),
      f(lbind, multiply, X19:'Any', X20:'Callable'),
      f(neighbors, (0, 0):'Point', X21:'Indices'),
      f(mapply, neighbors, X21:'ContainerContainer', X22:'FrozenSet'),
      f(apply, X20, X22:'Container', X23:'Container'),
      f(lbind, shift, X6:'Any', X24:'Callable'),
      f(mapply, X24, X23:'ContainerContainer', X25:'FrozenSet'),
      f(paint, IN:'Grid', X25:'Object', OUT:'Grid')
    ].
l_solve('36d67576', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(argmax, X1:'Container', numcolors, X2:'Any'),
      f(astuple, 'TWO':'Integer', 'FOUR':'Integer', X3:'Point'),
      f(rbind, contained, X3:'Any', X4:'Callable'),
      f(compose, X4, first, X5:'Callable'),
      f(rbind, sfilter, X5:'Any', X6:'Callable'),
      f(lbind, rbind, subtract:'Any', X7:'Callable'),
      f(lbind, occurrences, IN:'Any', X8:'Callable'),
      f(lbind, lbind, shift:'Any', X9:'Callable'),
      f(compose, X7, ulcorner, X10:'Callable'),
      f(chain, X10, X6, normalize, X11:'Callable'),
      f(chain, X8, X6, normalize, X12:'Callable'),
      f(fork, apply, X11, X12, X13:'Callable'),
      f(compose, X9, normalize, X14:'Callable'),
      f(fork, mapply, X14, X13, X15:'Callable'),
      f(astuple, cmirror:'Integer', dmirror:'Integer', X16:'Point'),
      f(astuple, hmirror:'Integer', vmirror:'Integer', X17:'Point'),
      f(combine, X16:'Container', X17:'Container', X18:'Container'),
      f(product, X18:'Container', X18:'Container', X19:'FrozenSet'),
      f(fork, compose, first, last, X20:'Callable'),
      f(apply, X20, X19:'Container', X21:'Container'),
      f(totuple, X21:'FrozenSet', X22:'Tuple'),
      f(combine, X18:'Container', X22:'Container', X23:'Container'),
      f(rapply, X23:'Container', X2:'Any', X24:'Container'),
      f(mapply, X15, X24:'ContainerContainer', X25:'FrozenSet'),
      f(paint, IN:'Grid', X25:'Object', OUT:'Grid')
    ].
l_solve('98cf29f8', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(fork, multiply, height, width, X2:'Callable'),
      f(fork, equality, size, X2, X3:'Callable'),
      f(extract, X1:'Container', X3, X4:'Any'),
      f(other, X1:'Container', X4:'Any', X5:'Any'),
      f(toindices, X5:'Patch', X6:'Indices'),
      f(rbind, add, (0, -1):'Any', X7:'Callable'),
      f(rbind, add, (0, 1):'Any', X8:'Callable'),
      f(rbind, add, (-1, 0):'Any', X9:'Callable'),
      f(rbind, add, (1, 0):'Any', X10:'Callable'),
      f(initset, 'ZERO':'Any', X11:'FrozenSet'),
      f(compose, initset, X8, X12:'Callable'),
      f(compose, initset, X10, X13:'Callable'),
      f(fork, insert, X7, X12, X14:'Callable'),
      f(fork, insert, X9, X13, X15:'Callable'),
      f(matcher, palette, X11:'Any', X16:'Callable'),
      f(rbind, toobject, IN:'Any', X17:'Callable'),
      f(chain, X16, X17, X14, X18:'Callable'),
      f(chain, X16, X17, X15, X19:'Callable'),
      f(fork, either, X18, X19, X20:'Callable'),
      f(sfilter, X6:'Container', X20, X21:'Container'),
      f(cover, IN:'Grid', X21:'Patch', X22:'Grid'),
      f(difference, X6:'FrozenSet', X21:'FrozenSet', X23:'FrozenSet'),
      f(gravitate, X23:'Patch', X4:'Patch', X24:'Point'),
      f(toobject, X23:'Patch', X22:'Grid', X25:'Object'),
      f(move, X22:'Grid', X25:'Object', X24:'Point', OUT:'Grid')
    ].
l_solve('469497ad', IN, OUT) :-
    [ f(numcolors, IN:'Element', X1:'IntegerSet'),
      f(decrement, X1:'Numerical', X2:'Numerical'),
      f(upscale, IN:'Element', X2:'Integer', X3:'Element'),
      f(objects,
        X3:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X4:'Objects'),
      f(argmin, X4:'Container', size, X5:'Any'),
      f(ulcorner, X5:'Patch', X6:'Point'),
      f(llcorner, X5:'Patch', X7:'Point'),
      f(shoot, X6:'Point', (-1, -1):'Point', X8:'Indices'),
      f(shoot, X6:'Point', (1, 1):'Point', X9:'Indices'),
      f(shoot, X7:'Point', (1, -1):'Point', X10:'Indices'),
      f(shoot, X7:'Point', (-1, 1):'Point', X11:'Indices'),
      f(combine, X8:'Container', X9:'Container', X12:'Container'),
      f(combine, X10:'Container', X11:'Container', X13:'Container'),
      f(combine, X12:'Container', X13:'Container', X14:'Container'),
      f(underfill, X3:'Grid', 'TWO':'Integer', X14:'Patch', X15:'Grid'),
      f(objects,
        X15:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X16:'Objects'),
      f(argmax, X16:'Container', lrcorner, X17:'Any'),
      f(urcorner, X17:'Patch', X18:'Point'),
      f(tojvec, 'NEG_TWO':'Integer', X19:'Point'),
      f(add, X18:'Numerical', X19:'Numerical', X20:'Numerical'),
      f(connect, X18:'Point', X20:'Point', X21:'Indices'),
      f(toobject, X21:'Patch', X15:'Grid', X22:'Object'),
      f(shift, X22:'Patch', (-1, 0):'Point', X23:'Patch'),
      f(color, X23:'Object', X24:'Integer'),
      f(equality, X24:'Any', 'SIX':'Any', returns=X25),
      f(branch, condition=X25, X23:'Any', X17:'Any', X26:'Any'),
      f(paint, X15:'Grid', X26:'Object', OUT:'Grid')
    ].
l_solve('39e1d7f9', IN, OUT) :-
    [ f(fgpartition, IN:'Grid', X1:'Objects'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X2:'Objects'),
      f(order, X1:'Container', height, X3:'Tuple'),
      f(last, X3:'Container', X4:'Any'),
      f(remove, X4:'Any', X3:'Container', X5:'Container'),
      f(last, X5:'Container', X6:'Any'),
      f(color, X6:'Object', X7:'Integer'),
      f(colorfilter, X2:'Objects', X7:'Integer', X8:'Objects'),
      f(power, outbox, 'TWO':'Integer', X9:'Callable'),
      f(rbind, toobject, IN:'Any', X10:'Callable'),
      f(chain, numcolors, X10, X9, X11:'Callable'),
      f(argmax, X8:'Container', X11, X12:'Any'),
      f(ulcorner, X12:'Patch', X13:'Point'),
      f(shape, X12:'Piece', X14:'Point'),
      f(subtract, X13:'Numerical', X14:'Numerical', X15:'Numerical'),
      f(decrement, X15:'Numerical', X16:'Numerical'),
      f(multiply, X14:'Numerical', 'THREE':'Numerical', X17:'Numerical'),
      f(add, X17:'Numerical', (2, 2):'Numerical', X18:'Numerical'),
      f(crop, IN:'Grid', X16:'Point', X18:'Point', X19:'Grid'),
      f(asobject, X19:'Grid', X20:'Object'),
      f(apply, ulcorner, X8:'Container', X21:'Container'),
      f(increment, X14:'Numerical', X22:'Numerical'),
      f(rbind, subtract, X22:'Any', X23:'Callable'),
      f(apply, X23, X21:'Container', X24:'Container'),
      f(lbind, shift, X20:'Any', X25:'Callable'),
      f(mapply, X25, X24:'ContainerContainer', X26:'FrozenSet'),
      f(paint, IN:'Grid', X26:'Object', OUT:'Grid')
    ].
l_solve('484b58aa', IN, OUT) :-
    [ f(height, IN:'Piece', X1:'Integer'),
      f(width, IN:'Piece', X2:'Integer'),
      f(partition, IN:'Grid', X3:'Objects'),
      f(colorfilter, X3:'Objects', 'ZERO':'Integer', X4:'Objects'),
      f(difference, X3:'FrozenSet', X4:'FrozenSet', X5:'FrozenSet'),
      f(merge, X5:'ContainerContainer', X6:'Container'),
      f(astuple, X1:'Integer', 'TWO':'Integer', X7:'Point'),
      f(astuple, 'TWO':'Integer', X2:'Integer', X8:'Point'),
      f(power, decrement, 'TWO':'Integer', X9:'Callable'),
      f(X9, X1, X10),
      f(X9, X2, X11),
      f(toivec, X11:'Integer', X12:'Point'),
      f(tojvec, X10:'Integer', X13:'Point'),
      f(crop, IN:'Grid', X12:'Point', X8:'Point', X14:'Grid'),
      f(crop, IN:'Grid', X13:'Point', X7:'Point', X15:'Grid'),
      f(asobject, X15:'Grid', X16:'Object'),
      f(asobject, X14:'Grid', X17:'Object'),
      f(vperiod, X16:'Object', X18:'Integer'),
      f(hperiod, X17:'Object', X19:'Integer'),
      f(astuple, X18:'Integer', X19:'Integer', X20:'Point'),
      f(lbind, multiply, X20:'Any', X21:'Callable'),
      f(neighbors, (0, 0):'Point', X22:'Indices'),
      f(mapply, neighbors, X22:'ContainerContainer', X23:'FrozenSet'),
      f(apply, X21, X23:'Container', X24:'Container'),
      f(lbind, shift, X6:'Any', X25:'Callable'),
      f(mapply, X25, X24:'ContainerContainer', X26:'FrozenSet'),
      f(paint, IN:'Grid', X26:'Object', OUT:'Grid')
    ].
l_solve('3befdf3e', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(totuple, X1:'FrozenSet', X2:'Tuple'),
      f(apply, mostcolor, X2:'Container', X3:'Container'),
      f(apply, leastcolor, X2:'Container', X4:'Container'),
      f(apply, width, X2:'Container', X5:'Container'),
      f(rbind, subtract, 'TWO':'Any', X6:'Callable'),
      f(apply, X6, X5:'Container', X7:'Container'),
      f(apply, invert, X7:'Container', X8:'Container'),
      f(papply, recolor, X4:'Tuple', X2:'Tuple', X9:'Tuple'),
      f(apply, toivec, X7:'Container', X10:'Container'),
      f(mpapply, shift, X9:'Tuple', X10:'Tuple', X11:'Tuple'),
      f(paint, IN:'Grid', X11:'Object', X12:'Grid'),
      f(apply, toivec, X8:'Container', X13:'Container'),
      f(mpapply, shift, X9:'Tuple', X13:'Tuple', X14:'Tuple'),
      f(paint, X12:'Grid', X14:'Object', X15:'Grid'),
      f(apply, tojvec, X7:'Container', X16:'Container'),
      f(mpapply, shift, X9:'Tuple', X16:'Tuple', X17:'Tuple'),
      f(paint, X15:'Grid', X17:'Object', X18:'Grid'),
      f(apply, tojvec, X8:'Container', X19:'Container'),
      f(mpapply, shift, X9:'Tuple', X19:'Tuple', X20:'Tuple'),
      f(paint, X18:'Grid', X20:'Object', X21:'Grid'),
      f(merge, X2:'ContainerContainer', X22:'Container'),
      f(paint, X21:'Grid', X22:'Object', X23:'Grid'),
      f(first, X4:'Container', X24:'Any'),
      f(first, X3:'Container', X25:'Any'),
      f(replace,
        X23:'Grid',
        X24:'Integer',
        'NEG_ONE':'Integer',
        X26:'Grid'),
      f(replace,
        X26:'Grid',
        X25:'Integer',
        X24:'Integer',
        X27:'Grid'),
      f(replace,
        X27:'Grid',
        'NEG_ONE':'Integer',
        X25:'Integer',
        OUT:'Grid')
    ].
l_solve('9aec4887', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'EIGHT':'Integer', X2:'Objects'),
      f(first, X2:'Container', X3:'Any'),
      f(other, X1:'Container', X3:'Any', X4:'Any'),
      f(subgrid, X4:'Patch', IN:'Grid', X5:'Grid'),
      f(normalize, X3:'Patch', X6:'Patch'),
      f(shift, X6:'Patch', (1, 1):'Point', X7:'Patch'),
      f(paint, X5:'Grid', X7:'Object', X8:'Grid'),
      f(palette, X8:'Element', X9:'IntegerSet'),
      f(remove, 'ZERO':'Any', X9:'Container', X10:'Container'),
      f(remove, 'EIGHT':'Any', X10:'Container', X11:'Container'),
      f(lbind, ofcolor, X8:'Any', X12:'Callable'),
      f(rbind, remove, X11:'Any', X13:'Callable'),
      f(lbind, mapply, X12:'Any', X14:'Callable'),
      f(lbind, rbind, manhattan:'Any', X15:'Callable'),
      f(chain, X15, X14, X13, X16:'Callable'),
      f(rbind, compose, initset:'Any', X17:'Callable'),
      f(lbind, lbind, manhattan:'Any', X18:'Callable'),
      f(chain, X17, X18, X12, X19:'Callable'),
      f(compose, X17, X16, X20:'Callable'),
      f(lbind, fork, greater:'Any', X21:'Callable'),
      f(fork, X21, X20, X19, X22:'Callable'),
      f(ofcolor, X8:'Grid', 'EIGHT':'Integer', X23:'Indices'),
      f(lbind, sfilter, X23:'Any', X24:'Callable'),
      f(compose, X24, X22, X25:'Callable'),
      f(fork, recolor, identity, X25, X26:'Callable'),
      f(mapply, X26, X11:'ContainerContainer', X27:'FrozenSet'),
      f(paint, X8:'Grid', X27:'Object', OUT:'Grid')
    ].
l_solve('49d1d64f', IN, OUT) :-
    [ f(upscale, IN:'Element', 'TWO':'Integer', X1:'Element'),
      f(asindices, X1:'Grid', X2:'Indices'),
      f(corners, X2:'Patch', X3:'Indices'),
      f(fill, X1:'Grid', 'ZERO':'Integer', X3:'Patch', X4:'Grid'),
      f(height, X4:'Piece', X5:'Integer'),
      f(width, X4:'Piece', X6:'Integer'),
      f(equality, X5:'Any', 'SIX':'Any', returns=X7),
      f(equality, X6:'Any', 'SIX':'Any', returns=X8),
      f(compose, decrement, halve, X9:'Callable'),
      f(X9, X5, X10),
      f(X9, X6, X11),
      f(astuple, X10:'Integer', X6:'Integer', X12:'Point'),
      f(crop, X4:'Grid', (0, 0):'Point', X12:'Point', X13:'Grid'),
      f(increment, X10:'Numerical', X14:'Numerical'),
      f(toivec, X14:'Integer', X15:'Point'),
      f(astuple, X14:'Integer', X6:'Integer', X16:'Point'),
      f(crop, X4:'Grid', X15:'Point', X16:'Point', X17:'Grid'),
      f(vconcat, X13:'Grid', X17:'Grid', X18:'Grid'),
      f(branch, condition=X7, X18:'Any', X4:'Any', X19:'Any'),
      f(decrement, X5:'Numerical', X20:'Numerical'),
      f(branch, condition=X7, X20:'Any', X5:'Any', X21:'Any'),
      f(astuple, X21:'Integer', X11:'Integer', X22:'Point'),
      f(crop, X19:'Grid', (0, 0):'Point', X22:'Point', X23:'Grid'),
      f(increment, X11:'Numerical', X24:'Numerical'),
      f(tojvec, X24:'Integer', X25:'Point'),
      f(astuple, X21:'Integer', X24:'Integer', X26:'Point'),
      f(crop, X19:'Grid', X25:'Point', X26:'Point', X27:'Grid'),
      f(hconcat, X23:'Grid', X27:'Grid', X28:'Grid'),
      f(branch, condition=X8, X28:'Any', X19:'Any', OUT:'Any')
    ].
l_solve('57aa92db', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X2:'Objects'),
      f(lbind, lbind, colorcount:'Any', X3:'Callable'),
      f(fork, apply, X3, palette, X4:'Callable'),
      f(compose, maximum, X4, X5:'Callable'),
      f(compose, minimum, X4, X6:'Callable'),
      f(fork, subtract, X5, X6, X7:'Callable'),
      f(argmax, X1:'Container', X7, X8:'Any'),
      f(leastcolor, X8:'Element', X9:'Integer'),
      f(normalize, X8:'Patch', X10:'Patch'),
      f(matcher, first, X9:'Any', X11:'Callable'),
      f(sfilter, X10:'Container', X11, X12:'Container'),
      f(ulcorner, X12:'Patch', X13:'Point'),
      f(colorfilter, X2:'Objects', X9:'Integer', X14:'Objects'),
      f(rbind, toobject, IN:'Any', X15:'Callable'),
      f(lbind, remove, 'ZERO':'Any', X16:'Callable'),
      f(chain, first, X16, palette, X17:'Callable'),
      f(chain, X17, X15, outbox, X18:'Callable'),
      f(lbind, multiply, X13:'Any', X19:'Callable'),
      f(compose, X19, width, X20:'Callable'),
      f(fork, subtract, ulcorner, X20, X21:'Callable'),
      f(lbind, shift, X10:'Any', X22:'Callable'),
      f(compose, X22, X21, X23:'Callable'),
      f(fork, upscale, X23, width, X24:'Callable'),
      f(fork, recolor, X18, X24, X25:'Callable'),
      f(mapply, X25, X14:'ContainerContainer', X26:'FrozenSet'),
      f(paint, IN:'Grid', X26:'Object', X27:'Grid'),
      f(merge, X2:'ContainerContainer', X28:'Container'),
      f(paint, X27:'Grid', X28:'Object', OUT:'Grid')
    ].
l_solve(aba27056, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(mapply, toindices, X1:'ContainerContainer', X2:'FrozenSet'),
      f(box, X2:'Patch', X3:'Indices'),
      f(difference, X3:'FrozenSet', X2:'FrozenSet', X4:'FrozenSet'),
      f(delta, X2:'Patch', X5:'Indices'),
      f(position, X5:'Patch', X4:'Patch', X6:'Point'),
      f(interval, 'ZERO':'Integer', 'NINE':'Integer', 'ONE':'Integer', X7:'Tuple'),
      f(lbind, multiply, X6:'Any', X8:'Callable'),
      f(apply, X8, X7:'Container', X9:'Container'),
      f(lbind, shift, X4:'Any', X10:'Callable'),
      f(mapply, X10, X9:'ContainerContainer', X11:'FrozenSet'),
      f(fill, IN:'Grid', 'FOUR':'Integer', X5:'Patch', X12:'Grid'),
      f(fill, X12:'Grid', 'FOUR':'Integer', X11:'Patch', X13:'Grid'),
      f(corners, X4:'Patch', X14:'Indices'),
      f(ofcolor, X13:'Grid', 'ZERO':'Integer', X15:'Indices'),
      f(rbind, toobject, X13:'Any', X16:'Callable'),
      f(rbind, colorcount, 'ZERO':'Any', X17:'Callable'),
      f(chain, X17, X16, dneighbors, X18:'Callable'),
      f(matcher, X18, 'TWO':'Any', X19:'Callable'),
      f(rbind, adjacent, X2:'Any', X20:'Callable'),
      f(rbind, adjacent, X11:'Any', X21:'Callable'),
      f(fork, both, X20, X21, X22:'Callable'),
      f(compose, X22, initset, X23:'Callable'),
      f(sfilter, X15:'Container', X19, X24:'Container'),
      f(sfilter, X24:'Container', X23, X25:'Container'),
      f(product, X14:'Container', X25:'Container', X26:'FrozenSet'),
      f(fork, subtract, last, first, X27:'Callable'),
      f(fork, shoot, first, X27, X28:'Callable'),
      f(mapply, X28, X26:'ContainerContainer', X29:'FrozenSet'),
      f(fill, X13:'Grid', 'FOUR':'Integer', X29:'Patch', OUT:'Grid')
    ].
l_solve(f1cefba8, IN, OUT) :-
    [ f(palette, IN:'Element', X1:'IntegerSet'),
      f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X2:'Objects'),
      f(ofcolor, IN:'Grid', 'ZERO':'Integer', X3:'Indices'),
      f(first, X2:'Container', X4:'Any'),
      f(ulcorner, X4:'Patch', X5:'Point'),
      f(subgrid, X4:'Patch', IN:'Grid', X6:'Grid'),
      f(power, trim, 'TWO':'Integer', X7:'Callable'),
      f(X7, X6, X8),
      f(asindices, X8:'Grid', X9:'Indices'),
      f(shift, X9:'Patch', (2, 2):'Point', X10:'Patch'),
      f(fill, X6:'Grid', 'ZERO':'Integer', X10:'Patch', X11:'Grid'),
      f(leastcolor, X11:'Element', X12:'Integer'),
      f(remove, 'ZERO':'Any', X1:'Container', X13:'Container'),
      f(other, X13:'Container', X12:'Any', X14:'Any'),
      f(ofcolor, X11:'Grid', X12:'Integer', X15:'Indices'),
      f(shift, X15:'Patch', X5:'Point', X16:'Patch'),
      f(ofcolor, IN:'Grid', X12:'Integer', X17:'Indices'),
      f(uppermost, X17:'Patch', X18:'Integer'),
      f(lowermost, X17:'Patch', X19:'Integer'),
      f(matcher, first, X18:'Any', X20:'Callable'),
      f(matcher, first, X19:'Any', X21:'Callable'),
      f(fork, either, X20, X21, X22:'Callable'),
      f(sfilter, X16:'Container', X22, X23:'Container'),
      f(difference,
        X16:'FrozenSet',
        X23:'FrozenSet',
        X24:'FrozenSet'),
      f(mapply, vfrontier, X23:'ContainerContainer', X25:'FrozenSet'),
      f(mapply, hfrontier, X24:'ContainerContainer', X26:'FrozenSet'),
      f(combine, X25:'Container', X26:'Container', X27:'Container'),
      f(intersection,
        X3:'FrozenSet',
        X27:'FrozenSet',
        X28:'FrozenSet'),
      f(fill, IN:'Grid', X14:'Integer', X27:'Patch', X29:'Grid'),
      f(fill, X29:'Grid', X12:'Integer', X28:'Patch', OUT:'Grid')
    ].
l_solve('1e32b0e9', IN, OUT) :-
    [ f(height, IN:'Piece', X1:'Integer'),
      f(mostcolor, IN:'Element', X2:'Integer'),
      f(asobject, IN:'Grid', X3:'Object'),
      f(subtract, X1:'Numerical', 'TWO':'Numerical', X4:'Numerical'),
      f(divide, X4:'Numerical', 'THREE':'Numerical', X5:'Numerical'),
      f(astuple, X5:'Integer', X5:'Integer', X6:'Point'),
      f(crop, IN:'Grid', (0, 0):'Point', X6:'Point', X7:'Grid'),
      f(partition, X7:'Grid', X8:'Objects'),
      f(matcher, color, 'ZERO':'Any', X9:'Callable'),
      f(compose, flip, X9, X10:'Callable'),
      f(extract, X8:'Container', X10, X11:'Any'),
      f(initset, X2:'Any', X12:'FrozenSet'),
      f(palette, X3:'Element', X13:'IntegerSet'),
      f(palette, X11:'Element', X14:'IntegerSet'),
      f(difference,
        X13:'FrozenSet',
        X14:'FrozenSet',
        X15:'FrozenSet'),
      f(difference,
        X15:'FrozenSet',
        X12:'FrozenSet',
        X16:'FrozenSet'),
      f(first, X16:'Container', X17:'Any'),
      f(interval, 'ZERO':'Integer', 'THREE':'Integer', 'ONE':'Integer', X18:'Tuple'),
      f(product, X18:'Container', X18:'Container', X19:'FrozenSet'),
      f(totuple, X19:'FrozenSet', X20:'Tuple'),
      f(apply, first, X20:'Container', X21:'Container'),
      f(apply, last, X20:'Container', X22:'Container'),
      f(lbind, multiply, X5:'Any', X23:'Callable'),
      f(apply, X23, X21:'Container', X24:'Container'),
      f(apply, X23, X22:'Container', X25:'Container'),
      f(papply, add, X24:'Tuple', X21:'Tuple', X26:'Tuple'),
      f(papply, add, X25:'Tuple', X22:'Tuple', X27:'Tuple'),
      f(papply, astuple, X26:'Tuple', X27:'Tuple', X28:'Tuple'),
      f(lbind, shift, X11:'Any', X29:'Callable'),
      f(mapply, X29, X28:'ContainerContainer', X30:'FrozenSet'),
      f(underfill,
        IN:'Grid',
        X17:'Integer',
        X30:'Patch',
        OUT:'Grid')
    ].
l_solve('28e73c20', IN, OUT) :-
    [ f(width, IN:'Piece', X1:'Integer'),
      f(astuple, 'ONE':'Integer', 'TWO':'Integer', X2:'Point'),
      f(astuple, 'TWO':'Integer', 'TWO':'Integer', X3:'Point'),
      f(astuple, 'TWO':'Integer', 'ONE':'Integer', X4:'Point'),
      f(astuple, 'THREE':'Integer', 'ONE':'Integer', X5:'Point'),
      f(canvas, 'THREE':'Integer', (1, 1):'Point', X6:'Grid'),
      f(upscale, X6:'Element', 'FOUR':'Integer', X7:'Element'),
      f(initset, (1, 0):'Any', X8:'FrozenSet'),
      f(insert, (1, 1):'Any', X8:'FrozenSet', X9:'FrozenSet'),
      f(insert, X2:'Any', X9:'FrozenSet', X10:'FrozenSet'),
      f(insert, X3:'Any', X10:'FrozenSet', X11:'FrozenSet'),
      f(fill, X7:'Grid', 'ZERO':'Integer', X11:'Patch', X12:'Grid'),
      f(vupscale, X6:'Grid', 'FIVE':'Integer', X13:'Grid'),
      f(hupscale, X13:'Grid', 'THREE':'Integer', X14:'Grid'),
      f(insert, X4:'Any', X9:'FrozenSet', X15:'FrozenSet'),
      f(insert, X5:'Any', X15:'FrozenSet', X16:'FrozenSet'),
      f(fill, X14:'Grid', 'ZERO':'Integer', X16:'Patch', X17:'Grid'),
      f(even, X1:'Integer', returns=X18),
      f(branch, condition=X18, X12:'Any', X17:'Any', X19:'Any'),
      f(canvas, 'ZERO':'Integer', (1, 1):'Point', X20:'Grid'),
      f(lbind, hupscale, X20:'Any', X21:'Callable'),
      f(chain, X21, decrement, height, X22:'Callable'),
      f(rbind, hconcat, X6:'Any', X23:'Callable'),
      f(compose, X23, X22, X24:'Callable'),
      f(lbind, hupscale, X6:'Any', X25:'Callable'),
      f(compose, X25, height, X26:'Callable'),
      f(fork, vconcat, X24, rot90, X27:'Callable'),
      f(fork, vconcat, X26, X27, X28:'Callable'),
      f(subtract, X1:'Numerical', 'FOUR':'Numerical', X29:'Numerical'),
      f(power, X28, X29:'Integer', X30:'Callable'),
      f(X30, X19, OUT)
    ].
l_solve('4c5c2cf0', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X2:'Objects'),
      f(first, X2:'Container', X3:'Any'),
      f(rbind, subgrid, IN:'Any', X4:'Callable'),
      f(fork, equality, identity, rot90, X5:'Callable'),
      f(compose, X5, X4, X6:'Callable'),
      f(extract, X1:'Container', X6, X7:'Any'),
      f(center, X7:'Patch', X8:'Point'),
      f(subgrid, X3:'Patch', IN:'Grid', X9:'Grid'),
      f(hmirror, X9:'Piece', X10:'Piece'),
      f(objects,
        X10:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X11:'Objects'),
      f(first, X11:'Container', X12:'Any'),
      f(objects,
        X10:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X13:'Objects'),
      f(extract, X13:'Container', X6, X14:'Any'),
      f(center, X14:'Patch', X15:'Point'),
      f(subtract, X8:'Numerical', X15:'Numerical', X16:'Numerical'),
      f(shift, X12:'Patch', X16:'Point', X17:'Patch'),
      f(paint, IN:'Grid', X17:'Object', X18:'Grid'),
      f(objects,
        X18:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X19:'Objects'),
      f(first, X19:'Container', X20:'Any'),
      f(subgrid, X20:'Patch', X18:'Grid', X21:'Grid'),
      f(vmirror, X21:'Piece', X22:'Piece'),
      f(objects,
        X22:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X23:'Objects'),
      f(first, X23:'Container', X24:'Any'),
      f(objects,
        X22:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X25:'Objects'),
      f(color, X7:'Object', X26:'Integer'),
      f(matcher, color, X26:'Any', X27:'Callable'),
      f(extract, X25:'Container', X27, X28:'Any'),
      f(center, X28:'Patch', X29:'Point'),
      f(subtract, X8:'Numerical', X29:'Numerical', X30:'Numerical'),
      f(shift, X24:'Patch', X30:'Point', X31:'Patch'),
      f(paint, X18:'Grid', X31:'Object', OUT:'Grid')
    ].
l_solve('508bd3b6', IN, OUT) :-
    [ f(width, IN:'Piece', X1:'Integer'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X2:'Objects'),
      f(argmin, X2:'Container', size, X3:'Any'),
      f(argmax, X2:'Container', size, X4:'Any'),
      f(ulcorner, X3:'Patch', X5:'Point'),
      f(urcorner, X3:'Patch', X6:'Point'),
      f(index, IN:'Grid', X5:'Point', X7:'Integer'),
      f(equality, X7:'Any', 'EIGHT':'Any', returns=X8),
      f(branch, condition=X8, X5:'Any', X6:'Any', X9:'Any'),
      f(branch, condition=X8, (1, 1):'Any', (1, -1):'Any', X10:'Any'),
      f(multiply, X10:'Numerical', X1:'Numerical', X11:'Numerical'),
      f(double, X11:'Numerical', X12:'Numerical'),
      f(add, X9:'Numerical', X12:'Numerical', X13:'Numerical'),
      f(subtract, X9:'Numerical', X12:'Numerical', X14:'Numerical'),
      f(connect, X13:'Point', X14:'Point', X15:'Indices'),
      f(fill, IN:'Grid', 'THREE':'Integer', X15:'Patch', X16:'Grid'),
      f(paint, X16:'Grid', X4:'Object', X17:'Grid'),
      f(objects,
        X17:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X18:'Objects'),
      f(rbind, adjacent, X4:'Any', X19:'Callable'),
      f(extract, X18:'Container', X19, X20:'Any'),
      f(first, X20:'Container', X21:'Any'),
      f(last, X21:'Container', X22:'Any'),
      f(flip, b=X8, returns=X23),
      f(branch, condition=X23, (1, 1):'Any', (1, -1):'Any', X24:'Any'),
      f(multiply, X24:'Numerical', X1:'Numerical', X25:'Numerical'),
      f(double, X25:'Numerical', X26:'Numerical'),
      f(add, X22:'Numerical', X26:'Numerical', X27:'Numerical'),
      f(subtract, X22:'Numerical', X26:'Numerical', X28:'Numerical'),
      f(connect, X27:'Point', X28:'Point', X29:'Indices'),
      f(fill, X17:'Grid', 'THREE':'Integer', X29:'Patch', X30:'Grid'),
      f(paint, X30:'Grid', X3:'Object', X31:'Grid'),
      f(paint, X31:'Grid', X4:'Object', OUT:'Grid')
    ].
l_solve('6d0160f0', IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'FOUR':'Integer', X1:'Indices'),
      f(first, X1:'Container', X2:'Any'),
      f(first, X2:'Container', X3:'Any'),
      f(last, X2:'Container', X4:'Any'),
      f(greater, X3:'Integer', 'THREE':'Integer', returns=X5),
      f(greater, X3:'Integer', 'SEVEN':'Integer', returns=X6),
      f(greater, X4:'Integer', 'THREE':'Integer', returns=X7),
      f(greater, X4:'Integer', 'SEVEN':'Integer', returns=X8),
      f(branch, condition=X5, 'FOUR':'Any', 'ZERO':'Any', X9:'Any'),
      f(branch, condition=X6, 'EIGHT':'Any', X9:'Any', X10:'Any'),
      f(branch, condition=X7, 'FOUR':'Any', 'ZERO':'Any', X11:'Any'),
      f(branch, condition=X8, 'EIGHT':'Any', X11:'Any', X12:'Any'),
      f(astuple, X10:'Integer', X12:'Integer', X13:'Point'),
      f(initset, 'ZERO':'Any', X14:'FrozenSet'),
      f(insert, 'FOUR':'Any', X14:'FrozenSet', X15:'FrozenSet'),
      f(insert, 'EIGHT':'Any', X15:'FrozenSet', X16:'FrozenSet'),
      f(product, X16:'Container', X16:'Container', X17:'FrozenSet'),
      f(crop, IN:'Grid', (0, 0):'Point', (3, 3):'Point', X18:'Grid'),
      f(asindices, X18:'Grid', X19:'Indices'),
      f(recolor, 'ZERO':'Integer', X19:'Patch', X20:'Object'),
      f(lbind, shift, X20:'Any', X21:'Callable'),
      f(mapply, X21, X17:'ContainerContainer', X22:'FrozenSet'),
      f(paint, IN:'Grid', X22:'Object', X23:'Grid'),
      f(crop, IN:'Grid', X13:'Point', (3, 3):'Point', X24:'Grid'),
      f(replace, X24:'Grid', 'FIVE':'Integer', 'ZERO':'Integer', X25:'Grid'),
      f(ofcolor, X25:'Grid', 'FOUR':'Integer', X26:'Indices'),
      f(first, X26:'Container', X27:'Any'),
      f(asindices, X25:'Grid', X28:'Indices'),
      f(toobject, X28:'Patch', X25:'Grid', X29:'Object'),
      f(multiply, X27:'Numerical', 'FOUR':'Numerical', X30:'Numerical'),
      f(shift, X29:'Patch', X30:'Point', X31:'Patch'),
      f(paint, X23:'Grid', X31:'Object', OUT:'Grid')
    ].
l_solve(f8a8fe49, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(replace, IN:'Grid', 'FIVE':'Integer', 'ZERO':'Integer', X2:'Grid'),
      f(colorfilter, X1:'Objects', 'TWO':'Integer', X3:'Objects'),
      f(first, X3:'Container', X4:'Any'),
      f(portrait, X4:'Piece', returns=X5),
      f(branch, condition=X5, hsplit:'Any', vsplit:'Any', X6:'Any'),
      f(branch, condition=X5, vmirror:'Any', hmirror:'Any', X7:'Any'),
      f(ofcolor, IN:'Grid', 'TWO':'Integer', X8:'Indices'),
      f(subgrid, X8:'Patch', IN:'Grid', X9:'Grid'),
      f(trim, X9:'Grid', X10:'Grid'),
      f(X7, X10, X11),
      f(X6, X11, 'TWO', X12),
      f(compose, normalize, asobject, X13:'Callable'),
      f(apply, X13, X12:'Container', X14:'Container'),
      f(last, X14:'Container', X15:'Any'),
      f(first, X14:'Container', X16:'Any'),
      f(ulcorner, X8:'Patch', X17:'Point'),
      f(increment, X17:'Numerical', X18:'Numerical'),
      f(shift, X15:'Patch', X18:'Point', X19:'Patch'),
      f(shift, X16:'Patch', X18:'Point', X20:'Patch'),
      f(branch, condition=X5, width:'Any', height:'Any', X21:'Any'),
      f(branch, condition=X5, tojvec:'Any', toivec:'Any', X22:'Any'),
      f(X21, X15, X23),
      f(double, X23:'Numerical', X24:'Numerical'),
      f(compose, X22, increment, X25:'Callable'),
      f(X25, X23, X26),
      f(invert, X26:'Numerical', X27:'Numerical'),
      f(X25, X24, X28),
      f(shift, X19:'Patch', X27:'Point', X29:'Patch'),
      f(shift, X20:'Patch', X28:'Point', X30:'Patch'),
      f(paint, X2:'Grid', X29:'Object', X31:'Grid'),
      f(paint, X31:'Grid', X30:'Object', OUT:'Grid')
    ].
l_solve(d07ae81c, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X1:'Objects'),
      f(sizefilter, X1:'Container', 'ONE':'Integer', X2:'FrozenSet'),
      f(apply, color, X2:'Container', X3:'Container'),
      f(difference, X1:'FrozenSet', X2:'FrozenSet', X4:'FrozenSet'),
      f(apply, color, X4:'Container', X5:'Container'),
      f(first, X5:'Container', X6:'Any'),
      f(last, X5:'Container', X7:'Any'),
      f(ofcolor, IN:'Grid', X6:'Integer', X8:'Indices'),
      f(ofcolor, IN:'Grid', X7:'Integer', X9:'Indices'),
      f(rbind, shoot, (1, 1):'Any', X10:'Callable'),
      f(rbind, shoot, (-1, -1):'Any', X11:'Callable'),
      f(rbind, shoot, (1, -1):'Any', X12:'Callable'),
      f(rbind, shoot, (-1, 1):'Any', X13:'Callable'),
      f(fork, combine, X10, X11, X14:'Callable'),
      f(fork, combine, X12, X13, X15:'Callable'),
      f(fork, combine, X14, X15, X16:'Callable'),
      f(compose, X16, center, X17:'Callable'),
      f(mapply, X17, X2:'ContainerContainer', X18:'FrozenSet'),
      f(intersection,
        X8:'FrozenSet',
        X18:'FrozenSet',
        X19:'FrozenSet'),
      f(intersection,
        X9:'FrozenSet',
        X18:'FrozenSet',
        X20:'FrozenSet'),
      f(first, X2:'Container', X21:'Any'),
      f(color, X21:'Object', X22:'Integer'),
      f(center, X21:'Patch', X23:'Point'),
      f(neighbors, X23:'Point', X24:'Indices'),
      f(toobject, X24:'Patch', IN:'Grid', X25:'Object'),
      f(mostcolor, X25:'Element', X26:'Integer'),
      f(other, X3:'Container', X22:'Any', X27:'Any'),
      f(equality, X26:'Any', X6:'Any', returns=X28),
      f(branch, condition=X28, X22:'Any', X27:'Any', X29:'Any'),
      f(branch, condition=X28, X27:'Any', X22:'Any', X30:'Any'),
      f(fill, IN:'Grid', X29:'Integer', X19:'Patch', X31:'Grid'),
      f(fill, X31:'Grid', X30:'Integer', X20:'Patch', OUT:'Grid')
    ].
l_solve('6a1e5592', IN, OUT) :-
    [ f(width, IN:'Piece', X1:'Integer'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X2:'Objects'),
      f(astuple, 'FIVE':'Integer', X1:'Integer', X3:'Point'),
      f(crop, IN:'Grid', (0, 0):'Point', X3:'Point', X4:'Grid'),
      f(colorfilter, X2:'Objects', 'FIVE':'Integer', X5:'Objects'),
      f(merge, X5:'ContainerContainer', X6:'Container'),
      f(cover, IN:'Grid', X6:'Patch', X7:'Grid'),
      f(compose, toindices, normalize, X8:'Callable'),
      f(apply, X8, X5:'Container', X9:'Container'),
      f(asindices, X4:'Grid', X10:'Indices'),
      f(ofcolor, X4:'Grid', 'ZERO':'Integer', X11:'Indices'),
      f(ofcolor, X4:'Grid', 'TWO':'Integer', X12:'Indices'),
      f(rbind, multiply, 'TEN':'Any', X13:'Callable'),
      f(rbind, multiply, 'EIGHT':'Any', X14:'Callable'),
      f(rbind, intersection, X12:'Any', X15:'Callable'),
      f(rbind, intersection, X11:'Any', X16:'Callable'),
      f(rbind, intersection, X10:'Any', X17:'Callable'),
      f(chain, X13, size, X15, X18:'Callable'),
      f(chain, size, X16, delta, X19:'Callable'),
      f(compose, X14, uppermost, X20:'Callable'),
      f(chain, size, X16, outbox, X21:'Callable'),
      f(chain, X13, size, X17, X22:'Callable'),
      f(compose, invert, X18, X23:'Callable'),
      f(fork, add, X22, X23, X24:'Callable'),
      f(fork, subtract, X24, X21, X25:'Callable'),
      f(fork, subtract, X25, X20, X26:'Callable'),
      f(fork, subtract, X26, X19, X27:'Callable'),
      f(rbind, apply, X10:'Any', X28:'Callable'),
      f(lbind, lbind, shift:'Any', X29:'Callable'),
      f(rbind, argmax, X27:'Any', X30:'Callable'),
      f(chain, X30, X28, X29, X31:'Callable'),
      f(mapply, X31, X9:'ContainerContainer', X32:'FrozenSet'),
      f(fill, X7:'Grid', 'ONE':'Integer', X32:'Patch', OUT:'Grid')
    ].
l_solve('0e206a2e', IN, OUT) :-
    [ f(palette, IN:'Element', X1:'IntegerSet'),
      f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X2:'Objects'),
      f(rbind, greater, 'ONE':'Any', X3:'Callable'),
      f(compose, X3, numcolors, X4:'Callable'),
      f(sfilter, X2:'Container', X4, X5:'Container'),
      f(remove, 'ZERO':'Any', X1:'Container', X6:'Container'),
      f(lbind, colorcount, IN:'Any', X7:'Callable'),
      f(argmax, X6:'Container', X7, X8:'Any'),
      f(remove, X8:'Any', X6:'Container', X9:'Container'),
      f(rbind, contained, X9:'Any', X10:'Callable'),
      f(compose, X10, first, X11:'Callable'),
      f(rbind, sfilter, X11:'Any', X12:'Callable'),
      f(lbind, rbind, subtract:'Any', X13:'Callable'),
      f(lbind, occurrences, IN:'Any', X14:'Callable'),
      f(lbind, lbind, shift:'Any', X15:'Callable'),
      f(compose, X13, ulcorner, X16:'Callable'),
      f(chain, X16, X12, normalize, X17:'Callable'),
      f(chain, X14, X12, normalize, X18:'Callable'),
      f(fork, apply, X17, X18, X19:'Callable'),
      f(compose, X15, normalize, X20:'Callable'),
      f(fork, mapply, X20, X19, X21:'Callable'),
      f(astuple, cmirror:'Integer', dmirror:'Integer', X22:'Point'),
      f(astuple, hmirror:'Integer', vmirror:'Integer', X23:'Point'),
      f(combine, X22:'Container', X23:'Container', X24:'Container'),
      f(product, X24:'Container', X24:'Container', X25:'FrozenSet'),
      f(fork, compose, first, last, X26:'Callable'),
      f(apply, X26, X25:'Container', X27:'Container'),
      f(totuple, X27:'FrozenSet', X28:'Tuple'),
      f(combine, X24:'Container', X28:'Container', X29:'Container'),
      f(lbind, rapply, X29:'Any', X30:'Callable'),
      f(mapply, X30, X5:'ContainerContainer', X31:'FrozenSet'),
      f(mapply, X21, X31:'ContainerContainer', X32:'FrozenSet'),
      f(paint, IN:'Grid', X32:'Object', X33:'Grid'),
      f(merge, X5:'ContainerContainer', X34:'Container'),
      f(cover, X33:'Grid', X34:'Patch', OUT:'Grid')
    ].
l_solve(d22278a0, IN, OUT) :-
    [ f(asindices, IN:'Grid', X1:'Indices'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X2:'Objects'),
      f(fork, multiply, sign, identity, X3:'Callable'),
      f(lbind, apply, X3:'Any', X4:'Callable'),
      f(chain, even, maximum, X4, X5:'Callable'),
      f(lbind, sfilter, X1:'Any', X6:'Callable'),
      f(fork, add, first, last, X7:'Callable'),
      f(rbind, remove, X2:'Any', X8:'Callable'),
      f(compose, center, last, X9:'Callable'),
      f(fork, subtract, first, X9, X10:'Callable'),
      f(compose, X5, X10, X11:'Callable'),
      f(lbind, rbind, equality:'Any', X12:'Callable'),
      f(lbind, argmin, X2:'Any', X13:'Callable'),
      f(chain, X7, X4, X10, X14:'Callable'),
      f(lbind, lbind, astuple:'Any', X15:'Callable'),
      f(lbind, rbind, astuple:'Any', X16:'Callable'),
      f(lbind, compose, X11:'Any', X17:'Callable'),
      f(lbind, compose, X14:'Any', X18:'Callable'),
      f(compose, X18, X15, X19:'Callable'),
      f(compose, X18, X16, X20:'Callable'),
      f(compose, X13, X19, X21:'Callable'),
      f(rbind, compose, X21:'Any', X22:'Callable'),
      f(lbind, lbind, valmin:'Any', X23:'Callable'),
      f(rbind, compose, X19:'Any', X24:'Callable'),
      f(chain, X24, X23, X8, X25:'Callable'),
      f(lbind, fork, greater:'Any', X26:'Callable'),
      f(fork, X26, X25, X20, X27:'Callable'),
      f(chain, X6, X17, X16, X28:'Callable'),
      f(chain, X6, X22, X12, X29:'Callable'),
      f(fork, intersection, X28, X29, X30:'Callable'),
      f(compose, X6, X27, X31:'Callable'),
      f(fork, intersection, X30, X31, X32:'Callable'),
      f(fork, recolor, color, X32, X33:'Callable'),
      f(mapply, X33, X2:'ContainerContainer', X34:'FrozenSet'),
      f(paint, IN:'Grid', X34:'Object', OUT:'Grid')
    ].
l_solve('4290ef0e', IN, OUT) :-
    [ f(mostcolor, IN:'Element', X1:'Integer'),
      f(fgpartition, IN:'Grid', X2:'Objects'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X3:'Objects'),
      f(rbind, valmax, width:'Any', X4:'Callable'),
      f(lbind, colorfilter, X3:'Any', X5:'Callable'),
      f(chain, X4, X5, color, X6:'Callable'),
      f(compose, maximum, shape, X7:'Callable'),
      f(fork, add, X7, X6, X8:'Callable'),
      f(compose, invert, X8, X9:'Callable'),
      f(order, X2:'Container', X9, X10:'Tuple'),
      f(rbind, argmin, centerofmass:'Any', X11:'Callable'),
      f(compose, initset, vmirror, X12:'Callable'),
      f(fork, insert, dmirror, X12, X13:'Callable'),
      f(fork, insert, cmirror, X13, X14:'Callable'),
      f(fork, insert, hmirror, X14, X15:'Callable'),
      f(compose, X11, X15, X16:'Callable'),
      f(apply, X16, X10:'Container', X17:'Container'),
      f(size, X2:'Container', X18:'Integer'),
      f(apply, size, X2:'Container', X19:'Container'),
      f(contained, 'ONE':'Any', X19:'Container', returns=X20),
      f(increment, X18:'Numerical', X21:'Numerical'),
      f(branch, condition=X20, X18:'Any', X21:'Any', X22:'Any'),
      f(double, X22:'Numerical', X23:'Numerical'),
      f(decrement, X23:'Numerical', X24:'Numerical'),
      f(apply, normalize, X17:'Container', X25:'Container'),
      f(interval, 'ZERO':'Integer', X22:'Integer', 'ONE':'Integer', X26:'Tuple'),
      f(pair, X26:'Tuple', X26:'Tuple', X27:'TupleTuple'),
      f(mpapply, shift, X25:'Tuple', X27:'Tuple', X28:'Tuple'),
      f(astuple, X24:'Integer', X24:'Integer', X29:'Point'),
      f(canvas, X1:'Integer', X29:'Point', X30:'Grid'),
      f(paint, X30:'Grid', X28:'Object', X31:'Grid'),
      f(rot90, X31:'Grid', X32:'Grid'),
      f(paint, X32:'Grid', X28:'Object', X33:'Grid'),
      f(rot90, X33:'Grid', X34:'Grid'),
      f(paint, X34:'Grid', X28:'Object', X35:'Grid'),
      f(rot90, X35:'Grid', X36:'Grid'),
      f(paint, X36:'Grid', X28:'Object', OUT:'Grid')
    ].
l_solve(50846271, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'TWO':'Integer', X1:'Indices'),
      f(prapply, connect, X1, X1, X2),
      f(lbind, greater, 'SIX':'Any', X3:'Callable'),
      f(compose, X3, size, X4:'Callable'),
      f(fork, either, vline, hline, X5:'Callable'),
      f(fork, both, X4, X5, X6:'Callable'),
      f(mfilter, X2:'Container', X6, X7:'FrozenSet'),
      f(fill, IN:'Grid', 'TWO':'Integer', X7:'Patch', X8:'Grid'),
      f(objects,
        X8:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X9:'Objects'),
      f(colorfilter, X9:'Objects', 'TWO':'Integer', X10:'Objects'),
      f(valmax, X10:'Container', width, X11:'Integer'),
      f(halve, X11:'Numerical', X12:'Numerical'),
      f(toivec, X12:'Integer', X13:'Point'),
      f(tojvec, X12:'Integer', X14:'Point'),
      f(rbind, add, (0, 2):'Any', X15:'Callable'),
      f(rbind, add, (2, 0):'Any', X16:'Callable'),
      f(rbind, subtract, (0, 2):'Any', X17:'Callable'),
      f(rbind, subtract, (2, 0):'Any', X18:'Callable'),
      f(rbind, colorcount, 'TWO':'Any', X19:'Callable'),
      f(rbind, toobject, X8:'Any', X20:'Callable'),
      f(compose, initset, X15, X21:'Callable'),
      f(fork, insert, X16, X21, X22:'Callable'),
      f(fork, insert, X17, X22, X23:'Callable'),
      f(fork, insert, X18, X23, X24:'Callable'),
      f(fork, combine, dneighbors, X24, X25:'Callable'),
      f(chain, X19, X20, X25, X26:'Callable'),
      f(rbind, argmax, X26:'Any', X27:'Callable'),
      f(compose, X27, toindices, X28:'Callable'),
      f(apply, X28, X10:'Container', X29:'Container'),
      f(rbind, add, X13:'Any', X30:'Callable'),
      f(rbind, subtract, X13:'Any', X31:'Callable'),
      f(rbind, add, X14:'Any', X32:'Callable'),
      f(rbind, subtract, X14:'Any', X33:'Callable'),
      f(fork, connect, X30, X31, X34:'Callable'),
      f(fork, connect, X32, X33, X35:'Callable'),
      f(fork, combine, X34, X35, X36:'Callable'),
      f(mapply, X36, X29:'ContainerContainer', X37:'FrozenSet'),
      f(fill, X8:'Grid', 'EIGHT':'Integer', X37:'Patch', X38:'Grid'),
      f(fill, X38:'Grid', 'TWO':'Integer', X1:'Patch', OUT:'Grid')
    ].
l_solve(b527c5c6, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(matcher, first, 'TWO':'Any', X2:'Callable'),
      f(rbind, sfilter, X2:'Any', X3:'Callable'),
      f(compose, lowermost, X3, X4:'Callable'),
      f(compose, rightmost, X3, X5:'Callable'),
      f(compose, uppermost, X3, X6:'Callable'),
      f(compose, leftmost, X3, X7:'Callable'),
      f(fork, equality, X4, lowermost, X8:'Callable'),
      f(fork, equality, X5, rightmost, X9:'Callable'),
      f(fork, equality, X6, uppermost, X10:'Callable'),
      f(fork, equality, X7, leftmost, X11:'Callable'),
      f(compose, invert, X10, X12:'Callable'),
      f(compose, invert, X11, X13:'Callable'),
      f(fork, add, X12, X8, X14:'Callable'),
      f(fork, add, X13, X9, X15:'Callable'),
      f(fork, astuple, X14, X15, X16:'Callable'),
      f(compose, center, X3, X17:'Callable'),
      f(fork, shoot, X17, X16, X18:'Callable'),
      f(mapply, X18, X1:'ContainerContainer', X19:'FrozenSet'),
      f(fill, IN:'Grid', 'TWO':'Integer', X19:'Patch', X20:'Grid'),
      f(compose, vline, X18, X21:'Callable'),
      f(sfilter, X1:'Container', X21, X22:'Container'),
      f(difference, X1:'FrozenSet', X22:'FrozenSet', X23:'FrozenSet'),
      f(chain, decrement, minimum, shape, X24:'Callable'),
      f(compose, increment, X24, X25:'Callable'),
      f(compose, invert, X24, X26:'Callable'),
      f(rbind, interval, 'ONE':'Any', X27:'Callable'),
      f(fork, X27, X26, X25, X28:'Callable'),
      f(lbind, apply, toivec:'Any', X29:'Callable'),
      f(lbind, apply, tojvec:'Any', X30:'Callable'),
      f(lbind, lbind, shift:'Any', X31:'Callable'),
      f(compose, X31, X18, X32:'Callable'),
      f(compose, X29, X28, X33:'Callable'),
      f(compose, X30, X28, X34:'Callable'),
      f(fork, mapply, X32, X33, X35:'Callable'),
      f(fork, mapply, X32, X34, X36:'Callable'),
      f(mapply, X35, X23:'ContainerContainer', X37:'FrozenSet'),
      f(mapply, X36, X22:'ContainerContainer', X38:'FrozenSet'),
      f(combine, X37:'Container', X38:'Container', X39:'Container'),
      f(underfill, X20:'Grid', 'THREE':'Integer', X39:'Patch', OUT:'Grid')
    ].
l_solve('150deff5', IN, OUT) :-
    [ f(canvas, 'FIVE':'Integer', (2, 2):'Point', X1:'Grid'),
      f(asobject, X1:'Grid', X2:'Object'),
      f(occurrences, IN:'Grid', X2:'Object', X3:'Indices'),
      f(lbind, shift, X2:'Any', X4:'Callable'),
      f(mapply, X4, X3:'ContainerContainer', X5:'FrozenSet'),
      f(fill, IN:'Grid', 'EIGHT':'Integer', X5:'Patch', X6:'Grid'),
      f(canvas, 'FIVE':'Integer', (1, 1):'Point', X7:'Grid'),
      f(astuple, 'TWO':'Integer', 'ONE':'Integer', X8:'Point'),
      f(canvas, 'EIGHT':'Integer', X8:'Point', X9:'Grid'),
      f(vconcat, X9:'Grid', X7:'Grid', X10:'Grid'),
      f(asobject, X10:'Grid', X11:'Object'),
      f(occurrences, X6:'Grid', X11:'Object', X12:'Indices'),
      f(lbind, shift, X11:'Any', X13:'Callable'),
      f(mapply, X13, X12:'ContainerContainer', X14:'FrozenSet'),
      f(fill, X6:'Grid', 'TWO':'Integer', X14:'Patch', X15:'Grid'),
      f(astuple, 'ONE':'Integer', 'THREE':'Integer', X16:'Point'),
      f(canvas, 'FIVE':'Integer', X16:'Point', X17:'Grid'),
      f(asobject, X17:'Grid', X18:'Object'),
      f(occurrences, X15:'Grid', X18:'Object', X19:'Indices'),
      f(lbind, shift, X18:'Any', X20:'Callable'),
      f(mapply, X20, X19:'ContainerContainer', X21:'FrozenSet'),
      f(fill, X15:'Grid', 'TWO':'Integer', X21:'Patch', X22:'Grid'),
      f(hmirror, X10:'Piece', X23:'Piece'),
      f(asobject, X23:'Grid', X24:'Object'),
      f(occurrences, X22:'Grid', X24:'Object', X25:'Indices'),
      f(lbind, shift, X24:'Any', X26:'Callable'),
      f(mapply, X26, X25:'ContainerContainer', X27:'FrozenSet'),
      f(fill, X22:'Grid', 'TWO':'Integer', X27:'Patch', X28:'Grid'),
      f(dmirror, X10:'Piece', X29:'Piece'),
      f(asobject, X29:'Grid', X30:'Object'),
      f(occurrences, X28:'Grid', X30:'Object', X31:'Indices'),
      f(lbind, shift, X30:'Any', X32:'Callable'),
      f(mapply, X32, X31:'ContainerContainer', X33:'FrozenSet'),
      f(fill, X28:'Grid', 'TWO':'Integer', X33:'Patch', X34:'Grid'),
      f(vmirror, X29:'Piece', X35:'Piece'),
      f(asobject, X35:'Grid', X36:'Object'),
      f(occurrences, X34:'Grid', X36:'Object', X37:'Indices'),
      f(lbind, shift, X36:'Any', X38:'Callable'),
      f(mapply, X38, X37:'ContainerContainer', X39:'FrozenSet'),
      f(fill, X34:'Grid', 'TWO':'Integer', X39:'Patch', OUT:'Grid')
    ].
l_solve(b7249182, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(merge, X1:'ContainerContainer', X2:'Container'),
      f(portrait, X2:'Piece', returns=X3),
      f(branch, condition=X3, identity:'Any', dmirror:'Any', X4:'Any'),
      f(X4, IN, X5),
      f(objects,
        X5:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X6:'Objects'),
      f(order, X6:'Container', uppermost, X7:'Tuple'),
      f(first, X7:'Container', X8:'Any'),
      f(last, X7:'Container', X9:'Any'),
      f(color, X8:'Object', X10:'Integer'),
      f(color, X9:'Object', X11:'Integer'),
      f(compose, first, toindices, X12:'Callable'),
      f(X12, X8, X13),
      f(X12, X9, X14),
      f(connect, X13:'Point', X14:'Point', X15:'Indices'),
      f(centerofmass, X15:'Patch', X16:'Point'),
      f(connect, X13:'Point', X16:'Point', X17:'Indices'),
      f(fill, X5:'Grid', X11:'Integer', X15:'Patch', X18:'Grid'),
      f(fill, X18:'Grid', X10:'Integer', X17:'Patch', X19:'Grid'),
      f(add, X16:'Numerical', (1, 0):'Numerical', X20:'Numerical'),
      f(initset, X16:'Any', X21:'FrozenSet'),
      f(insert, X20:'Any', X21:'FrozenSet', X22:'FrozenSet'),
      f(toobject, X22:'Patch', X19:'Grid', X23:'Object'),
      f(astuple, 'ZERO':'Integer', 'NEG_TWO':'Integer', X24:'Point'),
      f(shift, X23:'Patch', (0, 2):'Point', X25:'Patch'),
      f(shift, X23:'Patch', X24:'Point', X26:'Patch'),
      f(combine, X25:'Container', X26:'Container', X27:'Container'),
      f(ulcorner, X27:'Patch', X28:'Point'),
      f(urcorner, X27:'Patch', X29:'Point'),
      f(connect, X28:'Point', X29:'Point', X30:'Indices'),
      f(shift, X30:'Patch', (-1, 0):'Point', X31:'Patch'),
      f(llcorner, X27:'Patch', X32:'Point'),
      f(lrcorner, X27:'Patch', X33:'Point'),
      f(connect, X32:'Point', X33:'Point', X34:'Indices'),
      f(shift, X34:'Patch', (1, 0):'Point', X35:'Patch'),
      f(paint, X19:'Grid', X27:'Object', X36:'Grid'),
      f(fill, X36:'Grid', X10:'Integer', X31:'Patch', X37:'Grid'),
      f(fill, X37:'Grid', X11:'Integer', X35:'Patch', X38:'Grid'),
      f(cover, X38:'Grid', X22:'Patch', X39:'Grid'),
      f(X4, X39, OUT)
    ].
l_solve('9d9215db', IN, OUT) :-
    [ f(rot90, IN:'Grid', X1:'Grid'),
      f(rot180, IN:'Grid', X2:'Grid'),
      f(rot270, IN:'Grid', X3:'Grid'),
      f(initset, IN:'Any', X4:'FrozenSet'),
      f(chain, numcolors, lefthalf, tophalf, X5:'Callable'),
      f(insert, X1:'Any', X4:'FrozenSet', X6:'FrozenSet'),
      f(insert, X2:'Any', X6:'FrozenSet', X7:'FrozenSet'),
      f(insert, X3:'Any', X7:'FrozenSet', X8:'FrozenSet'),
      f(argmax, X8:'Container', X5, X9:'Any'),
      f(vmirror, X9:'Piece', X10:'Piece'),
      f(papply, pair, X9:'Tuple', X10:'Tuple', X11:'Tuple'),
      f(lbind, apply, maximum:'Any', X12:'Callable'),
      f(apply, X12, X11:'Container', X13:'Container'),
      f(partition, X13:'Grid', X14:'Objects'),
      f(sizefilter, X14:'Container', 'FOUR':'Integer', X15:'FrozenSet'),
      f(apply, llcorner, X15:'Container', X16:'Container'),
      f(apply, lrcorner, X15:'Container', X17:'Container'),
      f(combine, X16:'Container', X17:'Container', X18:'Container'),
      f(cover, X13:'Grid', X18:'Patch', X19:'Grid'),
      f(tojvec, 'NEG_TWO':'Integer', X20:'Point'),
      f(rbind, add, (0, 2):'Any', X21:'Callable'),
      f(rbind, add, X20:'Any', X22:'Callable'),
      f(compose, X21, ulcorner, X23:'Callable'),
      f(compose, X22, urcorner, X24:'Callable'),
      f(fork, connect, X23, X24, X25:'Callable'),
      f(compose, even, last, X26:'Callable'),
      f(rbind, sfilter, X26:'Any', X27:'Callable'),
      f(chain, normalize, X27, X25, X28:'Callable'),
      f(fork, shift, X28, X23, X29:'Callable'),
      f(fork, recolor, color, X29, X30:'Callable'),
      f(mapply, X30, X15:'ContainerContainer', X31:'FrozenSet'),
      f(paint, X19:'Grid', X31:'Object', X32:'Grid'),
      f(rot90, X32:'Grid', X33:'Grid'),
      f(rot180, X32:'Grid', X34:'Grid'),
      f(rot270, X32:'Grid', X35:'Grid'),
      f(papply, pair, X32:'Tuple', X33:'Tuple', X36:'Tuple'),
      f(apply, X12, X36:'Container', X37:'Container'),
      f(papply, pair, X37:'Tuple', X34:'Tuple', X38:'Tuple'),
      f(apply, X12, X38:'Container', X39:'Container'),
      f(papply, pair, X39:'Tuple', X35:'Tuple', X40:'Tuple'),
      f(apply, X12, X40:'Container', OUT:'Container')
    ].
l_solve('6855a6e4', IN, OUT) :-
    [ f(fgpartition, IN:'Grid', X1:'Objects'),
      f(rot90, IN:'Grid', X2:'Grid'),
      f(colorfilter, X1:'Objects', 'TWO':'Integer', X3:'Objects'),
      f(first, X3:'Container', X4:'Any'),
      f(portrait, X4:'Piece', returns=X5),
      f(branch, condition=X5, IN:'Any', X2:'Any', X6:'Any'),
      f(objects,
        X6:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X7:'Objects'),
      f(colorfilter, X7:'Objects', 'FIVE':'Integer', X8:'Objects'),
      f(apply, center, X8:'Container', X9:'Container'),
      f(valmin, X9:'Container', first, X10:'Integer'),
      f(compose, first, center, X11:'Callable'),
      f(matcher, X11, X10:'Any', X12:'Callable'),
      f(compose, flip, X12, X13:'Callable'),
      f(extract, X8:'Container', X12, X14:'Any'),
      f(extract, X8:'Container', X13, X15:'Any'),
      f(ulcorner, X14:'Patch', X16:'Point'),
      f(ulcorner, X15:'Patch', X17:'Point'),
      f(subgrid, X14:'Patch', X6:'Grid', X18:'Grid'),
      f(subgrid, X15:'Patch', X6:'Grid', X19:'Grid'),
      f(hmirror, X18:'Piece', X20:'Piece'),
      f(hmirror, X19:'Piece', X21:'Piece'),
      f(ofcolor, X20:'Grid', 'FIVE':'Integer', X22:'Indices'),
      f(recolor, 'FIVE':'Integer', X22:'Patch', X23:'Object'),
      f(ofcolor, X21:'Grid', 'FIVE':'Integer', X24:'Indices'),
      f(recolor, 'FIVE':'Integer', X24:'Patch', X25:'Object'),
      f(height, X23:'Piece', X26:'Integer'),
      f(height, X25:'Piece', X27:'Integer'),
      f(add, 'THREE':'Numerical', X26:'Numerical', X28:'Numerical'),
      f(add, 'THREE':'Numerical', X27:'Numerical', X29:'Numerical'),
      f(toivec, X28:'Integer', X30:'Point'),
      f(toivec, X29:'Integer', X31:'Point'),
      f(add, X16:'Numerical', X30:'Numerical', X32:'Numerical'),
      f(subtract, X17:'Numerical', X31:'Numerical', X33:'Numerical'),
      f(shift, X23:'Patch', X32:'Point', X34:'Patch'),
      f(shift, X25:'Patch', X33:'Point', X35:'Patch'),
      f(merge, X8:'ContainerContainer', X36:'Container'),
      f(cover, X6:'Grid', X36:'Patch', X37:'Grid'),
      f(paint, X37:'Grid', X34:'Object', X38:'Grid'),
      f(paint, X38:'Grid', X35:'Object', X39:'Grid'),
      f(rot270, X39:'Grid', X40:'Grid'),
      f(branch, condition=X5, X39:'Any', X40:'Any', OUT:'Any')
    ].
l_solve('264363fd', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(argmin, X1:'Container', size, X2:'Any'),
      f(normalize, X2:'Patch', X3:'Patch'),
      f(height, X2:'Piece', X4:'Integer'),
      f(width, X2:'Piece', X5:'Integer'),
      f(equality, X4:'Any', 'FIVE':'Any', returns=X6),
      f(equality, X5:'Any', 'FIVE':'Any', returns=X7),
      f(astuple, X6:'Integer', X7:'Integer', X8:'Point'),
      f(add, (1, 1):'Numerical', X8:'Numerical', X9:'Numerical'),
      f(invert, X9:'Numerical', X10:'Numerical'),
      f(center, X2:'Patch', X11:'Point'),
      f(index, IN:'Grid', X11:'Point', X12:'Integer'),
      f(branch, condition=X6, (-1, 0):'Any', (0, 1):'Any', X13:'Any'),
      f(add, X13:'Numerical', X11:'Numerical', X14:'Numerical'),
      f(index, IN:'Grid', X14:'Point', X15:'Integer'),
      f(astuple, X12:'Integer', (0, 0):'Integer', X16:'Point'),
      f(initset, X16:'Any', X17:'FrozenSet'),
      f(cover, IN:'Grid', X2:'Patch', X18:'Grid'),
      f(mostcolor, X18:'Element', X19:'Integer'),
      f(ofcolor, X18:'Grid', X19:'Integer', X20:'Indices'),
      f(occurrences, X18:'Grid', X17:'Object', X21:'Indices'),
      f(objects,
        X18:'Grid',
        univalued=false,
        diagonal=false,
        without_bg=true,
        X22:'Objects'),
      f(rbind, occurrences, X17:'Any', X23:'Callable'),
      f(rbind, subgrid, X18:'Any', X24:'Callable'),
      f(compose, X23, X24, X25:'Callable'),
      f(lbind, mapply, vfrontier:'Any', X26:'Callable'),
      f(lbind, mapply, hfrontier:'Any', X27:'Callable'),
      f(compose, X26, X25, X28:'Callable'),
      f(compose, X27, X25, X29:'Callable'),
      f(branch, condition=X6, X28:'Any', X29:'Any', X30:'Any'),
      f(branch, condition=X7, X29:'Any', X28:'Any', X31:'Any'),
      f(fork, combine, X30, X31, X32:'Callable'),
      f(lbind, recolor, X15:'Any', X33:'Callable'),
      f(compose, X33, X32, X34:'Callable'),
      f(fork, paint, X24, X34, X35:'Callable'),
      f(compose, asobject, X35, X36:'Callable'),
      f(fork, shift, X36, ulcorner, X37:'Callable'),
      f(mapply, X37, X22:'ContainerContainer', X38:'FrozenSet'),
      f(paint, X18:'Grid', X38:'Object', X39:'Grid'),
      f(shift, X3:'Patch', X10:'Point', X40:'Patch'),
      f(lbind, shift, X40:'Any', X41:'Callable'),
      f(mapply, X41, X21:'ContainerContainer', X42:'FrozenSet'),
      f(paint, X39:'Grid', X42:'Object', X43:'Grid'),
      f(fill, X43:'Grid', X19:'Integer', X20:'Patch', OUT:'Grid')
    ].
l_solve('7df24a62', IN, OUT) :-
    [ f(height, IN:'Piece', X1:'Integer'),
      f(width, IN:'Piece', X2:'Integer'),
      f(ofcolor, IN:'Grid', 'ONE':'Integer', X3:'Indices'),
      f(ofcolor, IN:'Grid', 'FOUR':'Integer', X4:'Indices'),
      f(ulcorner, X3:'Patch', X5:'Point'),
      f(subgrid, X3:'Patch', IN:'Grid', X6:'Grid'),
      f(rot90, X6:'Grid', X7:'Grid'),
      f(rot180, X6:'Grid', X8:'Grid'),
      f(rot270, X6:'Grid', X9:'Grid'),
      f(matcher, size, 'ZERO':'Any', X10:'Callable'),
      f(rbind, ofcolor, 'ONE':'Any', X11:'Callable'),
      f(compose, normalize, X11, X12:'Callable'),
      f(rbind, ofcolor, 'FOUR':'Any', X13:'Callable'),
      f(rbind, shift, X5:'Any', X14:'Callable'),
      f(compose, X14, X13, X15:'Callable'),
      f(lbind, subtract, X1:'Any', X16:'Callable'),
      f(chain, increment, X16, height, X17:'Callable'),
      f(lbind, subtract, X2:'Any', X18:'Callable'),
      f(chain, increment, X18, width, X19:'Callable'),
      f(rbind, interval, 'ONE':'Any', X20:'Callable'),
      f(lbind, X20, 'ZERO':'Any', X21:'Callable'),
      f(compose, X21, X17, X22:'Callable'),
      f(compose, X21, X19, X23:'Callable'),
      f(fork, product, X22, X23, X24:'Callable'),
      f(rbind, shift, (-1, -1):'Any', X25:'Callable'),
      f(lbind, lbind, shift:'Any', X26:'Callable'),
      f(chain, X26, X25, X12, X27:'Callable'),
      f(astuple, X6:'Integer', X7:'Integer', X28:'Point'),
      f(astuple, X8:'Integer', X9:'Integer', X29:'Point'),
      f(combine, X28:'Container', X29:'Container', X30:'Container'),
      f(apply, X15, X30:'Container', X31:'Container'),
      f(lbind, difference, X4:'Any', X32:'Callable'),
      f(apply, X32, X31:'Container', X33:'Container'),
      f(apply, normalize, X31:'Container', X34:'Container'),
      f(apply, X24, X34:'Container', X35:'Container'),
      f(lbind, rbind, difference:'Any', X36:'Callable'),
      f(apply, X26, X34:'Container', X37:'Container'),
      f(apply, X36, X33:'Container', X38:'Container'),
      f(papply, compose, X38:'Tuple', X37:'Tuple', X39:'Tuple'),
      f(lbind, compose, X10:'Any', X40:'Callable'),
      f(apply, X40, X39:'Container', X41:'Container'),
      f(papply, sfilter, X35:'Tuple', X41:'Tuple', X42:'Tuple'),
      f(apply, X27, X30:'Container', X43:'Container'),
      f(mpapply, mapply, X43:'Tuple', X42:'Tuple', X44:'Tuple'),
      f(fill, IN:'Grid', 'ONE':'Integer', X44:'Patch', OUT:'Grid')
    ].
l_solve(f15e1fac, IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'TWO':'Integer', X1:'Indices'),
      f(portrait, X1:'Piece', returns=X2),
      f(branch, condition=X2, identity:'Any', dmirror:'Any', X3:'Any'),
      f(X3, IN, X4),
      f(leftmost, X1:'Patch', X5:'Integer'),
      f(equality, X5:'Any', 'ZERO':'Any', returns=X6),
      f(branch, condition=X6, identity:'Any', vmirror:'Any', X7:'Any'),
      f(X7, X4, X8),
      f(ofcolor, X8:'Grid', 'EIGHT':'Integer', X9:'Indices'),
      f(uppermost, X9:'Patch', X10:'Integer'),
      f(equality, X10:'Any', 'ZERO':'Any', returns=X11),
      f(branch, condition=X11, identity:'Any', hmirror:'Any', X12:'Any'),
      f(X12, X8, X13),
      f(ofcolor, X13:'Grid', 'EIGHT':'Integer', X14:'Indices'),
      f(ofcolor, X13:'Grid', 'TWO':'Integer', X15:'Indices'),
      f(rbind, shoot, (1, 0):'Any', X16:'Callable'),
      f(mapply, X16, X14:'ContainerContainer', X17:'FrozenSet'),
      f(height, X13:'Piece', X18:'Integer'),
      f(apply, first, X15:'Container', X19:'Container'),
      f(insert, 'ZERO':'Any', X19:'FrozenSet', X20:'FrozenSet'),
      f(insert, X18:'Any', X19:'FrozenSet', X21:'FrozenSet'),
      f(apply, decrement, X21:'Container', X22:'Container'),
      f(order, X20:'Container', identity, X23:'Tuple'),
      f(order, X22:'Container', identity, X24:'Tuple'),
      f(size, X15:'Container', X25:'Integer'),
      f(increment, X25:'Numerical', X26:'Numerical'),
      f(interval, 'ZERO':'Integer', X26:'Integer', 'ONE':'Integer', X27:'Tuple'),
      f(apply, tojvec, X27:'Container', X28:'Container'),
      f(pair, X23:'Tuple', X24:'Tuple', X29:'TupleTuple'),
      f(lbind, sfilter, X17:'Any', X30:'Callable'),
      f(compose, first, last, X31:'Callable'),
      f(chain, decrement, first, first, X32:'Callable'),
      f(fork, greater, X31, X32, X33:'Callable'),
      f(chain, increment, last, first, X34:'Callable'),
      f(fork, greater, X34, X31, X35:'Callable'),
      f(fork, both, X33, X35, X36:'Callable'),
      f(lbind, lbind, astuple:'Any', X37:'Callable'),
      f(lbind, compose, X36:'Any', X38:'Callable'),
      f(chain, X30, X38, X37, X39:'Callable'),
      f(apply, X39, X29:'Container', X40:'Container'),
      f(papply, shift, X40:'Tuple', X28:'Tuple', X41:'Tuple'),
      f(merge, X41:'ContainerContainer', X42:'Container'),
      f(fill, X13:'Grid', 'EIGHT':'Integer', X42:'Patch', X43:'Grid'),
      f(chain, X3, X7, X12, X44:'Callable'),
      f(X44, X43, OUT)
    ].
l_solve('234bbc79', IN, OUT) :-
    [ f(rbind, objects, true:'Any', X1:'Callable'),
      f(rbind, X1, false:'Any', X2:'Callable'),
      f(rbind, X2, false:'Any', X3:'Callable'),
      f(rbind, argmin, leftmost:'Any', X4:'Callable'),
      f(compose, X4, X3, X5:'Callable'),
      f(fork, remove, X5, X3, X6:'Callable'),
      f(compose, X4, X6, X7:'Callable'),
      f(compose, last, last, X8:'Callable'),
      f(matcher, first, 'FIVE':'Any', X9:'Callable'),
      f(rbind, sfilter, X9:'Any', X10:'Callable'),
      f(fork, difference, identity, X10, X11:'Callable'),
      f(rbind, argmin, X8:'Any', X12:'Callable'),
      f(compose, X12, X10, X13:'Callable'),
      f(compose, last, X13, X14:'Callable'),
      f(rbind, add, (0, 1):'Any', X15:'Callable'),
      f(compose, X14, X7, X16:'Callable'),
      f(compose, X14, X5, X17:'Callable'),
      f(fork, subtract, X16, X17, X18:'Callable'),
      f(compose, invert, X18, X19:'Callable'),
      f(compose, X15, X19, X20:'Callable'),
      f(compose, mostcolor, X11, X21:'Callable'),
      f(fork, astuple, X21, X14, X22:'Callable'),
      f(fork, remove, X13, identity, X23:'Callable'),
      f(fork, insert, X22, X23, X24:'Callable'),
      f(compose, X24, X7, X25:'Callable'),
      f(fork, cover, identity, X25, X26:'Callable'),
      f(fork, shift, X25, X20, X27:'Callable'),
      f(fork, paint, X26, X27, X28:'Callable'),
      f(rbind, argmax, X8:'Any', X29:'Callable'),
      f(chain, first, X29, X11, X30:'Callable'),
      f(fork, recolor, X30, X10, X31:'Callable'),
      f(fork, combine, X11, X31, X32:'Callable'),
      f(compose, X32, X5, X33:'Callable'),
      f(fork, paint, X28, X33, X34:'Callable'),
      f(X34, IN, X35),
      f(X34, X35, X36),
      f(palette, X36:'Element', X37:'IntegerSet'),
      f(contained, 'FIVE':'Any', X37:'Container', returns=X38),
      f(branch, condition=X38, X34:'Any', identity:'Any', X39:'Any'),
      f(X39, X36, X40),
      f(X3, X40, X41),
      f(merge, X41:'ContainerContainer', X42:'Container'),
      f(width, X42:'Piece', X43:'Integer'),
      f(astuple, 'THREE':'Integer', X43:'Integer', X44:'Point'),
      f(crop, X40:'Grid', (0, 0):'Point', X44:'Point', OUT:'Grid')
    ].
l_solve('22233c11', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X2:'Objects'),
      f(first, X1:'Container', X3:'Any'),
      f(recolor, 'EIGHT':'Integer', X3:'Patch', X4:'Object'),
      f(normalize, X4:'Patch', X5:'Patch'),
      f(totuple, X2:'FrozenSet', X6:'Tuple'),
      f(apply, width, X6:'Container', X7:'Container'),
      f(lbind, index, IN:'Any', X8:'Callable'),
      f(rbind, equality, 'ZERO':'Any', X9:'Callable'),
      f(chain, flip, X9, X8, X10:'Callable'),
      f(apply, urcorner, X6:'Container', X11:'Container'),
      f(apply, X10, X11:'Container', X12:'Container'),
      f(first, X7:'Container', X13:'Any'),
      f(halve, X13:'Numerical', X14:'Numerical'),
      f(pair, X6:'Tuple', X12:'Tuple', X15:'TupleTuple'),
      f(sfilter, X15:'Container', last, X16:'Container'),
      f(apply, first, X16:'Container', X17:'Container'),
      f(apply, flip, X12:'Container', X18:'Container'),
      f(pair, X6:'Tuple', X18:'Tuple', X19:'TupleTuple'),
      f(sfilter, X19:'Container', last, X20:'Container'),
      f(apply, first, X20:'Container', X21:'Container'),
      f(apply, urcorner, X21:'Container', X22:'Container'),
      f(invert, X14:'Numerical', X23:'Numerical'),
      f(astuple, X23:'Integer', 'ONE':'Integer', X24:'Point'),
      f(lbind, add, X24:'Any', X25:'Callable'),
      f(apply, X25, X22:'Container', X26:'Container'),
      f(lbind, shift, X5:'Any', X27:'Callable'),
      f(mapply, X27, X26:'ContainerContainer', X28:'FrozenSet'),
      f(apply, llcorner, X21:'Container', X29:'Container'),
      f(astuple, 'ONE':'Integer', X23:'Integer', X30:'Point'),
      f(lbind, add, X30:'Any', X31:'Callable'),
      f(apply, X31, X29:'Container', X32:'Container'),
      f(mapply, X27, X32:'ContainerContainer', X33:'FrozenSet'),
      f(apply, ulcorner, X17:'Container', X34:'Container'),
      f(astuple, X23:'Integer', X23:'Integer', X35:'Point'),
      f(lbind, add, X35:'Any', X36:'Callable'),
      f(apply, X36, X34:'Container', X37:'Container'),
      f(mapply, X27, X37:'ContainerContainer', X38:'FrozenSet'),
      f(apply, lrcorner, X17:'Container', X39:'Container'),
      f(lbind, add, (1, 1):'Any', X40:'Callable'),
      f(apply, X40, X39:'Container', X41:'Container'),
      f(mapply, X27, X41:'ContainerContainer', X42:'FrozenSet'),
      f(paint, IN:'Grid', X28:'Object', X43:'Grid'),
      f(paint, X43:'Grid', X33:'Object', X44:'Grid'),
      f(paint, X44:'Grid', X38:'Object', X45:'Grid'),
      f(paint, X45:'Grid', X42:'Object', OUT:'Grid')
    ].
l_solve('2dd70a9a', IN, OUT) :-
    [ f(ofcolor, IN:'Grid', 'TWO':'Integer', X1:'Indices'),
      f(ofcolor, IN:'Grid', 'THREE':'Integer', X2:'Indices'),
      f(vline, X1:'Patch', returns=X3),
      f(vline, X2:'Patch', returns=X4),
      f(center, X1:'Patch', X5:'Point'),
      f(branch, condition=X4, uppermost:'Any', rightmost:'Any', X6:'Any'),
      f(X6, X1, X7),
      f(X6, X2, X8),
      f(greater, X7:'Integer', X8:'Integer', returns=X9),
      f(both, a=X4, b=X9, returns=X10),
      f(branch, condition=X10, lowermost:'Any', uppermost:'Any', X11:'Any'),
      f(X11, X2, X12),
      f(branch, condition=X4, leftmost:'Any', rightmost:'Any', X13:'Any'),
      f(X13, X2, X14),
      f(astuple, X12:'Integer', X14:'Integer', X15:'Point'),
      f(other, X2:'Container', X15:'Any', X16:'Any'),
      f(subtract, X15:'Numerical', X16:'Numerical', X17:'Numerical'),
      f(shoot, X15:'Point', X17:'Point', X18:'Indices'),
      f(underfill, IN:'Grid', 'ONE':'Integer', X18:'Patch', X19:'Grid'),
      f(objects,
        X19:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=false,
        X20:'Objects'),
      f(colorfilter, X20:'Objects', 'ONE':'Integer', X21:'Objects'),
      f(rbind, adjacent, X2:'Any', X22:'Callable'),
      f(sfilter, X21:'Container', X22, X23:'Container'),
      f(difference,
        X21:'FrozenSet',
        X23:'FrozenSet',
        X24:'FrozenSet'),
      f(merge, X24:'ContainerContainer', X25:'Container'),
      f(cover, X19:'Grid', X25:'Patch', X26:'Grid'),
      f(shoot, X5:'Point', (1, 0):'Point', X27:'Indices'),
      f(shoot, X5:'Point', (-1, 0):'Point', X28:'Indices'),
      f(shoot, X5:'Point', (0, -1):'Point', X29:'Indices'),
      f(shoot, X5:'Point', (0, 1):'Point', X30:'Indices'),
      f(combine, X27:'Container', X28:'Container', X31:'Container'),
      f(combine, X29:'Container', X30:'Container', X32:'Container'),
      f(branch, condition=X3, X31:'Any', X32:'Any', X33:'Any'),
      f(ofcolor, X26:'Grid', 'ONE':'Integer', X34:'Indices'),
      f(initset, X15:'Any', X35:'FrozenSet'),
      f(rbind, manhattan, X35:'Any', X36:'Callable'),
      f(compose, X36, initset, X37:'Callable'),
      f(argmax, X34:'Container', X37, X38:'Any'),
      f(initset, X38:'Any', X39:'FrozenSet'),
      f(gravitate, X39:'Patch', X33:'Patch', X40:'Point'),
      f(crement, X40:'Numerical', X41:'Numerical'),
      f(add, X38:'Numerical', X41:'Numerical', X42:'Numerical'),
      f(connect, X38:'Point', X42:'Point', X43:'Indices'),
      f(fill, X26:'Grid', 'ONE':'Integer', X43:'Patch', X44:'Grid'),
      f(connect, X42:'Point', X5:'Point', X45:'Indices'),
      f(underfill, X44:'Grid', 'ONE':'Integer', X45:'Patch', X46:'Grid'),
      f(replace, X46:'Grid', 'ONE':'Integer', 'THREE':'Integer', OUT:'Grid')
    ].
l_solve(a64e4611, IN, OUT) :-
    [ f(asindices, IN:'Grid', X1:'Indices'),
      f(fork, product, identity, identity, X2:'Callable'),
      f(lbind, canvas, 'ZERO':'Any', X3:'Callable'),
      f(compose, asobject, X3, X4:'Callable'),
      f(fork, multiply, first, last, X5:'Callable'),
      f(compose, positive, size, X6:'Callable'),
      f(lbind, lbind, shift:'Any', X7:'Callable'),
      f(rbind, fork, X5:'Any', X8:'Callable'),
      f(lbind, X8, multiply:'Any', X9:'Callable'),
      f(lbind, chain, X6:'Any', X10:'Callable'),
      f(rbind, X10, X4:'Any', X11:'Callable'),
      f(lbind, lbind, occurrences:'Any', X12:'Callable'),
      f(chain, X9, X11, X12, X13:'Callable'),
      f(compose, X2, first, X14:'Callable'),
      f(compose, X13, last, X15:'Callable'),
      f(fork, argmax, X14, X15, X16:'Callable'),
      f(chain, X7, X4, X16, X17:'Callable'),
      f(compose, X4, X16, X18:'Callable'),
      f(fork, occurrences, last, X18, X19:'Callable'),
      f(fork, mapply, X17, X19, X20:'Callable'),
      f(multiply, 'TWO':'Numerical', 'SIX':'Numerical', X21:'Numerical'),
      f(interval, 'THREE':'Integer', X21:'Integer', 'ONE':'Integer', X22:'Tuple'),
      f(astuple, X22:'Integer', IN:'Integer', X23:'Point'),
      f(X20, X23, X24),
      f(fill, IN:'Grid', 'THREE':'Integer', X24:'Patch', X25:'Grid'),
      f(interval, 'THREE':'Integer', 'TEN':'Integer', 'ONE':'Integer', X26:'Tuple'),
      f(astuple, X26:'Integer', X25:'Integer', X27:'Point'),
      f(X20, X27, X28),
      f(fill, X25:'Grid', 'THREE':'Integer', X28:'Patch', X29:'Grid'),
      f(astuple, X26:'Integer', X29:'Integer', X30:'Point'),
      f(X20, X30, X31),
      f(fill, X29:'Grid', 'THREE':'Integer', X31:'Patch', X32:'Grid'),
      f(rbind, toobject, X32:'Any', X33:'Callable'),
      f(rbind, colorcount, 'THREE':'Any', X34:'Callable'),
      f(chain, X34, X33, neighbors, X35:'Callable'),
      f(matcher, X35, 'EIGHT':'Any', X36:'Callable'),
      f(sfilter, X1:'Container', X36, X37:'Container'),
      f(fill, IN:'Grid', 'THREE':'Integer', X37:'Patch', X38:'Grid'),
      f(ofcolor, X38:'Grid', 'ZERO':'Integer', X39:'Indices'),
      f(rbind, bordering, X38:'Any', X40:'Callable'),
      f(compose, X40, initset, X41:'Callable'),
      f(lbind, contained, 'THREE':'Any', X42:'Callable'),
      f(rbind, toobject, X38:'Any', X43:'Callable'),
      f(chain, X42, palette, X43, X44:'Callable'),
      f(compose, X44, dneighbors, X45:'Callable'),
      f(fork, both, X45, X41, X46:'Callable'),
      f(sfilter, X39:'Container', X46, X47:'Container'),
      f(fill, X38:'Grid', 'THREE':'Integer', X47:'Patch', OUT:'Grid')
    ].
l_solve('7837ac64', IN, OUT) :-
    [ f(compress, IN:'Grid', X1:'Grid'),
      f(lbind, colorcount, X1:'Any', X2:'Callable'),
      f(palette, X1:'Element', X3:'IntegerSet'),
      f(order, X3:'Container', X2, X4:'Tuple'),
      f(remove, 'ZERO':'Any', X4:'Container', X5:'Container'),
      f(last, X5:'Container', X6:'Any'),
      f(replace, X1:'Grid', X6:'Integer', 'ZERO':'Integer', X7:'Grid'),
      f(objects,
        X7:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X8:'Objects'),
      f(merge, X8:'ContainerContainer', X9:'Container'),
      f(subgrid, X9:'Patch', X7:'Grid', X10:'Grid'),
      f(index, X10:'Grid', (0, 0):'Point', X11:'Integer'),
      f(vmirror, X10:'Piece', X12:'Piece'),
      f(index, X12:'Grid', (0, 0):'Point', X13:'Integer'),
      f(hmirror, X10:'Piece', X14:'Piece'),
      f(index, X14:'Grid', (0, 0):'Point', X15:'Integer'),
      f(vmirror, X14:'Piece', X16:'Piece'),
      f(index, X16:'Grid', (0, 0):'Point', X17:'Integer'),
      f(width, X10:'Piece', X18:'Integer'),
      f(subtract, X18:'Numerical', 'FOUR':'Numerical', X19:'Numerical'),
      f(divide, X19:'Numerical', 'THREE':'Numerical', X20:'Numerical'),
      f(increment, X20:'Numerical', X21:'Numerical'),
      f(tojvec, X21:'Integer', X22:'Point'),
      f(toivec, X21:'Integer', X23:'Point'),
      f(index, X10:'Grid', X22:'Point', X24:'Integer'),
      f(index, X12:'Grid', X22:'Point', X25:'Integer'),
      f(index, X14:'Grid', X22:'Point', X26:'Integer'),
      f(index, X16:'Grid', X22:'Point', X27:'Integer'),
      f(index, X10:'Grid', X23:'Point', X28:'Integer'),
      f(index, X14:'Grid', X23:'Point', X29:'Integer'),
      f(index, X12:'Grid', X23:'Point', X30:'Integer'),
      f(equality, X24:'Any', X25:'Any', returns=X31),
      f(equality, X26:'Any', X27:'Any', returns=X32),
      f(equality, X28:'Any', X29:'Any', returns=X33),
      f(equality, X29:'Any', X30:'Any', returns=X34),
      f(branch, condition=X31, X24:'Any', 'ZERO':'Any', X35:'Any'),
      f(branch, condition=X32, X26:'Any', 'ZERO':'Any', X36:'Any'),
      f(branch, condition=X33, X28:'Any', 'ZERO':'Any', X37:'Any'),
      f(branch, condition=X34, X29:'Any', 'ZERO':'Any', X38:'Any'),
      f(astuple, X11:'Integer', X35:'Integer', X39:'Point'),
      f(repeat, X13:'Any', 'ONE':'Integer', X40:'Tuple'),
      f(combine, X39:'Container', X40:'Container', X41:'Container'),
      f(astuple, X37:'Integer', 'ZERO':'Integer', X42:'Point'),
      f(repeat, X38:'Any', 'ONE':'Integer', X43:'Tuple'),
      f(combine, X42:'Container', X43:'Container', X44:'Container'),
      f(astuple, X15:'Integer', X36:'Integer', X45:'Point'),
      f(repeat, X17:'Any', 'ONE':'Integer', X46:'Tuple'),
      f(combine, X45:'Container', X46:'Container', X47:'Container'),
      f(astuple, X41:'Integer', X44:'Integer', X48:'Point'),
      f(repeat, X47:'Any', 'ONE':'Integer', X49:'Tuple'),
      f(vconcat, X48:'Grid', X49:'Grid', OUT:'Grid')
    ].
l_solve(a8c38be5, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=true,
        diagonal=false,
        without_bg=true,
        X1:'Objects'),
      f(colorfilter, X1:'Objects', 'FIVE':'Integer', X2:'Objects'),
      f(argmax, X2:'Container', size, X3:'Any'),
      f(subgrid, X3:'Patch', IN:'Grid', X4:'Grid'),
      f(difference, X1:'FrozenSet', X2:'FrozenSet', X5:'FrozenSet'),
      f(fork, equality, height, width, X6:'Callable'),
      f(fork, greater, width, height, X7:'Callable'),
      f(sfilter, X5:'Container', X6, X8:'Container'),
      f(sfilter, X5:'Container', portrait, X9:'Container'),
      f(sfilter, X5:'Container', X7, X10:'Container'),
      f(rbind, subgrid, IN:'Any', X11:'Callable'),
      f(chain, center, delta, normalize, X12:'Callable'),
      f(order, X8:'Container', X12, X13:'Tuple'),
      f(apply, X11, X13:'Container', X14:'Container'),
      f(order, X9:'Container', X12, X15:'Tuple'),
      f(apply, X11, X15:'Container', X16:'Container'),
      f(order, X10:'Container', X12, X17:'Tuple'),
      f(apply, X11, X17:'Container', X18:'Container'),
      f(first, X14:'Container', X19:'Any'),
      f(remove, X19:'Any', X14:'Container', X20:'Container'),
      f(first, X20:'Container', X21:'Any'),
      f(remove, X21:'Any', X20:'Container', X22:'Container'),
      f(first, X22:'Container', X23:'Any'),
      f(last, X14:'Container', X24:'Any'),
      f(last, X16:'Container', X25:'Any'),
      f(first, X16:'Container', X26:'Any'),
      f(last, X18:'Container', X27:'Any'),
      f(first, X18:'Container', X28:'Any'),
      f(astuple, 'ONE':'Integer', 'TWO':'Integer', X29:'Point'),
      f(astuple, 'ONE':'Integer', 'THREE':'Integer', X30:'Point'),
      f(astuple, 'NINE':'Integer', 'ONE':'Integer', X31:'Point'),
      f(canvas, 'FIVE':'Integer', X29:'Point', X32:'Grid'),
      f(canvas, 'FIVE':'Integer', X30:'Point', X33:'Grid'),
      f(canvas, 'FIVE':'Integer', X31:'Point', X34:'Grid'),
      f(vconcat, X24:'Grid', X32:'Grid', X35:'Grid'),
      f(vconcat, X35:'Grid', X25:'Grid', X36:'Grid'),
      f(vconcat, X36:'Grid', X32:'Grid', X37:'Grid'),
      f(vconcat, X37:'Grid', X21:'Grid', X38:'Grid'),
      f(vconcat, X27:'Grid', X33:'Grid', X39:'Grid'),
      f(vconcat, X39:'Grid', X4:'Grid', X40:'Grid'),
      f(vconcat, X40:'Grid', X33:'Grid', X41:'Grid'),
      f(vconcat, X41:'Grid', X28:'Grid', X42:'Grid'),
      f(vconcat, X23:'Grid', X32:'Grid', X43:'Grid'),
      f(vconcat, X43:'Grid', X26:'Grid', X44:'Grid'),
      f(vconcat, X44:'Grid', X32:'Grid', X45:'Grid'),
      f(vconcat, X45:'Grid', X19:'Grid', X46:'Grid'),
      f(hconcat, X38:'Grid', X34:'Grid', X47:'Grid'),
      f(hconcat, X47:'Grid', X42:'Grid', X48:'Grid'),
      f(hconcat, X48:'Grid', X34:'Grid', X49:'Grid'),
      f(hconcat, X49:'Grid', X46:'Grid', OUT:'Grid')
    ].
l_solve(b775ac94, IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(lbind, rbind, equality:'Any', X2:'Callable'),
      f(rbind, compose, first:'Any', X3:'Callable'),
      f(chain, X3, X2, mostcolor, X4:'Callable'),
      f(fork, sfilter, identity, X4, X5:'Callable'),
      f(fork, difference, identity, X5, X6:'Callable'),
      f(lbind, rbind, adjacent:'Any', X7:'Callable'),
      f(rbind, compose, initset:'Any', X8:'Callable'),
      f(chain, X8, X7, X6, X9:'Callable'),
      f(fork, extract, X5, X9, X10:'Callable'),
      f(fork, insert, X10, X6, X11:'Callable'),
      f(lbind, recolor, 'ZERO':'Any', X12:'Callable'),
      f(chain, X12, delta, X11, X13:'Callable'),
      f(fork, combine, X11, X13, X14:'Callable'),
      f(fork, position, X5, X6, X15:'Callable'),
      f(chain, toivec, first, X15, X16:'Callable'),
      f(chain, tojvec, last, X15, X17:'Callable'),
      f(fork, multiply, shape, X16, X18:'Callable'),
      f(fork, multiply, shape, X17, X19:'Callable'),
      f(fork, multiply, shape, X15, X20:'Callable'),
      f(fork, shift, hmirror, X18, X21:'Callable'),
      f(fork, shift, vmirror, X19, X22:'Callable'),
      f(compose, hmirror, vmirror, X23:'Callable'),
      f(fork, shift, X23, X20, X24:'Callable'),
      f(lbind, compose, X5:'Any', X25:'Callable'),
      f(X25, X21, X26),
      f(X25, X22, X27),
      f(X25, X24, X28),
      f(compose, crement, invert, X29:'Callable'),
      f(lbind, compose, X29:'Any', X30:'Callable'),
      f(X30, X16, X31),
      f(X30, X17, X32),
      f(X30, X15, X33),
      f(fork, shift, X26, X31, X34:'Callable'),
      f(fork, shift, X27, X32, X35:'Callable'),
      f(fork, shift, X28, X33, X36:'Callable'),
      f(lbind, index, IN:'Any', X37:'Callable'),
      f(lbind, compose, toindices:'Any', X38:'Callable'),
      f(X38, X14, X39),
      f(X38, X34, X40),
      f(X38, X35, X41),
      f(X38, X36, X42),
      f(fork, intersection, X39, X40, X43:'Callable'),
      f(fork, intersection, X39, X41, X44:'Callable'),
      f(fork, intersection, X39, X42, X45:'Callable'),
      f(chain, X37, first, X43, X46:'Callable'),
      f(chain, X37, first, X44, X47:'Callable'),
      f(chain, X37, first, X45, X48:'Callable'),
      f(fork, recolor, X46, X34, X49:'Callable'),
      f(fork, recolor, X47, X35, X50:'Callable'),
      f(fork, recolor, X48, X36, X51:'Callable'),
      f(mapply, X49, X1:'ContainerContainer', X52:'FrozenSet'),
      f(mapply, X50, X1:'ContainerContainer', X53:'FrozenSet'),
      f(mapply, X51, X1:'ContainerContainer', X54:'FrozenSet'),
      f(paint, IN:'Grid', X52:'Object', X55:'Grid'),
      f(paint, X55:'Grid', X53:'Object', X56:'Grid'),
      f(paint, X56:'Grid', X54:'Object', OUT:'Grid')
    ].
l_solve('97a05b5b', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(argmax, X1:'Container', size, X2:'Any'),
      f(subgrid, X2:'Patch', IN:'Grid', X3:'Grid'),
      f(rbind, greater, 'ONE':'Any', X4:'Callable'),
      f(compose, X4, numcolors, X5:'Callable'),
      f(sfilter, X1:'Container', X5, X6:'Container'),
      f(lbind, rbind, subtract:'Any', X7:'Callable'),
      f(switch, X3:'Grid', 'TWO':'Integer', 'ZERO':'Integer', X8:'Grid'),
      f(lbind, occurrences, X8:'Any', X9:'Callable'),
      f(lbind, lbind, shift:'Any', X10:'Callable'),
      f(compose, X7, ulcorner, X11:'Callable'),
      f(matcher, first, 'TWO':'Any', X12:'Callable'),
      f(compose, flip, X12, X13:'Callable'),
      f(rbind, sfilter, X12:'Any', X14:'Callable'),
      f(rbind, sfilter, X13:'Any', X15:'Callable'),
      f(lbind, recolor, 'ZERO':'Any', X16:'Callable'),
      f(compose, X16, X15, X17:'Callable'),
      f(fork, combine, X17, X14, X18:'Callable'),
      f(chain, X11, X18, normalize, X19:'Callable'),
      f(objects,
        X8:'Grid',
        univalued=true,
        diagonal=true,
        without_bg=true,
        X20:'Objects'),
      f(apply, toindices, X20:'Container', X21:'Container'),
      f(chain, X9, X18, normalize, X22:'Callable'),
      f(rbind, colorcount, 'TWO':'Any', X23:'Callable'),
      f(lbind, sfilter, X21:'Any', X24:'Callable'),
      f(chain, size, first, X24, X25:'Callable'),
      f(compose, positive, size, X26:'Callable'),
      f(lbind, lbind, contained:'Any', X27:'Callable'),
      f(chain, X26, X24, X27, X28:'Callable'),
      f(compose, X25, X27, X29:'Callable'),
      f(rbind, sfilter, X28:'Any', X30:'Callable'),
      f(compose, X30, X22, X31:'Callable'),
      f(lbind, rbind, equality:'Any', X32:'Callable'),
      f(rbind, compose, X29:'Any', X33:'Callable'),
      f(chain, X33, X32, X23, X34:'Callable'),
      f(fork, sfilter, X31, X34, X35:'Callable'),
      f(fork, apply, X19, X35, X36:'Callable'),
      f(compose, X10, normalize, X37:'Callable'),
      f(fork, mapply, X37, X36, X38:'Callable'),
      f(astuple, cmirror:'Integer', dmirror:'Integer', X39:'Point'),
      f(astuple, hmirror:'Integer', vmirror:'Integer', X40:'Point'),
      f(combine, X39:'Container', X40:'Container', X41:'Container'),
      f(product, X41:'Container', X41:'Container', X42:'FrozenSet'),
      f(fork, compose, first, last, X43:'Callable'),
      f(apply, X43, X42:'Container', X44:'Container'),
      f(lbind, rapply, X44:'Any', X45:'Callable'),
      f(mapply, X45, X6:'ContainerContainer', X46:'FrozenSet'),
      f(mapply, X38, X46:'ContainerContainer', X47:'FrozenSet'),
      f(paint, X3:'Grid', X47:'Object', X48:'Grid'),
      f(palette, X47:'Element', X49:'IntegerSet'),
      f(lbind, remove, 'TWO':'Any', X50:'Callable'),
      f(X50, X49, X51),
      f(chain, first, X50, palette, X52:'Callable'),
      f(rbind, contained, X51:'Any', X53:'Callable'),
      f(chain, flip, X53, X52, X54:'Callable'),
      f(sfilter, X6:'Container', X54, X55:'Container'),
      f(fork, apply, X19, X22, X56:'Callable'),
      f(fork, mapply, X37, X56, X57:'Callable'),
      f(mapply, X45, X55:'ContainerContainer', X58:'FrozenSet'),
      f(mapply, X57, X58:'ContainerContainer', X59:'FrozenSet'),
      f(paint, X48:'Grid', X59:'Object', OUT:'Grid')
    ].
l_solve('3e980e27', IN, OUT) :-
    [ f(objects,
        IN:'Grid',
        univalued=false,
        diagonal=true,
        without_bg=true,
        X1:'Objects'),
      f(lbind, contained, 'TWO':'Any', X2:'Callable'),
      f(compose, X2, palette, X3:'Callable'),
      f(lbind, contained, 'THREE':'Any', X4:'Callable'),
      f(compose, X4, palette, X5:'Callable'),
      f(sfilter, X1:'Container', X3, X6:'Container'),
      f(sfilter, X1:'Container', X5, X7:'Container'),
      f(compose, positive, size, X8:'Callable'),
      f(X8, X7, X9),
      f(X8, X6, X10),
      f(both, a=X9, b=X10, returns=X11),
      f(repeat, 'ZERO':'Any', 'ZERO':'Integer', X12:'Tuple'),
      f(rbind, subgrid, IN:'Any', X13:'Callable'),
      f(chain, asobject, vmirror, X13, X14:'Callable'),
      f(matcher, first, 'ZERO':'Any', X15:'Callable'),
      f(compose, flip, X15, X16:'Callable'),
      f(lbind, matcher, first:'Any', X17:'Callable'),
      f(lbind, rbind, add:'Any', X18:'Callable'),
      f(rbind, argmax, size:'Any', X19:'Callable'),
      f(chain, X18, invert, ulcorner, X20:'Callable'),
      f(lbind, lbind, shift:'Any', X21:'Callable'),
      f(lbind, occurrences, IN:'Any', X22:'Callable'),
      f(rbind, astuple, (0, 0):'Any', X23:'Callable'),
      f(chain, X22, initset, X23, X24:'Callable'),
      f(branch, condition=X9, X7:'Any', X6:'Any', X25:'Any'),
      f(X19, X25, X26),
      f(branch, condition=X9, identity:'Any', X14:'Any', X27:'Any'),
      f(branch, condition=X9, 'THREE':'Any', 'TWO':'Any', X28:'Any'),
      f(X27, X26, X29),
      f(sfilter, X29:'Container', X16, X30:'Container'),
      f(X24, X28, X31),
      f(X17, X28, X32),
      f(sfilter, X26:'Container', X32, X33:'Container'),
      f(center, X33:'Patch', X34:'Point'),
      f(remove, X34:'Any', X31:'Container', X35:'Container'),
      f(normalize, X30:'Patch', X36:'Patch'),
      f(sfilter, X36:'Container', X32, X37:'Container'),
      f(X20, X37, X38),
      f(apply, X38, X35:'Container', X39:'Container'),
      f(X21, X36, X40),
      f(mapply, X40, X39:'ContainerContainer', X41:'FrozenSet'),
      f(paint, IN:'Grid', X41:'Object', X42:'Grid'),
      f(branch, condition=X10, X6:'Any', X7:'Any', X43:'Any'),
      f(X19, X43, X44),
      f(branch, condition=X9, X14:'Any', identity:'Any', X45:'Any'),
      f(branch, condition=X10, 'TWO':'Any', 'THREE':'Any', X46:'Any'),
      f(X45, X44, X47),
      f(sfilter, X47:'Container', X16, X48:'Container'),
      f(X24, X46, X49),
      f(X17, X46, X50),
      f(sfilter, X44:'Container', X50, X51:'Container'),
      f(center, X51:'Patch', X52:'Point'),
      f(remove, X52:'Any', X49:'Container', X53:'Container'),
      f(normalize, X48:'Patch', X54:'Patch'),
      f(sfilter, X54:'Container', X50, X55:'Container'),
      f(X20, X55, X56),
      f(apply, X56, X53:'Container', X57:'Container'),
      f(branch, condition=X11, X57:'Any', X12:'Any', X58:'Any'),
      f(X21, X54, X59),
      f(mapply, X59, X58:'ContainerContainer', X60:'FrozenSet'),
      f(paint, X42:'Grid', X60:'Object', OUT:'Grid')
    ].

end_of_file.



from dsl import *
from constants import *


def solve_67a3c6ac(I):
    O = vmirror(I)
    return O


def solve_68b16354(I):
    O = hmirror(I)
    return O


def solve_74dd1130(I):
    O = dmirror(I)
    return O


def solve_3c9b0459(I):
    O = rot180(I)
    return O


def solve_6150a2bd(I):
    O = rot180(I)
    return O


def solve_9172f3a0(I):
    O = upscale(I, THREE)
    return O


def solve_9dfd6313(I):
    O = dmirror(I)
    return O


def solve_a416b8f3(I):
    O = hconcat(I, I)
    return O


def solve_b1948b0a(I):
    O = replace(I, SIX, TWO)
    return O


def solve_c59eb873(I):
    O = upscale(I, TWO)
    return O


def solve_c8f0f002(I):
    O = replace(I, SEVEN, FIVE)
    return O


def solve_d10ecb37(I):
    O = crop(I, ORIGIN, TWO_BY_TWO)
    return O


def solve_d511f180(I):
    O = switch(I, FIVE, EIGHT)
    return O


def solve_ed36ccf7(I):
    O = rot270(I)
    return O


def solve_4c4377d9(I):
    x1 = hmirror(I)
    O = vconcat(x1, I)
    return O


def solve_6d0aefbc(I):
    x1 = vmirror(I)
    O = hconcat(I, x1)
    return O


def solve_6fa7a44f(I):
    x1 = hmirror(I)
    O = vconcat(I, x1)
    return O


def solve_5614dbcf(I):
    x1 = replace(I, FIVE, ZERO)
    O = downscale(x1, THREE)
    return O


def solve_5bd6f4ac(I):
    x1 = tojvec(SIX)
    O = crop(I, x1, THREE_BY_THREE)
    return O


def solve_5582e5ca(I):
    x1 = mostcolor(I)
    O = canvas(x1, THREE_BY_THREE)
    return O


def solve_8be77c9e(I):
    x1 = hmirror(I)
    O = vconcat(I, x1)
    return O


def solve_c9e6f938(I):
    x1 = vmirror(I)
    O = hconcat(I, x1)
    return O


def solve_2dee498d(I):
    x1 = hsplit(I, THREE)
    O = first(x1)
    return O


def solve_1cf80156(I):
    x1 = objects(I, T, T, T)
    x2 = first(x1)
    O = subgrid(x2, I)
    return O


def solve_32597951(I):
    x1 = ofcolor(I, EIGHT)
    x2 = delta(x1)
    O = fill(I, THREE, x2)
    return O


def solve_25ff71a9(I):
    x1 = objects(I, T, T, T)
    x2 = first(x1)
    O = move(I, x2, DOWN)
    return O


def solve_0b148d64(I):
    x1 = partition(I)
    x2 = argmin(x1, size)
    O = subgrid(x2, I)
    return O


def solve_1f85a75f(I):
    x1 = objects(I, T, T, T)
    x2 = argmax(x1, size)
    O = subgrid(x2, I)
    return O


def solve_23b5c85d(I):
    x1 = objects(I, T, T, T)
    x2 = argmin(x1, size)
    O = subgrid(x2, I)
    return O


def solve_9ecd008a(I):
    x1 = vmirror(I)
    x2 = ofcolor(I, ZERO)
    O = subgrid(x2, x1)
    return O


def solve_ac0a08a4(I):
    x1 = colorcount(I, ZERO)
    x2 = subtract(NINE, x1)
    O = upscale(I, x2)
    return O


def solve_be94b721(I):
    x1 = objects(I, T, F, T)
    x2 = argmax(x1, size)
    O = subgrid(x2, I)
    return O


def solve_c909285e(I):
    x1 = leastcolor(I)
    x2 = ofcolor(I, x1)
    O = subgrid(x2, I)
    return O


def solve_f25ffba3(I):
    x1 = bottomhalf(I)
    x2 = hmirror(x1)
    O = vconcat(x2, x1)
    return O


def solve_c1d99e64(I):
    x1 = frontiers(I)
    x2 = merge(x1)
    O = fill(I, TWO, x2)
    return O


def solve_b91ae062(I):
    x1 = numcolors(I)
    x2 = decrement(x1)
    O = upscale(I, x2)
    return O


def solve_3aa6fb7a(I):
    x1 = objects(I, T, F, T)
    x2 = mapply(corners, x1)
    O = underfill(I, ONE, x2)
    return O


def solve_7b7f7511(I):
    x1 = portrait(I)
    x2 = branch(x1, tophalf, lefthalf)
    O = x2(I)
    return O


def solve_4258a5f9(I):
    x1 = ofcolor(I, FIVE)
    x2 = mapply(neighbors, x1)
    O = fill(I, ONE, x2)
    return O


def solve_2dc579da(I):
    x1 = vsplit(I, TWO)
    x2 = rbind(hsplit, TWO)
    x3 = mapply(x2, x1)
    O = argmax(x3, numcolors)
    return O


def solve_28bf18c6(I):
    x1 = objects(I, T, T, T)
    x2 = first(x1)
    x3 = subgrid(x2, I)
    O = hconcat(x3, x3)
    return O


def solve_3af2c5a8(I):
    x1 = vmirror(I)
    x2 = hconcat(I, x1)
    x3 = hmirror(x2)
    O = vconcat(x2, x3)
    return O


def solve_44f52bb0(I):
    x1 = vmirror(I)
    x2 = equality(x1, I)
    x3 = branch(x2, ONE, SEVEN)
    O = canvas(x3, UNITY)
    return O


def solve_62c24649(I):
    x1 = vmirror(I)
    x2 = hconcat(I, x1)
    x3 = hmirror(x2)
    O = vconcat(x2, x3)
    return O


def solve_67e8384a(I):
    x1 = vmirror(I)
    x2 = hconcat(I, x1)
    x3 = hmirror(x2)
    O = vconcat(x2, x3)
    return O


def solve_7468f01a(I):
    x1 = objects(I, F, T, T)
    x2 = first(x1)
    x3 = subgrid(x2, I)
    O = vmirror(x3)
    return O


def solve_662c240a(I):
    x1 = vsplit(I, THREE)
    x2 = fork(equality, dmirror, identity)
    x3 = compose(flip, x2)
    O = extract(x1, x3)
    return O


def solve_42a50994(I):
    x1 = objects(I, T, T, T)
    x2 = sizefilter(x1, ONE)
    x3 = merge(x2)
    O = cover(I, x3)
    return O


def solve_56ff96f3(I):
    x1 = fgpartition(I)
    x2 = fork(recolor, color, backdrop)
    x3 = mapply(x2, x1)
    O = paint(I, x3)
    return O


def solve_50cb2852(I):
    x1 = objects(I, T, F, T)
    x2 = compose(backdrop, inbox)
    x3 = mapply(x2, x1)
    O = fill(I, EIGHT, x3)
    return O


def solve_4347f46a(I):
    x1 = objects(I, T, F, T)
    x2 = fork(difference, toindices, box)
    x3 = mapply(x2, x1)
    O = fill(I, ZERO, x3)
    return O


def solve_46f33fce(I):
    x1 = rot180(I)
    x2 = downscale(x1, TWO)
    x3 = rot180(x2)
    O = upscale(x3, FOUR)
    return O


def solve_a740d043(I):
    x1 = objects(I, T, T, T)
    x2 = merge(x1)
    x3 = subgrid(x2, I)
    O = replace(x3, ONE, ZERO)
    return O


def solve_a79310a0(I):
    x1 = objects(I, T, F, T)
    x2 = first(x1)
    x3 = move(I, x2, DOWN)
    O = replace(x3, EIGHT, TWO)
    return O


def solve_aabf363d(I):
    x1 = leastcolor(I)
    x2 = replace(I, x1, ZERO)
    x3 = leastcolor(x2)
    O = replace(x2, x3, x1)
    return O


def solve_ae4f1146(I):
    x1 = objects(I, F, F, T)
    x2 = rbind(colorcount, ONE)
    x3 = argmax(x1, x2)
    O = subgrid(x3, I)
    return O


def solve_b27ca6d3(I):
    x1 = objects(I, T, F, T)
    x2 = sizefilter(x1, TWO)
    x3 = mapply(outbox, x2)
    O = fill(I, THREE, x3)
    return O


def solve_ce22a75a(I):
    x1 = objects(I, T, F, T)
    x2 = apply(outbox, x1)
    x3 = mapply(backdrop, x2)
    O = fill(I, ONE, x3)
    return O


def solve_dc1df850(I):
    x1 = objects(I, T, F, T)
    x2 = colorfilter(x1, TWO)
    x3 = mapply(outbox, x2)
    O = fill(I, ONE, x3)
    return O


def solve_f25fbde4(I):
    x1 = objects(I, T, T, T)
    x2 = first(x1)
    x3 = subgrid(x2, I)
    O = upscale(x3, TWO)
    return O


def solve_44d8ac46(I):
    x1 = objects(I, T, F, T)
    x2 = apply(delta, x1)
    x3 = mfilter(x2, square)
    O = fill(I, TWO, x3)
    return O


def solve_1e0a9b12(I):
    x1 = rot270(I)
    x2 = rbind(order, identity)
    x3 = apply(x2, x1)
    O = rot90(x3)
    return O


def solve_0d3d703e(I):
    x1 = switch(I, THREE, FOUR)
    x2 = switch(x1, EIGHT, NINE)
    x3 = switch(x2, TWO, SIX)
    O = switch(x3, ONE, FIVE)
    return O


def solve_3618c87e(I):
    x1 = objects(I, T, F, T)
    x2 = sizefilter(x1, ONE)
    x3 = merge(x2)
    O = move(I, x3, TWO_BY_ZERO)
    return O


def solve_1c786137(I):
    x1 = objects(I, T, F, F)
    x2 = argmax(x1, height)
    x3 = subgrid(x2, I)
    O = trim(x3)
    return O


def solve_8efcae92(I):
    x1 = objects(I, T, F, F)
    x2 = colorfilter(x1, ONE)
    x3 = compose(size, delta)
    x4 = argmax(x2, x3)
    O = subgrid(x4, I)
    return O


def solve_445eab21(I):
    x1 = objects(I, T, F, T)
    x2 = fork(multiply, height, width)
    x3 = argmax(x1, x2)
    x4 = color(x3)
    O = canvas(x4, TWO_BY_TWO)
    return O


def solve_6f8cd79b(I):
    x1 = asindices(I)
    x2 = apply(initset, x1)
    x3 = rbind(bordering, I)
    x4 = mfilter(x2, x3)
    O = fill(I, EIGHT, x4)
    return O


def solve_2013d3e2(I):
    x1 = objects(I, F, T, T)
    x2 = first(x1)
    x3 = subgrid(x2, I)
    x4 = lefthalf(x3)
    O = tophalf(x4)
    return O


def solve_41e4d17e(I):
    x1 = objects(I, T, F, T)
    x2 = fork(combine, vfrontier, hfrontier)
    x3 = compose(x2, center)
    x4 = mapply(x3, x1)
    O = underfill(I, SIX, x4)
    return O


def solve_9565186b(I):
    x1 = shape(I)
    x2 = objects(I, T, F, F)
    x3 = argmax(x2, size)
    x4 = canvas(FIVE, x1)
    O = paint(x4, x3)
    return O


def solve_aedd82e4(I):
    x1 = objects(I, T, F, F)
    x2 = colorfilter(x1, TWO)
    x3 = sizefilter(x2, ONE)
    x4 = merge(x3)
    O = fill(I, ONE, x4)
    return O


def solve_bb43febb(I):
    x1 = objects(I, T, F, F)
    x2 = colorfilter(x1, FIVE)
    x3 = compose(backdrop, inbox)
    x4 = mapply(x3, x2)
    O = fill(I, TWO, x4)
    return O


def solve_e98196ab(I):
    x1 = tophalf(I)
    x2 = bottomhalf(I)
    x3 = objects(x1, T, F, T)
    x4 = merge(x3)
    O = paint(x2, x4)
    return O


def solve_f76d97a5(I):
    x1 = palette(I)
    x2 = first(x1)
    x3 = last(x1)
    x4 = switch(I, x2, x3)
    O = replace(x4, FIVE, ZERO)
    return O


def solve_ce9e57f2(I):
    x1 = objects(I, T, F, T)
    x2 = fork(connect, ulcorner, centerofmass)
    x3 = mapply(x2, x1)
    x4 = fill(I, EIGHT, x3)
    O = switch(x4, EIGHT, TWO)
    return O


def solve_22eb0ac0(I):
    x1 = fgpartition(I)
    x2 = fork(recolor, color, backdrop)
    x3 = apply(x2, x1)
    x4 = mfilter(x3, hline)
    O = paint(I, x4)
    return O


def solve_9f236235(I):
    x1 = compress(I)
    x2 = objects(I, T, F, F)
    x3 = vmirror(x1)
    x4 = valmin(x2, width)
    O = downscale(x3, x4)
    return O


def solve_a699fb00(I):
    x1 = ofcolor(I, ONE)
    x2 = shift(x1, RIGHT)
    x3 = shift(x1, LEFT)
    x4 = intersection(x2, x3)
    O = fill(I, TWO, x4)
    return O


def solve_46442a0e(I):
    x1 = rot90(I)
    x2 = rot180(I)
    x3 = rot270(I)
    x4 = hconcat(I, x1)
    x5 = hconcat(x3, x2)
    O = vconcat(x4, x5)
    return O


def solve_7fe24cdd(I):
    x1 = rot90(I)
    x2 = rot180(I)
    x3 = rot270(I)
    x4 = hconcat(I, x1)
    x5 = hconcat(x3, x2)
    O = vconcat(x4, x5)
    return O


def solve_0ca9ddb6(I):
    x1 = ofcolor(I, ONE)
    x2 = ofcolor(I, TWO)
    x3 = mapply(dneighbors, x1)
    x4 = mapply(ineighbors, x2)
    x5 = fill(I, SEVEN, x3)
    O = fill(x5, FOUR, x4)
    return O


def solve_543a7ed5(I):
    x1 = objects(I, T, F, T)
    x2 = colorfilter(x1, SIX)
    x3 = mapply(outbox, x2)
    x4 = fill(I, THREE, x3)
    x5 = mapply(delta, x2)
    O = fill(x4, FOUR, x5)
    return O


def solve_0520fde7(I):
    x1 = vmirror(I)
    x2 = lefthalf(x1)
    x3 = righthalf(x1)
    x4 = vmirror(x3)
    x5 = cellwise(x2, x4, ZERO)
    O = replace(x5, ONE, TWO)
    return O


def solve_dae9d2b5(I):
    x1 = lefthalf(I)
    x2 = righthalf(I)
    x3 = ofcolor(x1, FOUR)
    x4 = ofcolor(x2, THREE)
    x5 = combine(x3, x4)
    O = fill(x1, SIX, x5)
    return O


def solve_8d5021e8(I):
    x1 = vmirror(I)
    x2 = hconcat(x1, I)
    x3 = hmirror(x2)
    x4 = vconcat(x2, x3)
    x5 = vconcat(x4, x2)
    O = hmirror(x5)
    return O


def solve_928ad970(I):
    x1 = ofcolor(I, FIVE)
    x2 = subgrid(x1, I)
    x3 = trim(x2)
    x4 = leastcolor(x3)
    x5 = inbox(x1)
    O = fill(I, x4, x5)
    return O


def solve_b60334d2(I):
    x1 = ofcolor(I, FIVE)
    x2 = replace(I, FIVE, ZERO)
    x3 = mapply(dneighbors, x1)
    x4 = mapply(ineighbors, x1)
    x5 = fill(x2, ONE, x3)
    O = fill(x5, FIVE, x4)
    return O


def solve_b94a9452(I):
    x1 = objects(I, F, F, T)
    x2 = first(x1)
    x3 = subgrid(x2, I)
    x4 = leastcolor(x3)
    x5 = mostcolor(x3)
    O = switch(x3, x4, x5)
    return O


def solve_d037b0a7(I):
    x1 = objects(I, T, F, T)
    x2 = rbind(shoot, DOWN)
    x3 = compose(x2, center)
    x4 = fork(recolor, color, x3)
    x5 = mapply(x4, x1)
    O = paint(I, x5)
    return O


def solve_d0f5fe59(I):
    x1 = objects(I, T, F, T)
    x2 = size(x1)
    x3 = astuple(x2, x2)
    x4 = canvas(ZERO, x3)
    x5 = shoot(ORIGIN, UNITY)
    O = fill(x4, EIGHT, x5)
    return O


def solve_e3497940(I):
    x1 = lefthalf(I)
    x2 = righthalf(I)
    x3 = vmirror(x2)
    x4 = objects(x3, T, F, T)
    x5 = merge(x4)
    O = paint(x1, x5)
    return O


def solve_e9afcf9a(I):
    x1 = astuple(TWO, ONE)
    x2 = crop(I, ORIGIN, x1)
    x3 = hmirror(x2)
    x4 = hconcat(x2, x3)
    x5 = hconcat(x4, x4)
    O = hconcat(x5, x4)
    return O


def solve_48d8fb45(I):
    x1 = objects(I, T, T, T)
    x2 = matcher(size, ONE)
    x3 = extract(x1, x2)
    x4 = lbind(adjacent, x3)
    x5 = extract(x1, x4)
    O = subgrid(x5, I)
    return O


def solve_d406998b(I):
    x1 = vmirror(I)
    x2 = ofcolor(x1, FIVE)
    x3 = compose(even, last)
    x4 = sfilter(x2, x3)
    x5 = fill(x1, THREE, x4)
    O = vmirror(x5)
    return O


def solve_5117e062(I):
    x1 = objects(I, F, T, T)
    x2 = matcher(numcolors, TWO)
    x3 = extract(x1, x2)
    x4 = subgrid(x3, I)
    x5 = mostcolor(x3)
    O = replace(x4, EIGHT, x5)
    return O


def solve_3906de3d(I):
    x1 = rot270(I)
    x2 = rbind(order, identity)
    x3 = switch(x1, ONE, TWO)
    x4 = apply(x2, x3)
    x5 = switch(x4, ONE, TWO)
    O = cmirror(x5)
    return O


def solve_00d62c1b(I):
    x1 = objects(I, T, F, F)
    x2 = colorfilter(x1, ZERO)
    x3 = rbind(bordering, I)
    x4 = compose(flip, x3)
    x5 = mfilter(x2, x4)
    O = fill(I, FOUR, x5)
    return O


def solve_7b6016b9(I):
    x1 = objects(I, T, F, F)
    x2 = rbind(bordering, I)
    x3 = compose(flip, x2)
    x4 = mfilter(x1, x3)
    x5 = fill(I, TWO, x4)
    O = replace(x5, ZERO, THREE)
    return O


def solve_67385a82(I):
    x1 = objects(I, T, F, F)
    x2 = colorfilter(x1, THREE)
    x3 = sizefilter(x2, ONE)
    x4 = difference(x2, x3)
    x5 = merge(x4)
    O = fill(I, EIGHT, x5)
    return O


def solve_a5313dff(I):
    x1 = objects(I, T, F, F)
    x2 = colorfilter(x1, ZERO)
    x3 = rbind(bordering, I)
    x4 = compose(flip, x3)
    x5 = mfilter(x2, x4)
    O = fill(I, ONE, x5)
    return O


def solve_ea32f347(I):
    x1 = objects(I, T, F, T)
    x2 = replace(I, FIVE, FOUR)
    x3 = argmin(x1, size)
    x4 = argmax(x1, size)
    x5 = fill(x2, ONE, x4)
    O = fill(x5, TWO, x3)
    return O


def solve_d631b094(I):
    x1 = palette(I)
    x2 = other(x1, ZERO)
    x3 = ofcolor(I, x2)
    x4 = size(x3)
    x5 = astuple(ONE, x4)
    O = canvas(x2, x5)
    return O


def solve_10fcaaa3(I):
    x1 = leastcolor(I)
    x2 = hconcat(I, I)
    x3 = vconcat(x2, x2)
    x4 = ofcolor(x3, x1)
    x5 = mapply(ineighbors, x4)
    O = underfill(x3, EIGHT, x5)
    return O


def solve_007bbfb7(I):
    x1 = hupscale(I, THREE)
    x2 = vupscale(x1, THREE)
    x3 = hconcat(I, I)
    x4 = hconcat(x3, I)
    x5 = vconcat(x4, x4)
    x6 = vconcat(x5, x4)
    O = cellwise(x2, x6, ZERO)
    return O


def solve_496994bd(I):
    x1 = width(I)
    x2 = height(I)
    x3 = halve(x2)
    x4 = astuple(x3, x1)
    x5 = crop(I, ORIGIN, x4)
    x6 = hmirror(x5)
    O = vconcat(x5, x6)
    return O


def solve_1f876c06(I):
    x1 = fgpartition(I)
    x2 = compose(last, first)
    x3 = power(last, TWO)
    x4 = fork(connect, x2, x3)
    x5 = fork(recolor, color, x4)
    x6 = mapply(x5, x1)
    O = paint(I, x6)
    return O


def solve_05f2a901(I):
    x1 = objects(I, T, F, T)
    x2 = colorfilter(x1, TWO)
    x3 = first(x2)
    x4 = colorfilter(x1, EIGHT)
    x5 = first(x4)
    x6 = gravitate(x3, x5)
    O = move(I, x3, x6)
    return O


def solve_39a8645d(I):
    x1 = objects(I, T, T, T)
    x2 = totuple(x1)
    x3 = apply(color, x2)
    x4 = mostcommon(x3)
    x5 = matcher(color, x4)
    x6 = extract(x1, x5)
    O = subgrid(x6, I)
    return O


def solve_1b2d62fb(I):
    x1 = lefthalf(I)
    x2 = righthalf(I)
    x3 = ofcolor(x1, ZERO)
    x4 = ofcolor(x2, ZERO)
    x5 = intersection(x3, x4)
    x6 = replace(x1, NINE, ZERO)
    O = fill(x6, EIGHT, x5)
    return O


def solve_90c28cc7(I):
    x1 = objects(I, F, F, T)
    x2 = first(x1)
    x3 = subgrid(x2, I)
    x4 = dedupe(x3)
    x5 = rot90(x4)
    x6 = dedupe(x5)
    O = rot270(x6)
    return O


def solve_b6afb2da(I):
    x1 = objects(I, T, F, F)
    x2 = replace(I, FIVE, TWO)
    x3 = colorfilter(x1, FIVE)
    x4 = mapply(box, x3)
    x5 = fill(x2, FOUR, x4)
    x6 = mapply(corners, x3)
    O = fill(x5, ONE, x6)
    return O


def solve_b9b7f026(I):
    x1 = objects(I, T, F, F)
    x2 = argmin(x1, size)
    x3 = rbind(adjacent, x2)
    x4 = remove(x2, x1)
    x5 = extract(x4, x3)
    x6 = color(x5)
    O = canvas(x6, UNITY)
    return O


def solve_ba97ae07(I):
    x1 = objects(I, T, F, T)
    x2 = totuple(x1)
    x3 = apply(color, x2)
    x4 = mostcommon(x3)
    x5 = ofcolor(I, x4)
    x6 = backdrop(x5)
    O = fill(I, x4, x6)
    return O


def solve_c9f8e694(I):
    x1 = height(I)
    x2 = width(I)
    x3 = ofcolor(I, ZERO)
    x4 = astuple(x1, ONE)
    x5 = crop(I, ORIGIN, x4)
    x6 = hupscale(x5, x2)
    O = fill(x6, ZERO, x3)
    return O


def solve_d23f8c26(I):
    x1 = asindices(I)
    x2 = width(I)
    x3 = halve(x2)
    x4 = matcher(last, x3)
    x5 = compose(flip, x4)
    x6 = sfilter(x1, x5)
    O = fill(I, ZERO, x6)
    return O


def solve_d5d6de2d(I):
    x1 = objects(I, T, F, T)
    x2 = sfilter(x1, square)
    x3 = difference(x1, x2)
    x4 = compose(backdrop, inbox)
    x5 = mapply(x4, x3)
    x6 = replace(I, TWO, ZERO)
    O = fill(x6, THREE, x5)
    return O


def solve_dbc1a6ce(I):
    x1 = ofcolor(I, ONE)
    x2 = product(x1, x1)
    x3 = fork(connect, first, last)
    x4 = apply(x3, x2)
    x5 = fork(either, vline, hline)
    x6 = mfilter(x4, x5)
    O = underfill(I, EIGHT, x6)
    return O


def solve_ded97339(I):
    x1 = ofcolor(I, EIGHT)
    x2 = product(x1, x1)
    x3 = fork(connect, first, last)
    x4 = apply(x3, x2)
    x5 = fork(either, vline, hline)
    x6 = mfilter(x4, x5)
    O = underfill(I, EIGHT, x6)
    return O


def solve_ea786f4a(I):
    x1 = width(I)
    x2 = shoot(ORIGIN, UNITY)
    x3 = decrement(x1)
    x4 = tojvec(x3)
    x5 = shoot(x4, DOWN_LEFT)
    x6 = combine(x2, x5)
    O = fill(I, ZERO, x6)
    return O


def solve_08ed6ac7(I):
    x1 = objects(I, T, F, T)
    x2 = totuple(x1)
    x3 = order(x1, height)
    x4 = size(x2)
    x5 = interval(x4, ZERO, NEG_ONE)
    x6 = mpapply(recolor, x5, x3)
    O = paint(I, x6)
    return O


def solve_40853293(I):
    x1 = partition(I)
    x2 = fork(recolor, color, backdrop)
    x3 = apply(x2, x1)
    x4 = mfilter(x3, hline)
    x5 = mfilter(x3, vline)
    x6 = paint(I, x4)
    O = paint(x6, x5)
    return O


def solve_5521c0d9(I):
    x1 = objects(I, T, F, T)
    x2 = merge(x1)
    x3 = cover(I, x2)
    x4 = chain(toivec, invert, height)
    x5 = fork(shift, identity, x4)
    x6 = mapply(x5, x1)
    O = paint(x3, x6)
    return O


def solve_f8ff0b80(I):
    x1 = objects(I, T, T, T)
    x2 = order(x1, size)
    x3 = apply(color, x2)
    x4 = rbind(canvas, UNITY)
    x5 = apply(x4, x3)
    x6 = merge(x5)
    O = hmirror(x6)
    return O


def solve_85c4e7cd(I):
    x1 = objects(I, T, F, F)
    x2 = compose(invert, size)
    x3 = order(x1, size)
    x4 = order(x1, x2)
    x5 = apply(color, x4)
    x6 = mpapply(recolor, x5, x3)
    O = paint(I, x6)
    return O


def solve_d2abd087(I):
    x1 = objects(I, T, F, T)
    x2 = matcher(size, SIX)
    x3 = compose(flip, x2)
    x4 = mfilter(x1, x2)
    x5 = mfilter(x1, x3)
    x6 = fill(I, TWO, x4)
    O = fill(x6, ONE, x5)
    return O


def solve_017c7c7b(I):
    x1 = tophalf(I)
    x2 = bottomhalf(I)
    x3 = equality(x1, x2)
    x4 = crop(I, TWO_BY_ZERO, THREE_BY_THREE)
    x5 = branch(x3, x2, x4)
    x6 = vconcat(I, x5)
    O = replace(x6, ONE, TWO)
    return O


def solve_363442ee(I):
    x1 = ofcolor(I, ONE)
    x2 = crop(I, ORIGIN, THREE_BY_THREE)
    x3 = asobject(x2)
    x4 = lbind(shift, x3)
    x5 = compose(x4, decrement)
    x6 = mapply(x5, x1)
    O = paint(I, x6)
    return O


def solve_5168d44c(I):
    x1 = ofcolor(I, THREE)
    x2 = height(x1)
    x3 = equality(x2, ONE)
    x4 = branch(x3, ZERO_BY_TWO, TWO_BY_ZERO)
    x5 = ofcolor(I, TWO)
    x6 = recolor(TWO, x5)
    O = move(I, x6, x4)
    return O


def solve_e9614598(I):
    x1 = ofcolor(I, ONE)
    x2 = fork(add, first, last)
    x3 = x2(x1)
    x4 = halve(x3)
    x5 = dneighbors(x4)
    x6 = insert(x4, x5)
    O = fill(I, THREE, x6)
    return O


def solve_d9fac9be(I):
    x1 = palette(I)
    x2 = objects(I, T, F, T)
    x3 = argmax(x2, size)
    x4 = color(x3)
    x5 = remove(ZERO, x1)
    x6 = other(x5, x4)
    O = canvas(x6, UNITY)
    return O


def solve_e50d258f(I):
    x1 = width(I)
    x2 = astuple(NINE, x1)
    x3 = canvas(ZERO, x2)
    x4 = vconcat(I, x3)
    x5 = objects(x4, F, F, T)
    x6 = rbind(colorcount, TWO)
    x7 = argmax(x5, x6)
    O = subgrid(x7, I)
    return O


def solve_810b9b61(I):
    x1 = objects(I, T, T, T)
    x2 = apply(toindices, x1)
    x3 = fork(either, vline, hline)
    x4 = sfilter(x2, x3)
    x5 = difference(x2, x4)
    x6 = fork(equality, identity, box)
    x7 = mfilter(x5, x6)
    O = fill(I, THREE, x7)
    return O


def solve_54d82841(I):
    x1 = height(I)
    x2 = objects(I, T, F, T)
    x3 = compose(last, center)
    x4 = apply(x3, x2)
    x5 = decrement(x1)
    x6 = lbind(astuple, x5)
    x7 = apply(x6, x4)
    O = fill(I, FOUR, x7)
    return O


def solve_60b61512(I):
    x1 = objects(I, T, T, T)
    x2 = rbind(subgrid, I)
    x3 = compose(asindices, x2)
    x4 = fork(shift, x3, ulcorner)
    x5 = mapply(x4, x1)
    x6 = fill(I, SEVEN, x5)
    x7 = merge(x1)
    O = paint(x6, x7)
    return O


def solve_25d8a9c8(I):
    x1 = asindices(I)
    x2 = objects(I, T, F, F)
    x3 = sizefilter(x2, THREE)
    x4 = mfilter(x3, hline)
    x5 = toindices(x4)
    x6 = difference(x1, x5)
    x7 = fill(I, FIVE, x5)
    O = fill(x7, ZERO, x6)
    return O


def solve_239be575(I):
    x1 = objects(I, F, T, T)
    x2 = lbind(contained, TWO)
    x3 = compose(x2, palette)
    x4 = sfilter(x1, x3)
    x5 = size(x4)
    x6 = greater(x5, ONE)
    x7 = branch(x6, ZERO, EIGHT)
    O = canvas(x7, UNITY)
    return O


def solve_67a423a3(I):
    x1 = leastcolor(I)
    x2 = objects(I, T, F, T)
    x3 = colorfilter(x2, x1)
    x4 = merge(x3)
    x5 = delta(x4)
    x6 = first(x5)
    x7 = neighbors(x6)
    O = fill(I, FOUR, x7)
    return O


def solve_5c0a986e(I):
    x1 = ofcolor(I, TWO)
    x2 = ofcolor(I, ONE)
    x3 = lrcorner(x1)
    x4 = ulcorner(x2)
    x5 = shoot(x3, UNITY)
    x6 = shoot(x4, NEG_UNITY)
    x7 = fill(I, TWO, x5)
    O = fill(x7, ONE, x6)
    return O


def solve_6430c8c4(I):
    x1 = tophalf(I)
    x2 = bottomhalf(I)
    x3 = astuple(FOUR, FOUR)
    x4 = ofcolor(x1, ZERO)
    x5 = ofcolor(x2, ZERO)
    x6 = intersection(x4, x5)
    x7 = canvas(ZERO, x3)
    O = fill(x7, THREE, x6)
    return O


def solve_94f9d214(I):
    x1 = tophalf(I)
    x2 = bottomhalf(I)
    x3 = ofcolor(x1, ZERO)
    x4 = ofcolor(x2, ZERO)
    x5 = astuple(FOUR, FOUR)
    x6 = canvas(ZERO, x5)
    x7 = intersection(x3, x4)
    O = fill(x6, TWO, x7)
    return O


def solve_a1570a43(I):
    x1 = ofcolor(I, TWO)
    x2 = ofcolor(I, THREE)
    x3 = recolor(TWO, x1)
    x4 = ulcorner(x2)
    x5 = ulcorner(x1)
    x6 = subtract(x4, x5)
    x7 = increment(x6)
    O = move(I, x3, x7)
    return O


def solve_ce4f8723(I):
    x1 = tophalf(I)
    x2 = bottomhalf(I)
    x3 = ofcolor(x1, ZERO)
    x4 = ofcolor(x2, ZERO)
    x5 = intersection(x3, x4)
    x6 = astuple(FOUR, FOUR)
    x7 = canvas(THREE, x6)
    O = fill(x7, ZERO, x5)
    return O


def solve_d13f3404(I):
    x1 = objects(I, T, F, T)
    x2 = rbind(shoot, UNITY)
    x3 = compose(x2, center)
    x4 = fork(recolor, color, x3)
    x5 = mapply(x4, x1)
    x6 = astuple(SIX, SIX)
    x7 = canvas(ZERO, x6)
    O = paint(x7, x5)
    return O


def solve_dc433765(I):
    x1 = ofcolor(I, THREE)
    x2 = ofcolor(I, FOUR)
    x3 = first(x1)
    x4 = first(x2)
    x5 = subtract(x4, x3)
    x6 = sign(x5)
    x7 = recolor(THREE, x1)
    O = move(I, x7, x6)
    return O


def solve_f2829549(I):
    x1 = lefthalf(I)
    x2 = righthalf(I)
    x3 = ofcolor(x1, ZERO)
    x4 = ofcolor(x2, ZERO)
    x5 = intersection(x3, x4)
    x6 = shape(x1)
    x7 = canvas(ZERO, x6)
    O = fill(x7, THREE, x5)
    return O


def solve_fafffa47(I):
    x1 = tophalf(I)
    x2 = bottomhalf(I)
    x3 = shape(x2)
    x4 = ofcolor(x1, ZERO)
    x5 = ofcolor(x2, ZERO)
    x6 = intersection(x4, x5)
    x7 = canvas(ZERO, x3)
    O = fill(x7, TWO, x6)
    return O


def solve_fcb5c309(I):
    x1 = leastcolor(I)
    x2 = objects(I, T, F, T)
    x3 = colorfilter(x2, x1)
    x4 = difference(x2, x3)
    x5 = argmax(x4, size)
    x6 = color(x5)
    x7 = subgrid(x5, I)
    O = replace(x7, x6, x1)
    return O


def solve_ff805c23(I):
    x1 = hmirror(I)
    x2 = vmirror(I)
    x3 = ofcolor(I, ONE)
    x4 = subgrid(x3, x1)
    x5 = subgrid(x3, x2)
    x6 = palette(x4)
    x7 = contained(ONE, x6)
    O = branch(x7, x5, x4)
    return O


def solve_e76a88a6(I):
    x1 = objects(I, F, F, T)
    x2 = argmax(x1, numcolors)
    x3 = normalize(x2)
    x4 = remove(x2, x1)
    x5 = apply(ulcorner, x4)
    x6 = lbind(shift, x3)
    x7 = mapply(x6, x5)
    O = paint(I, x7)
    return O


def solve_7c008303(I):
    x1 = ofcolor(I, THREE)
    x2 = subgrid(x1, I)
    x3 = ofcolor(x2, ZERO)
    x4 = replace(I, THREE, ZERO)
    x5 = replace(x4, EIGHT, ZERO)
    x6 = compress(x5)
    x7 = upscale(x6, THREE)
    O = fill(x7, ZERO, x3)
    return O


def solve_7f4411dc(I):
    x1 = leastcolor(I)
    x2 = ofcolor(I, x1)
    x3 = rbind(difference, x2)
    x4 = rbind(greater, TWO)
    x5 = chain(x4, size, x3)
    x6 = compose(x5, dneighbors)
    x7 = sfilter(x2, x6)
    O = fill(I, ZERO, x7)
    return O


def solve_b230c067(I):
    x1 = objects(I, T, T, T)
    x2 = totuple(x1)
    x3 = apply(normalize, x2)
    x4 = leastcommon(x3)
    x5 = matcher(normalize, x4)
    x6 = extract(x1, x5)
    x7 = replace(I, EIGHT, ONE)
    O = fill(x7, TWO, x6)
    return O


def solve_e8593010(I):
    x1 = objects(I, T, F, T)
    x2 = sizefilter(x1, ONE)
    x3 = sizefilter(x1, TWO)
    x4 = merge(x2)
    x5 = fill(I, THREE, x4)
    x6 = merge(x3)
    x7 = fill(x5, TWO, x6)
    O = replace(x7, ZERO, ONE)
    return O


def solve_6d75e8bb(I):
    x1 = objects(I, T, F, T)
    x2 = first(x1)
    x3 = ulcorner(x2)
    x4 = subgrid(x2, I)
    x5 = replace(x4, ZERO, TWO)
    x6 = asobject(x5)
    x7 = shift(x6, x3)
    O = paint(I, x7)
    return O


def solve_3f7978a0(I):
    x1 = fgpartition(I)
    x2 = matcher(color, FIVE)
    x3 = extract(x1, x2)
    x4 = ulcorner(x3)
    x5 = subtract(x4, DOWN)
    x6 = shape(x3)
    x7 = add(x6, TWO_BY_ZERO)
    O = crop(I, x5, x7)
    return O


def solve_1190e5a7(I):
    x1 = mostcolor(I)
    x2 = frontiers(I)
    x3 = sfilter(x2, vline)
    x4 = difference(x2, x3)
    x5 = astuple(x4, x3)
    x6 = apply(size, x5)
    x7 = increment(x6)
    O = canvas(x1, x7)
    return O


def solve_6e02f1e3(I):
    x1 = numcolors(I)
    x2 = canvas(ZERO, THREE_BY_THREE)
    x3 = equality(x1, THREE)
    x4 = equality(x1, TWO)
    x5 = branch(x3, TWO_BY_ZERO, ORIGIN)
    x6 = branch(x4, TWO_BY_TWO, ZERO_BY_TWO)
    x7 = connect(x5, x6)
    O = fill(x2, FIVE, x7)
    return O


def solve_a61f2674(I):
    x1 = objects(I, T, F, T)
    x2 = argmax(x1, size)
    x3 = argmin(x1, size)
    x4 = replace(I, FIVE, ZERO)
    x5 = recolor(ONE, x2)
    x6 = recolor(TWO, x3)
    x7 = combine(x5, x6)
    O = paint(x4, x7)
    return O


def solve_fcc82909(I):
    x1 = objects(I, F, T, T)
    x2 = rbind(add, DOWN)
    x3 = compose(x2, llcorner)
    x4 = compose(toivec, numcolors)
    x5 = fork(add, lrcorner, x4)
    x6 = fork(astuple, x3, x5)
    x7 = compose(box, x6)
    x8 = mapply(x7, x1)
    O = fill(I, THREE, x8)
    return O


def solve_72ca375d(I):
    x1 = objects(I, T, T, T)
    x2 = totuple(x1)
    x3 = rbind(subgrid, I)
    x4 = apply(x3, x2)
    x5 = apply(vmirror, x4)
    x6 = papply(equality, x4, x5)
    x7 = pair(x4, x6)
    x8 = extract(x7, last)
    O = first(x8)
    return O


def solve_253bf280(I):
    x1 = ofcolor(I, EIGHT)
    x2 = prapply(connect, x1, x1)
    x3 = rbind(greater, ONE)
    x4 = compose(x3, size)
    x5 = sfilter(x2, x4)
    x6 = fork(either, vline, hline)
    x7 = mfilter(x5, x6)
    x8 = fill(I, THREE, x7)
    O = fill(x8, EIGHT, x1)
    return O


def solve_694f12f3(I):
    x1 = objects(I, T, F, F)
    x2 = colorfilter(x1, FOUR)
    x3 = compose(backdrop, inbox)
    x4 = argmin(x2, size)
    x5 = argmax(x2, size)
    x6 = x3(x4)
    x7 = x3(x5)
    x8 = fill(I, ONE, x6)
    O = fill(x8, TWO, x7)
    return O


def solve_1f642eb9(I):
    x1 = objects(I, T, F, T)
    x2 = sizefilter(x1, ONE)
    x3 = difference(x1, x2)
    x4 = first(x3)
    x5 = rbind(gravitate, x4)
    x6 = compose(crement, x5)
    x7 = fork(shift, identity, x6)
    x8 = mapply(x7, x2)
    O = paint(I, x8)
    return O


def solve_31aa019c(I):
    x1 = leastcolor(I)
    x2 = ofcolor(I, x1)
    x3 = first(x2)
    x4 = neighbors(x3)
    x5 = astuple(TEN, TEN)
    x6 = canvas(ZERO, x5)
    x7 = initset(x3)
    x8 = fill(x6, x1, x7)
    O = fill(x8, TWO, x4)
    return O


def solve_27a28665(I):
    x1 = objects(I, T, F, F)
    x2 = valmax(x1, size)
    x3 = equality(x2, ONE)
    x4 = equality(x2, FOUR)
    x5 = equality(x2, FIVE)
    x6 = branch(x3, TWO, ONE)
    x7 = branch(x4, THREE, x6)
    x8 = branch(x5, SIX, x7)
    O = canvas(x8, UNITY)
    return O


def solve_7ddcd7ec(I):
    x1 = objects(I, T, F, T)
    x2 = sizefilter(x1, ONE)
    x3 = difference(x1, x2)
    x4 = first(x3)
    x5 = color(x4)
    x6 = lbind(position, x4)
    x7 = fork(shoot, center, x6)
    x8 = mapply(x7, x2)
    O = fill(I, x5, x8)
    return O


def solve_3bd67248(I):
    x1 = height(I)
    x2 = decrement(x1)
    x3 = decrement(x2)
    x4 = astuple(x3, ONE)
    x5 = astuple(x2, ONE)
    x6 = shoot(x4, UP_RIGHT)
    x7 = shoot(x5, RIGHT)
    x8 = fill(I, TWO, x6)
    O = fill(x8, FOUR, x7)
    return O


def solve_73251a56(I):
    x1 = dmirror(I)
    x2 = papply(pair, I, x1)
    x3 = lbind(apply, maximum)
    x4 = apply(x3, x2)
    x5 = mostcolor(x4)
    x6 = replace(x4, ZERO, x5)
    x7 = index(x6, ORIGIN)
    x8 = shoot(ORIGIN, UNITY)
    O = fill(x6, x7, x8)
    return O


def solve_25d487eb(I):
    x1 = leastcolor(I)
    x2 = objects(I, T, F, T)
    x3 = ofcolor(I, x1)
    x4 = center(x3)
    x5 = merge(x2)
    x6 = center(x5)
    x7 = subtract(x6, x4)
    x8 = shoot(x4, x7)
    O = underfill(I, x1, x8)
    return O


def solve_8f2ea7aa(I):
    x1 = objects(I, T, F, T)
    x2 = merge(x1)
    x3 = subgrid(x2, I)
    x4 = upscale(x3, THREE)
    x5 = hconcat(x3, x3)
    x6 = hconcat(x5, x3)
    x7 = vconcat(x6, x6)
    x8 = vconcat(x7, x6)
    O = cellwise(x4, x8, ZERO)
    return O


def solve_b8825c91(I):
    x1 = replace(I, FOUR, ZERO)
    x2 = dmirror(x1)
    x3 = papply(pair, x1, x2)
    x4 = lbind(apply, maximum)
    x5 = apply(x4, x3)
    x6 = cmirror(x5)
    x7 = papply(pair, x5, x6)
    x8 = apply(x4, x7)
    O = cmirror(x8)
    return O


def solve_cce03e0d(I):
    x1 = upscale(I, THREE)
    x2 = hconcat(I, I)
    x3 = hconcat(x2, I)
    x4 = vconcat(x3, x3)
    x5 = vconcat(x4, x3)
    x6 = ofcolor(x1, ZERO)
    x7 = ofcolor(x1, ONE)
    x8 = combine(x6, x7)
    O = fill(x5, ZERO, x8)
    return O


def solve_d364b489(I):
    x1 = ofcolor(I, ONE)
    x2 = shift(x1, DOWN)
    x3 = fill(I, EIGHT, x2)
    x4 = shift(x1, UP)
    x5 = fill(x3, TWO, x4)
    x6 = shift(x1, RIGHT)
    x7 = fill(x5, SIX, x6)
    x8 = shift(x1, LEFT)
    O = fill(x7, SEVEN, x8)
    return O


def solve_a5f85a15(I):
    x1 = objects(I, T, T, T)
    x2 = interval(ONE, NINE, ONE)
    x3 = apply(double, x2)
    x4 = apply(decrement, x3)
    x5 = papply(astuple, x4, x4)
    x6 = apply(ulcorner, x1)
    x7 = lbind(shift, x5)
    x8 = mapply(x7, x6)
    O = fill(I, FOUR, x8)
    return O


def solve_3ac3eb23(I):
    x1 = objects(I, T, F, T)
    x2 = chain(ineighbors, last, first)
    x3 = fork(recolor, color, x2)
    x4 = mapply(x3, x1)
    x5 = paint(I, x4)
    x6 = vsplit(x5, THREE)
    x7 = first(x6)
    x8 = vconcat(x7, x7)
    O = vconcat(x7, x8)
    return O


def solve_444801d8(I):
    x1 = objects(I, T, F, T)
    x2 = colorfilter(x1, ONE)
    x3 = rbind(toobject, I)
    x4 = chain(leastcolor, x3, delta)
    x5 = rbind(shift, UP)
    x6 = compose(x5, backdrop)
    x7 = fork(recolor, x4, x6)
    x8 = mapply(x7, x2)
    O = underpaint(I, x8)
    return O


def solve_22168020(I):
    x1 = palette(I)
    x2 = remove(ZERO, x1)
    x3 = lbind(ofcolor, I)
    x4 = lbind(prapply, connect)
    x5 = fork(x4, x3, x3)
    x6 = compose(merge, x5)
    x7 = fork(recolor, identity, x6)
    x8 = mapply(x7, x2)
    O = paint(I, x8)
    return O


def solve_6e82a1ae(I):
    x1 = objects(I, T, F, T)
    x2 = lbind(sizefilter, x1)
    x3 = compose(merge, x2)
    x4 = x3(TWO)
    x5 = x3(THREE)
    x6 = x3(FOUR)
    x7 = fill(I, THREE, x4)
    x8 = fill(x7, TWO, x5)
    O = fill(x8, ONE, x6)
    return O


def solve_b2862040(I):
    x1 = objects(I, T, F, F)
    x2 = colorfilter(x1, NINE)
    x3 = colorfilter(x1, ONE)
    x4 = rbind(bordering, I)
    x5 = compose(flip, x4)
    x6 = mfilter(x2, x5)
    x7 = rbind(adjacent, x6)
    x8 = mfilter(x3, x7)
    O = fill(I, EIGHT, x8)
    return O


def solve_868de0fa(I):
    x1 = objects(I, T, F, F)
    x2 = sfilter(x1, square)
    x3 = compose(even, height)
    x4 = sfilter(x2, x3)
    x5 = difference(x2, x4)
    x6 = merge(x4)
    x7 = merge(x5)
    x8 = fill(I, TWO, x6)
    O = fill(x8, SEVEN, x7)
    return O


def solve_681b3aeb(I):
    x1 = rot270(I)
    x2 = objects(x1, T, F, T)
    x3 = argmax(x2, size)
    x4 = argmin(x2, size)
    x5 = color(x4)
    x6 = canvas(x5, THREE_BY_THREE)
    x7 = normalize(x3)
    x8 = paint(x6, x7)
    O = rot90(x8)
    return O


def solve_8e5a5113(I):
    x1 = crop(I, ORIGIN, THREE_BY_THREE)
    x2 = rot90(x1)
    x3 = rot180(x1)
    x4 = astuple(x2, x3)
    x5 = astuple(FOUR, EIGHT)
    x6 = apply(tojvec, x5)
    x7 = apply(asobject, x4)
    x8 = mpapply(shift, x7, x6)
    O = paint(I, x8)
    return O


def solve_025d127b(I):
    x1 = objects(I, T, F, T)
    x2 = apply(color, x1)
    x3 = merge(x1)
    x4 = lbind(colorfilter, x1)
    x5 = rbind(argmax, rightmost)
    x6 = compose(x5, x4)
    x7 = mapply(x6, x2)
    x8 = difference(x3, x7)
    O = move(I, x8, RIGHT)
    return O


def solve_2281f1f4(I):
    x1 = ofcolor(I, FIVE)
    x2 = product(x1, x1)
    x3 = power(first, TWO)
    x4 = power(last, TWO)
    x5 = fork(astuple, x3, x4)
    x6 = apply(x5, x2)
    x7 = urcorner(x1)
    x8 = remove(x7, x6)
    O = underfill(I, TWO, x8)
    return O


def solve_cf98881b(I):
    x1 = hsplit(I, THREE)
    x2 = first(x1)
    x3 = remove(x2, x1)
    x4 = first(x3)
    x5 = last(x3)
    x6 = ofcolor(x4, NINE)
    x7 = ofcolor(x2, FOUR)
    x8 = fill(x5, NINE, x6)
    O = fill(x8, FOUR, x7)
    return O


def solve_d4f3cd78(I):
    x1 = ofcolor(I, FIVE)
    x2 = delta(x1)
    x3 = fill(I, EIGHT, x2)
    x4 = box(x1)
    x5 = difference(x4, x1)
    x6 = position(x4, x5)
    x7 = first(x5)
    x8 = shoot(x7, x6)
    O = fill(x3, EIGHT, x8)
    return O


def solve_bda2d7a6(I):
    x1 = partition(I)
    x2 = order(x1, size)
    x3 = apply(color, x2)
    x4 = last(x2)
    x5 = remove(x4, x2)
    x6 = repeat(x4, ONE)
    x7 = combine(x6, x5)
    x8 = mpapply(recolor, x3, x7)
    O = paint(I, x8)
    return O


def solve_137eaa0f(I):
    x1 = objects(I, F, T, T)
    x2 = matcher(first, FIVE)
    x3 = rbind(sfilter, x2)
    x4 = chain(invert, center, x3)
    x5 = fork(shift, identity, x4)
    x6 = canvas(ZERO, THREE_BY_THREE)
    x7 = mapply(x5, x1)
    x8 = shift(x7, UNITY)
    O = paint(x6, x8)
    return O


def solve_6455b5f5(I):
    x1 = objects(I, T, F, F)
    x2 = colorfilter(x1, ZERO)
    x3 = argmax(x1, size)
    x4 = valmin(x1, size)
    x5 = sizefilter(x2, x4)
    x6 = recolor(ONE, x3)
    x7 = merge(x5)
    x8 = paint(I, x6)
    O = fill(x8, EIGHT, x7)
    return O


def solve_b8cdaf2b(I):
    x1 = leastcolor(I)
    x2 = ofcolor(I, x1)
    x3 = shift(x2, UP)
    x4 = ulcorner(x3)
    x5 = urcorner(x3)
    x6 = shoot(x4, NEG_UNITY)
    x7 = shoot(x5, UP_RIGHT)
    x8 = combine(x6, x7)
    O = underfill(I, x1, x8)
    return O


def solve_bd4472b8(I):
    x1 = width(I)
    x2 = astuple(TWO, x1)
    x3 = crop(I, ORIGIN, x2)
    x4 = tophalf(x3)
    x5 = dmirror(x4)
    x6 = hupscale(x5, x1)
    x7 = repeat(x6, TWO)
    x8 = merge(x7)
    O = vconcat(x3, x8)
    return O


def solve_4be741c5(I):
    x1 = portrait(I)
    x2 = branch(x1, dmirror, identity)
    x3 = branch(x1, height, width)
    x4 = x3(I)
    x5 = astuple(ONE, x4)
    x6 = x2(I)
    x7 = crop(x6, ORIGIN, x5)
    x8 = apply(dedupe, x7)
    O = x2(x8)
    return O


def solve_bbc9ae5d(I):
    x1 = width(I)
    x2 = palette(I)
    x3 = halve(x1)
    x4 = vupscale(I, x3)
    x5 = rbind(shoot, UNITY)
    x6 = other(x2, ZERO)
    x7 = ofcolor(x4, x6)
    x8 = mapply(x5, x7)
    O = fill(x4, x6, x8)
    return O


def solve_d90796e8(I):
    x1 = objects(I, F, F, T)
    x2 = sizefilter(x1, TWO)
    x3 = lbind(contained, TWO)
    x4 = compose(x3, palette)
    x5 = mfilter(x2, x4)
    x6 = cover(I, x5)
    x7 = matcher(first, THREE)
    x8 = sfilter(x5, x7)
    O = fill(x6, EIGHT, x8)
    return O


def solve_2c608aff(I):
    x1 = leastcolor(I)
    x2 = objects(I, T, F, T)
    x3 = argmax(x2, size)
    x4 = toindices(x3)
    x5 = ofcolor(I, x1)
    x6 = prapply(connect, x4, x5)
    x7 = fork(either, vline, hline)
    x8 = mfilter(x6, x7)
    O = underfill(I, x1, x8)
    return O


def solve_f8b3ba0a(I):
    x1 = compress(I)
    x2 = astuple(THREE, ONE)
    x3 = palette(x1)
    x4 = lbind(colorcount, x1)
    x5 = compose(invert, x4)
    x6 = order(x3, x5)
    x7 = rbind(canvas, UNITY)
    x8 = apply(x7, x6)
    x9 = merge(x8)
    O = crop(x9, DOWN, x2)
    return O


def solve_80af3007(I):
    x1 = objects(I, T, T, T)
    x2 = first(x1)
    x3 = subgrid(x2, I)
    x4 = upscale(x3, THREE)
    x5 = hconcat(x3, x3)
    x6 = hconcat(x5, x3)
    x7 = vconcat(x6, x6)
    x8 = vconcat(x7, x6)
    x9 = cellwise(x4, x8, ZERO)
    O = downscale(x9, THREE)
    return O


def solve_83302e8f(I):
    x1 = objects(I, T, F, F)
    x2 = colorfilter(x1, ZERO)
    x3 = sfilter(x2, square)
    x4 = difference(x2, x3)
    x5 = merge(x3)
    x6 = recolor(THREE, x5)
    x7 = merge(x4)
    x8 = recolor(FOUR, x7)
    x9 = paint(I, x6)
    O = paint(x9, x8)
    return O


def solve_1fad071e(I):
    x1 = objects(I, T, F, T)
    x2 = colorfilter(x1, ONE)
    x3 = sizefilter(x2, FOUR)
    x4 = size(x3)
    x5 = subtract(FIVE, x4)
    x6 = astuple(ONE, x4)
    x7 = canvas(ONE, x6)
    x8 = astuple(ONE, x5)
    x9 = canvas(ZERO, x8)
    O = hconcat(x7, x9)
    return O


def solve_11852cab(I):
    x1 = objects(I, T, T, T)
    x2 = merge(x1)
    x3 = hmirror(x2)
    x4 = vmirror(x2)
    x5 = dmirror(x2)
    x6 = cmirror(x2)
    x7 = paint(I, x3)
    x8 = paint(x7, x4)
    x9 = paint(x8, x5)
    O = paint(x9, x6)
    return O


def solve_3428a4f5(I):
    x1 = tophalf(I)
    x2 = bottomhalf(I)
    x3 = astuple(SIX, FIVE)
    x4 = ofcolor(x1, TWO)
    x5 = ofcolor(x2, TWO)
    x6 = combine(x4, x5)
    x7 = intersection(x4, x5)
    x8 = difference(x6, x7)
    x9 = canvas(ZERO, x3)
    O = fill(x9, THREE, x8)
    return O


def solve_178fcbfb(I):
    x1 = objects(I, T, F, T)
    x2 = ofcolor(I, TWO)
    x3 = mapply(vfrontier, x2)
    x4 = fill(I, TWO, x3)
    x5 = colorfilter(x1, TWO)
    x6 = difference(x1, x5)
    x7 = compose(hfrontier, center)
    x8 = fork(recolor, color, x7)
    x9 = mapply(x8, x6)
    O = paint(x4, x9)
    return O


def solve_3de23699(I):
    x1 = fgpartition(I)
    x2 = sizefilter(x1, FOUR)
    x3 = first(x2)
    x4 = difference(x1, x2)
    x5 = first(x4)
    x6 = color(x3)
    x7 = color(x5)
    x8 = subgrid(x3, I)
    x9 = trim(x8)
    O = replace(x9, x7, x6)
    return O


def solve_54d9e175(I):
    x1 = objects(I, T, F, T)
    x2 = sizefilter(x1, ONE)
    x3 = compose(neighbors, center)
    x4 = fork(recolor, color, x3)
    x5 = mapply(x4, x2)
    x6 = paint(I, x5)
    x7 = replace(x6, ONE, SIX)
    x8 = replace(x7, TWO, SEVEN)
    x9 = replace(x8, THREE, EIGHT)
    O = replace(x9, FOUR, NINE)
    return O


def solve_5ad4f10b(I):
    x1 = objects(I, T, T, T)
    x2 = argmax(x1, size)
    x3 = color(x2)
    x4 = subgrid(x2, I)
    x5 = leastcolor(x4)
    x6 = replace(x4, x5, ZERO)
    x7 = replace(x6, x3, x5)
    x8 = height(x7)
    x9 = divide(x8, THREE)
    O = downscale(x7, x9)
    return O


def solve_623ea044(I):
    x1 = objects(I, T, F, T)
    x2 = first(x1)
    x3 = center(x2)
    x4 = color(x2)
    x5 = astuple(UNITY, NEG_UNITY)
    x6 = astuple(UP_RIGHT, DOWN_LEFT)
    x7 = combine(x5, x6)
    x8 = lbind(shoot, x3)
    x9 = mapply(x8, x7)
    O = fill(I, x4, x9)
    return O


def solve_6b9890af(I):
    x1 = objects(I, T, T, T)
    x2 = ofcolor(I, TWO)
    x3 = argmin(x1, size)
    x4 = subgrid(x2, I)
    x5 = width(x4)
    x6 = divide(x5, THREE)
    x7 = upscale(x3, x6)
    x8 = normalize(x7)
    x9 = shift(x8, UNITY)
    O = paint(x4, x9)
    return O


def solve_794b24be(I):
    x1 = ofcolor(I, ONE)
    x2 = size(x1)
    x3 = decrement(x2)
    x4 = canvas(ZERO, THREE_BY_THREE)
    x5 = tojvec(x3)
    x6 = connect(ORIGIN, x5)
    x7 = equality(x2, FOUR)
    x8 = insert(UNITY, x6)
    x9 = branch(x7, x8, x6)
    O = fill(x4, TWO, x9)
    return O


def solve_88a10436(I):
    x1 = objects(I, F, F, T)
    x2 = colorfilter(x1, FIVE)
    x3 = first(x2)
    x4 = center(x3)
    x5 = difference(x1, x2)
    x6 = first(x5)
    x7 = normalize(x6)
    x8 = shift(x7, x4)
    x9 = shift(x8, NEG_UNITY)
    O = paint(I, x9)
    return O


def solve_88a62173(I):
    x1 = lefthalf(I)
    x2 = righthalf(I)
    x3 = tophalf(x1)
    x4 = tophalf(x2)
    x5 = bottomhalf(x1)
    x6 = bottomhalf(x2)
    x7 = astuple(x3, x4)
    x8 = astuple(x5, x6)
    x9 = combine(x7, x8)
    O = leastcommon(x9)
    return O


def solve_890034e9(I):
    x1 = leastcolor(I)
    x2 = ofcolor(I, x1)
    x3 = inbox(x2)
    x4 = recolor(ZERO, x3)
    x5 = occurrences(I, x4)
    x6 = normalize(x2)
    x7 = shift(x6, NEG_UNITY)
    x8 = lbind(shift, x7)
    x9 = mapply(x8, x5)
    O = fill(I, x1, x9)
    return O


def solve_99b1bc43(I):
    x1 = tophalf(I)
    x2 = bottomhalf(I)
    x3 = ofcolor(x1, ZERO)
    x4 = ofcolor(x2, ZERO)
    x5 = combine(x3, x4)
    x6 = intersection(x3, x4)
    x7 = difference(x5, x6)
    x8 = shape(x1)
    x9 = canvas(ZERO, x8)
    O = fill(x9, THREE, x7)
    return O


def solve_a9f96cdd(I):
    x1 = ofcolor(I, TWO)
    x2 = replace(I, TWO, ZERO)
    x3 = shift(x1, NEG_UNITY)
    x4 = fill(x2, THREE, x3)
    x5 = shift(x1, UP_RIGHT)
    x6 = fill(x4, SIX, x5)
    x7 = shift(x1, DOWN_LEFT)
    x8 = fill(x6, EIGHT, x7)
    x9 = shift(x1, UNITY)
    O = fill(x8, SEVEN, x9)
    return O


def solve_af902bf9(I):
    x1 = ofcolor(I, FOUR)
    x2 = prapply(connect, x1, x1)
    x3 = fork(either, vline, hline)
    x4 = mfilter(x2, x3)
    x5 = underfill(I, NEG_ONE, x4)
    x6 = objects(x5, F, F, T)
    x7 = compose(backdrop, inbox)
    x8 = mapply(x7, x6)
    x9 = fill(x5, TWO, x8)
    O = replace(x9, NEG_ONE, ZERO)
    return O


def solve_b548a754(I):
    x1 = objects(I, T, F, T)
    x2 = replace(I, EIGHT, ZERO)
    x3 = leastcolor(x2)
    x4 = replace(x2, x3, ZERO)
    x5 = leastcolor(x4)
    x6 = merge(x1)
    x7 = backdrop(x6)
    x8 = box(x6)
    x9 = fill(I, x3, x7)
    O = fill(x9, x5, x8)
    return O


def solve_bdad9b1f(I):
    x1 = ofcolor(I, TWO)
    x2 = ofcolor(I, EIGHT)
    x3 = center(x1)
    x4 = center(x2)
    x5 = hfrontier(x3)
    x6 = vfrontier(x4)
    x7 = intersection(x5, x6)
    x8 = fill(I, TWO, x5)
    x9 = fill(x8, EIGHT, x6)
    O = fill(x9, FOUR, x7)
    return O


def solve_c3e719e8(I):
    x1 = mostcolor(I)
    x2 = hconcat(I, I)
    x3 = upscale(I, THREE)
    x4 = ofcolor(x3, x1)
    x5 = asindices(x3)
    x6 = difference(x5, x4)
    x7 = hconcat(x2, I)
    x8 = vconcat(x7, x7)
    x9 = vconcat(x8, x7)
    O = fill(x9, ZERO, x6)
    return O


def solve_de1cd16c(I):
    x1 = leastcolor(I)
    x2 = objects(I, T, F, F)
    x3 = sizefilter(x2, ONE)
    x4 = difference(x2, x3)
    x5 = rbind(subgrid, I)
    x6 = apply(x5, x4)
    x7 = rbind(colorcount, x1)
    x8 = argmax(x6, x7)
    x9 = mostcolor(x8)
    O = canvas(x9, UNITY)
    return O


def solve_d8c310e9(I):
    x1 = objects(I, F, F, T)
    x2 = first(x1)
    x3 = hperiod(x2)
    x4 = multiply(x3, THREE)
    x5 = tojvec(x3)
    x6 = tojvec(x4)
    x7 = shift(x2, x5)
    x8 = shift(x2, x6)
    x9 = paint(I, x7)
    O = paint(x9, x8)
    return O


def solve_a3325580(I):
    x1 = objects(I, T, F, T)
    x2 = valmax(x1, size)
    x3 = sizefilter(x1, x2)
    x4 = order(x3, leftmost)
    x5 = apply(color, x4)
    x6 = astuple(ONE, x2)
    x7 = rbind(canvas, x6)
    x8 = apply(x7, x5)
    x9 = merge(x8)
    O = dmirror(x9)
    return O


def solve_8eb1be9a(I):
    x1 = objects(I, T, T, T)
    x2 = first(x1)
    x3 = interval(NEG_TWO, FOUR, ONE)
    x4 = lbind(shift, x2)
    x5 = height(x2)
    x6 = rbind(multiply, x5)
    x7 = apply(x6, x3)
    x8 = apply(toivec, x7)
    x9 = mapply(x4, x8)
    O = paint(I, x9)
    return O


def solve_321b1fc6(I):
    x1 = objects(I, F, F, T)
    x2 = colorfilter(x1, EIGHT)
    x3 = difference(x1, x2)
    x4 = first(x3)
    x5 = cover(I, x4)
    x6 = normalize(x4)
    x7 = lbind(shift, x6)
    x8 = apply(ulcorner, x2)
    x9 = mapply(x7, x8)
    O = paint(x5, x9)
    return O


def solve_1caeab9d(I):
    x1 = objects(I, T, T, T)
    x2 = ofcolor(I, ONE)
    x3 = lowermost(x2)
    x4 = lbind(subtract, x3)
    x5 = chain(toivec, x4, lowermost)
    x6 = fork(shift, identity, x5)
    x7 = merge(x1)
    x8 = cover(I, x7)
    x9 = mapply(x6, x1)
    O = paint(x8, x9)
    return O


def solve_77fdfe62(I):
    x1 = ofcolor(I, EIGHT)
    x2 = subgrid(x1, I)
    x3 = replace(I, EIGHT, ZERO)
    x4 = replace(x3, ONE, ZERO)
    x5 = compress(x4)
    x6 = width(x2)
    x7 = halve(x6)
    x8 = upscale(x5, x7)
    x9 = ofcolor(x2, ZERO)
    O = fill(x8, ZERO, x9)
    return O


def solve_c0f76784(I):
    x1 = objects(I, T, F, F)
    x2 = colorfilter(x1, ZERO)
    x3 = sfilter(x2, square)
    x4 = sizefilter(x3, ONE)
    x5 = merge(x4)
    x6 = argmax(x3, size)
    x7 = merge(x3)
    x8 = fill(I, SEVEN, x7)
    x9 = fill(x8, EIGHT, x6)
    O = fill(x9, SIX, x5)
    return O


def solve_1b60fb0c(I):
    x1 = rot90(I)
    x2 = ofcolor(I, ONE)
    x3 = ofcolor(x1, ONE)
    x4 = neighbors(ORIGIN)
    x5 = mapply(neighbors, x4)
    x6 = lbind(shift, x3)
    x7 = apply(x6, x5)
    x8 = lbind(intersection, x2)
    x9 = argmax(x7, x8)
    O = underfill(I, TWO, x9)
    return O


def solve_ddf7fa4f(I):
    x1 = objects(I, T, F, T)
    x2 = sizefilter(x1, ONE)
    x3 = colorfilter(x1, FIVE)
    x4 = product(x2, x3)
    x5 = fork(vmatching, first, last)
    x6 = sfilter(x4, x5)
    x7 = compose(color, first)
    x8 = fork(recolor, x7, last)
    x9 = mapply(x8, x6)
    O = paint(I, x9)
    return O


def solve_47c1f68c(I):
    x1 = leastcolor(I)
    x2 = vmirror(I)
    x3 = objects(I, T, T, T)
    x4 = merge(x3)
    x5 = mostcolor(x4)
    x6 = cellwise(I, x2, x1)
    x7 = hmirror(x6)
    x8 = cellwise(x6, x7, x1)
    x9 = compress(x8)
    O = replace(x9, x1, x5)
    return O


def solve_6c434453(I):
    x1 = objects(I, T, F, T)
    x2 = sizefilter(x1, EIGHT)
    x3 = dneighbors(UNITY)
    x4 = insert(UNITY, x3)
    x5 = merge(x2)
    x6 = cover(I, x5)
    x7 = apply(ulcorner, x2)
    x8 = lbind(shift, x4)
    x9 = mapply(x8, x7)
    O = fill(x6, TWO, x9)
    return O


def solve_23581191(I):
    x1 = objects(I, T, T, T)
    x2 = fork(combine, vfrontier, hfrontier)
    x3 = compose(x2, center)
    x4 = fork(recolor, color, x3)
    x5 = mapply(x4, x1)
    x6 = paint(I, x5)
    x7 = fork(intersection, first, last)
    x8 = apply(x3, x1)
    x9 = x7(x8)
    O = fill(x6, TWO, x9)
    return O


def solve_c8cbb738(I):
    x1 = mostcolor(I)
    x2 = fgpartition(I)
    x3 = valmax(x2, shape)
    x4 = canvas(x1, x3)
    x5 = apply(normalize, x2)
    x6 = lbind(subtract, x3)
    x7 = chain(halve, x6, shape)
    x8 = fork(shift, identity, x7)
    x9 = mapply(x8, x5)
    O = paint(x4, x9)
    return O


def solve_3eda0437(I):
    x1 = interval(TWO, TEN, ONE)
    x2 = prapply(astuple, x1, x1)
    x3 = lbind(canvas, ZERO)
    x4 = lbind(occurrences, I)
    x5 = lbind(lbind, shift)
    x6 = fork(apply, x5, x4)
    x7 = chain(x6, asobject, x3)
    x8 = mapply(x7, x2)
    x9 = argmax(x8, size)
    O = fill(I, SIX, x9)
    return O


def solve_dc0a314f(I):
    x1 = ofcolor(I, THREE)
    x2 = replace(I, THREE, ZERO)
    x3 = dmirror(x2)
    x4 = papply(pair, x2, x3)
    x5 = lbind(apply, maximum)
    x6 = apply(x5, x4)
    x7 = cmirror(x6)
    x8 = papply(pair, x6, x7)
    x9 = apply(x5, x8)
    O = subgrid(x1, x9)
    return O


def solve_d4469b4b(I):
    x1 = palette(I)
    x2 = other(x1, ZERO)
    x3 = equality(x2, ONE)
    x4 = equality(x2, TWO)
    x5 = branch(x3, UNITY, TWO_BY_TWO)
    x6 = branch(x4, RIGHT, x5)
    x7 = fork(combine, vfrontier, hfrontier)
    x8 = x7(x6)
    x9 = canvas(ZERO, THREE_BY_THREE)
    O = fill(x9, FIVE, x8)
    return O


def solve_6ecd11f4(I):
    x1 = objects(I, F, T, T)
    x2 = argmax(x1, size)
    x3 = argmin(x1, size)
    x4 = subgrid(x2, I)
    x5 = subgrid(x3, I)
    x6 = width(x4)
    x7 = width(x5)
    x8 = divide(x6, x7)
    x9 = downscale(x4, x8)
    x10 = ofcolor(x9, ZERO)
    O = fill(x5, ZERO, x10)
    return O


def solve_760b3cac(I):
    x1 = ofcolor(I, FOUR)
    x2 = ofcolor(I, EIGHT)
    x3 = ulcorner(x1)
    x4 = index(I, x3)
    x5 = equality(x4, FOUR)
    x6 = branch(x5, NEG_ONE, ONE)
    x7 = multiply(x6, THREE)
    x8 = tojvec(x7)
    x9 = vmirror(x2)
    x10 = shift(x9, x8)
    O = fill(I, EIGHT, x10)
    return O


def solve_c444b776(I):
    x1 = objects(I, T, F, F)
    x2 = colorfilter(x1, ZERO)
    x3 = argmin(x2, size)
    x4 = backdrop(x3)
    x5 = toobject(x4, I)
    x6 = normalize(x5)
    x7 = lbind(shift, x6)
    x8 = compose(x7, ulcorner)
    x9 = remove(x3, x2)
    x10 = mapply(x8, x9)
    O = paint(I, x10)
    return O


def solve_d4a91cb9(I):
    x1 = ofcolor(I, EIGHT)
    x2 = ofcolor(I, TWO)
    x3 = first(x1)
    x4 = first(x2)
    x5 = last(x3)
    x6 = first(x4)
    x7 = astuple(x6, x5)
    x8 = connect(x7, x3)
    x9 = connect(x7, x4)
    x10 = combine(x8, x9)
    O = underfill(I, FOUR, x10)
    return O


def solve_eb281b96(I):
    x1 = height(I)
    x2 = width(I)
    x3 = decrement(x1)
    x4 = astuple(x3, x2)
    x5 = crop(I, ORIGIN, x4)
    x6 = hmirror(x5)
    x7 = vconcat(I, x6)
    x8 = double(x3)
    x9 = astuple(x8, x2)
    x10 = crop(x7, DOWN, x9)
    O = vconcat(x7, x10)
    return O


def solve_ff28f65a(I):
    x1 = objects(I, T, F, T)
    x2 = colorfilter(x1, TWO)
    x3 = size(x2)
    x4 = double(x3)
    x5 = interval(ZERO, x4, TWO)
    x6 = apply(tojvec, x5)
    x7 = astuple(ONE, NINE)
    x8 = canvas(ZERO, x7)
    x9 = fill(x8, ONE, x6)
    x10 = hsplit(x9, THREE)
    O = merge(x10)
    return O


def solve_7e0986d6(I):
    x1 = leastcolor(I)
    x2 = ofcolor(I, x1)
    x3 = replace(I, x1, ZERO)
    x4 = leastcolor(x3)
    x5 = rbind(colorcount, x4)
    x6 = rbind(greater, ONE)
    x7 = compose(x6, x5)
    x8 = rbind(toobject, x3)
    x9 = chain(x7, x8, dneighbors)
    x10 = sfilter(x2, x9)
    O = fill(x3, x4, x10)
    return O


def solve_09629e4f(I):
    x1 = objects(I, F, T, T)
    x2 = rbind(subgrid, I)
    x3 = apply(x2, x1)
    x4 = argmin(x3, numcolors)
    x5 = upscale(x4, FOUR)
    x6 = ofcolor(I, FIVE)
    x7 = shift(x6, UNITY)
    x8 = fill(x5, FIVE, x7)
    x9 = shape(x8)
    x10 = decrement(x9)
    O = crop(x8, UNITY, x10)
    return O


def solve_a85d4709(I):
    x1 = ofcolor(I, FIVE)
    x2 = lbind(matcher, last)
    x3 = lbind(sfilter, x1)
    x4 = lbind(mapply, hfrontier)
    x5 = chain(x4, x3, x2)
    x6 = x5(ZERO)
    x7 = x5(TWO)
    x8 = x5(ONE)
    x9 = fill(I, TWO, x6)
    x10 = fill(x9, THREE, x7)
    O = fill(x10, FOUR, x8)
    return O


def solve_feca6190(I):
    x1 = objects(I, T, F, T)
    x2 = size(x1)
    x3 = multiply(x2, FIVE)
    x4 = astuple(x3, x3)
    x5 = canvas(ZERO, x4)
    x6 = rbind(shoot, UNITY)
    x7 = compose(x6, center)
    x8 = fork(recolor, color, x7)
    x9 = mapply(x8, x1)
    x10 = paint(x5, x9)
    O = hmirror(x10)
    return O


def solve_a68b268e(I):
    x1 = tophalf(I)
    x2 = bottomhalf(I)
    x3 = lefthalf(x1)
    x4 = righthalf(x1)
    x5 = lefthalf(x2)
    x6 = righthalf(x2)
    x7 = ofcolor(x4, FOUR)
    x8 = ofcolor(x3, SEVEN)
    x9 = ofcolor(x5, EIGHT)
    x10 = fill(x6, EIGHT, x9)
    x11 = fill(x10, FOUR, x7)
    O = fill(x11, SEVEN, x8)
    return O


def solve_beb8660c(I):
    x1 = shape(I)
    x2 = objects(I, T, F, T)
    x3 = compose(invert, size)
    x4 = order(x2, x3)
    x5 = apply(normalize, x4)
    x6 = size(x5)
    x7 = interval(ZERO, x6, ONE)
    x8 = apply(toivec, x7)
    x9 = mpapply(shift, x5, x8)
    x10 = canvas(ZERO, x1)
    x11 = paint(x10, x9)
    O = rot180(x11)
    return O


def solve_913fb3ed(I):
    x1 = lbind(ofcolor, I)
    x2 = lbind(mapply, neighbors)
    x3 = chain(x2, x1, last)
    x4 = fork(recolor, first, x3)
    x5 = astuple(SIX, THREE)
    x6 = astuple(FOUR, EIGHT)
    x7 = astuple(ONE, TWO)
    x8 = initset(x5)
    x9 = insert(x6, x8)
    x10 = insert(x7, x9)
    x11 = mapply(x4, x10)
    O = paint(I, x11)
    return O


def solve_0962bcdd(I):
    x1 = leastcolor(I)
    x2 = replace(I, ZERO, x1)
    x3 = leastcolor(x2)
    x4 = ofcolor(I, x3)
    x5 = mapply(dneighbors, x4)
    x6 = fill(I, x3, x5)
    x7 = objects(x6, F, T, T)
    x8 = fork(connect, ulcorner, lrcorner)
    x9 = fork(connect, llcorner, urcorner)
    x10 = fork(combine, x8, x9)
    x11 = mapply(x10, x7)
    O = fill(x6, x1, x11)
    return O


def solve_3631a71a(I):
    x1 = shape(I)
    x2 = replace(I, NINE, ZERO)
    x3 = lbind(apply, maximum)
    x4 = dmirror(x2)
    x5 = papply(pair, x2, x4)
    x6 = apply(x3, x5)
    x7 = subtract(x1, TWO_BY_TWO)
    x8 = crop(x6, TWO_BY_TWO, x7)
    x9 = vmirror(x8)
    x10 = objects(x9, T, F, T)
    x11 = merge(x10)
    x12 = shift(x11, TWO_BY_TWO)
    O = paint(x6, x12)
    return O


def solve_05269061(I):
    x1 = objects(I, T, T, T)
    x2 = neighbors(ORIGIN)
    x3 = mapply(neighbors, x2)
    x4 = rbind(multiply, THREE)
    x5 = apply(x4, x3)
    x6 = merge(x1)
    x7 = lbind(shift, x6)
    x8 = mapply(x7, x5)
    x9 = shift(x8, UP_RIGHT)
    x10 = shift(x8, DOWN_LEFT)
    x11 = paint(I, x8)
    x12 = paint(x11, x9)
    O = paint(x12, x10)
    return O


def solve_95990924(I):
    x1 = objects(I, T, F, T)
    x2 = apply(ulcorner, x1)
    x3 = apply(urcorner, x1)
    x4 = apply(llcorner, x1)
    x5 = apply(lrcorner, x1)
    x6 = shift(x2, NEG_UNITY)
    x7 = shift(x3, UP_RIGHT)
    x8 = shift(x4, DOWN_LEFT)
    x9 = shift(x5, UNITY)
    x10 = fill(I, ONE, x6)
    x11 = fill(x10, TWO, x7)
    x12 = fill(x11, THREE, x8)
    O = fill(x12, FOUR, x9)
    return O


def solve_e509e548(I):
    x1 = objects(I, T, F, T)
    x2 = rbind(subgrid, I)
    x3 = chain(palette, trim, x2)
    x4 = lbind(contained, THREE)
    x5 = compose(x4, x3)
    x6 = fork(add, height, width)
    x7 = compose(decrement, x6)
    x8 = fork(equality, size, x7)
    x9 = mfilter(x1, x5)
    x10 = mfilter(x1, x8)
    x11 = replace(I, THREE, SIX)
    x12 = fill(x11, TWO, x9)
    O = fill(x12, ONE, x10)
    return O


def solve_d43fd935(I):
    x1 = objects(I, T, F, T)
    x2 = ofcolor(I, THREE)
    x3 = sizefilter(x1, ONE)
    x4 = rbind(vmatching, x2)
    x5 = rbind(hmatching, x2)
    x6 = fork(either, x4, x5)
    x7 = sfilter(x3, x6)
    x8 = rbind(gravitate, x2)
    x9 = fork(add, center, x8)
    x10 = fork(connect, center, x9)
    x11 = fork(recolor, color, x10)
    x12 = mapply(x11, x7)
    O = paint(I, x12)
    return O


def solve_db3e9e38(I):
    x1 = ofcolor(I, SEVEN)
    x2 = lrcorner(x1)
    x3 = shoot(x2, UP_RIGHT)
    x4 = shoot(x2, NEG_UNITY)
    x5 = combine(x3, x4)
    x6 = rbind(shoot, UP)
    x7 = mapply(x6, x5)
    x8 = last(x2)
    x9 = rbind(subtract, x8)
    x10 = chain(even, x9, last)
    x11 = fill(I, EIGHT, x7)
    x12 = sfilter(x7, x10)
    O = fill(x11, SEVEN, x12)
    return O


def solve_e73095fd(I):
    x1 = objects(I, T, F, F)
    x2 = colorfilter(x1, ZERO)
    x3 = fork(equality, toindices, backdrop)
    x4 = sfilter(x2, x3)
    x5 = lbind(mapply, dneighbors)
    x6 = chain(x5, corners, outbox)
    x7 = fork(difference, x6, outbox)
    x8 = ofcolor(I, FIVE)
    x9 = rbind(intersection, x8)
    x10 = matcher(size, ZERO)
    x11 = chain(x10, x9, x7)
    x12 = mfilter(x4, x11)
    O = fill(I, FOUR, x12)
    return O


def solve_1bfc4729(I):
    x1 = asindices(I)
    x2 = tophalf(I)
    x3 = bottomhalf(I)
    x4 = leastcolor(x2)
    x5 = leastcolor(x3)
    x6 = ofcolor(x2, x4)
    x7 = first(x6)
    x8 = hfrontier(x7)
    x9 = box(x1)
    x10 = combine(x8, x9)
    x11 = fill(x2, x4, x10)
    x12 = hmirror(x11)
    x13 = replace(x12, x4, x5)
    O = vconcat(x11, x13)
    return O


def solve_93b581b8(I):
    x1 = objects(I, F, F, T)
    x2 = first(x1)
    x3 = dmirror(x2)
    x4 = cmirror(x3)
    x5 = upscale(x4, THREE)
    x6 = astuple(NEG_TWO, NEG_TWO)
    x7 = shift(x5, x6)
    x8 = underpaint(I, x7)
    x9 = toindices(x2)
    x10 = mapply(vfrontier, x9)
    x11 = mapply(hfrontier, x9)
    x12 = combine(x10, x11)
    x13 = fill(x8, ZERO, x12)
    O = paint(x13, x2)
    return O


def solve_9edfc990(I):
    x1 = objects(I, T, F, F)
    x2 = colorfilter(x1, ZERO)
    x3 = ofcolor(I, ONE)
    x4 = rbind(adjacent, x3)
    x5 = mfilter(x2, x4)
    x6 = recolor(ONE, x5)
    x7 = paint(I, x6)
    x8 = add(NINE, FOUR)
    x9 = astuple(SIX, x8)
    x10 = initset(x9)
    x11 = fill(x7, ZERO, x10)
    x12 = index(x7, x9)
    x13 = equality(x12, ONE)
    O = branch(x13, x11, x7)
    return O


def solve_a65b410d(I):
    x1 = ofcolor(I, TWO)
    x2 = urcorner(x1)
    x3 = add(x2, UP_RIGHT)
    x4 = add(x2, DOWN_LEFT)
    x5 = shoot(x3, UP_RIGHT)
    x6 = shoot(x4, DOWN_LEFT)
    x7 = fill(I, THREE, x5)
    x8 = fill(x7, ONE, x6)
    x9 = objects(x8, T, F, T)
    x10 = rbind(shoot, LEFT)
    x11 = compose(x10, urcorner)
    x12 = fork(recolor, color, x11)
    x13 = mapply(x12, x9)
    O = paint(x8, x13)
    return O


def solve_7447852a(I):
    x1 = objects(I, T, F, F)
    x2 = colorfilter(x1, ZERO)
    x3 = compose(last, center)
    x4 = order(x2, x3)
    x5 = size(x4)
    x6 = interval(ZERO, x5, THREE)
    x7 = rbind(contained, x6)
    x8 = compose(x7, last)
    x9 = interval(ZERO, x5, ONE)
    x10 = pair(x4, x9)
    x11 = sfilter(x10, x8)
    x12 = mapply(first, x11)
    x13 = recolor(FOUR, x12)
    O = paint(I, x13)
    return O


def solve_97999447(I):
    x1 = objects(I, T, F, T)
    x2 = apply(toindices, x1)
    x3 = rbind(shoot, RIGHT)
    x4 = compose(x3, center)
    x5 = fork(recolor, color, x4)
    x6 = mapply(x5, x1)
    x7 = paint(I, x6)
    x8 = interval(ZERO, FIVE, ONE)
    x9 = apply(double, x8)
    x10 = apply(increment, x9)
    x11 = apply(tojvec, x10)
    x12 = prapply(shift, x2, x11)
    x13 = merge(x12)
    O = fill(x7, FIVE, x13)
    return O


def solve_91714a58(I):
    x1 = shape(I)
    x2 = asindices(I)
    x3 = objects(I, T, F, T)
    x4 = argmax(x3, size)
    x5 = mostcolor(x4)
    x6 = canvas(ZERO, x1)
    x7 = paint(x6, x4)
    x8 = rbind(toobject, x7)
    x9 = rbind(colorcount, x5)
    x10 = chain(x9, x8, neighbors)
    x11 = lbind(greater, THREE)
    x12 = compose(x11, x10)
    x13 = sfilter(x2, x12)
    O = fill(x7, ZERO, x13)
    return O


def solve_a61ba2ce(I):
    x1 = objects(I, T, F, T)
    x2 = lbind(index, I)
    x3 = matcher(x2, ZERO)
    x4 = lbind(extract, x1)
    x5 = rbind(subgrid, I)
    x6 = lbind(compose, x3)
    x7 = chain(x5, x4, x6)
    x8 = x7(ulcorner)
    x9 = x7(urcorner)
    x10 = x7(llcorner)
    x11 = x7(lrcorner)
    x12 = hconcat(x11, x10)
    x13 = hconcat(x9, x8)
    O = vconcat(x12, x13)
    return O


def solve_8e1813be(I):
    x1 = replace(I, FIVE, ZERO)
    x2 = objects(x1, T, T, T)
    x3 = first(x2)
    x4 = vline(x3)
    x5 = branch(x4, dmirror, identity)
    x6 = x5(x1)
    x7 = objects(x6, T, T, T)
    x8 = order(x7, uppermost)
    x9 = apply(color, x8)
    x10 = dedupe(x9)
    x11 = size(x10)
    x12 = rbind(repeat, x11)
    x13 = apply(x12, x10)
    O = x5(x13)
    return O


def solve_bc1d5164(I):
    x1 = leastcolor(I)
    x2 = crop(I, ORIGIN, THREE_BY_THREE)
    x3 = crop(I, TWO_BY_ZERO, THREE_BY_THREE)
    x4 = tojvec(FOUR)
    x5 = crop(I, x4, THREE_BY_THREE)
    x6 = astuple(TWO, FOUR)
    x7 = crop(I, x6, THREE_BY_THREE)
    x8 = canvas(ZERO, THREE_BY_THREE)
    x9 = rbind(ofcolor, x1)
    x10 = astuple(x2, x3)
    x11 = astuple(x5, x7)
    x12 = combine(x10, x11)
    x13 = mapply(x9, x12)
    O = fill(x8, x1, x13)
    return O


def solve_ce602527(I):
    x1 = vmirror(I)
    x2 = fgpartition(x1)
    x3 = order(x2, size)
    x4 = last(x3)
    x5 = remove(x4, x3)
    x6 = compose(toindices, normalize)
    x7 = rbind(upscale, TWO)
    x8 = chain(toindices, x7, normalize)
    x9 = x6(x4)
    x10 = rbind(intersection, x9)
    x11 = chain(size, x10, x8)
    x12 = argmax(x5, x11)
    x13 = subgrid(x12, x1)
    O = vmirror(x13)
    return O


def solve_5c2c9af4(I):
    x1 = leastcolor(I)
    x2 = ofcolor(I, x1)
    x3 = center(x2)
    x4 = ulcorner(x2)
    x5 = subtract(x3, x4)
    x6 = multiply(NEG_ONE, NINE)
    x7 = interval(ZERO, NINE, ONE)
    x8 = interval(ZERO, x6, NEG_ONE)
    x9 = lbind(multiply, x5)
    x10 = apply(x9, x7)
    x11 = apply(x9, x8)
    x12 = pair(x10, x11)
    x13 = mapply(box, x12)
    x14 = shift(x13, x3)
    O = fill(I, x1, x14)
    return O


def solve_75b8110e(I):
    x1 = lefthalf(I)
    x2 = righthalf(I)
    x3 = tophalf(x1)
    x4 = bottomhalf(x1)
    x5 = tophalf(x2)
    x6 = bottomhalf(x2)
    x7 = rbind(ofcolor, ZERO)
    x8 = fork(difference, asindices, x7)
    x9 = fork(toobject, x8, identity)
    x10 = x9(x5)
    x11 = x9(x4)
    x12 = x9(x6)
    x13 = paint(x3, x12)
    x14 = paint(x13, x11)
    O = paint(x14, x10)
    return O


def solve_941d9a10(I):
    x1 = shape(I)
    x2 = objects(I, T, F, F)
    x3 = colorfilter(x2, ZERO)
    x4 = apply(toindices, x3)
    x5 = lbind(lbind, contained)
    x6 = lbind(extract, x4)
    x7 = compose(x6, x5)
    x8 = decrement(x1)
    x9 = astuple(FIVE, FIVE)
    x10 = x7(ORIGIN)
    x11 = x7(x8)
    x12 = x7(x9)
    x13 = fill(I, ONE, x10)
    x14 = fill(x13, THREE, x11)
    O = fill(x14, TWO, x12)
    return O


def solve_c3f564a4(I):
    x1 = asindices(I)
    x2 = dmirror(I)
    x3 = invert(NINE)
    x4 = papply(pair, I, x2)
    x5 = lbind(apply, maximum)
    x6 = apply(x5, x4)
    x7 = ofcolor(x6, ZERO)
    x8 = difference(x1, x7)
    x9 = toobject(x8, x6)
    x10 = interval(x3, NINE, ONE)
    x11 = interval(NINE, x3, NEG_ONE)
    x12 = pair(x10, x11)
    x13 = lbind(shift, x9)
    x14 = mapply(x13, x12)
    O = paint(x6, x14)
    return O


def solve_1a07d186(I):
    x1 = objects(I, T, F, T)
    x2 = sizefilter(x1, ONE)
    x3 = difference(x1, x2)
    x4 = apply(color, x3)
    x5 = rbind(contained, x4)
    x6 = compose(x5, color)
    x7 = sfilter(x2, x6)
    x8 = lbind(colorfilter, x3)
    x9 = chain(first, x8, color)
    x10 = fork(gravitate, identity, x9)
    x11 = fork(shift, identity, x10)
    x12 = mapply(x11, x7)
    x13 = merge(x2)
    x14 = cover(I, x13)
    O = paint(x14, x12)
    return O


def solve_d687bc17(I):
    x1 = objects(I, T, F, T)
    x2 = sizefilter(x1, ONE)
    x3 = difference(x1, x2)
    x4 = apply(color, x3)
    x5 = rbind(contained, x4)
    x6 = compose(x5, color)
    x7 = sfilter(x2, x6)
    x8 = lbind(colorfilter, x3)
    x9 = chain(first, x8, color)
    x10 = fork(gravitate, identity, x9)
    x11 = fork(shift, identity, x10)
    x12 = merge(x2)
    x13 = mapply(x11, x7)
    x14 = cover(I, x12)
    O = paint(x14, x13)
    return O


def solve_9af7a82c(I):
    x1 = objects(I, T, F, F)
    x2 = order(x1, size)
    x3 = valmax(x1, size)
    x4 = rbind(astuple, ONE)
    x5 = lbind(subtract, x3)
    x6 = compose(x4, size)
    x7 = chain(x4, x5, size)
    x8 = fork(canvas, color, x6)
    x9 = lbind(canvas, ZERO)
    x10 = compose(x9, x7)
    x11 = fork(vconcat, x8, x10)
    x12 = compose(cmirror, x11)
    x13 = apply(x12, x2)
    x14 = merge(x13)
    O = cmirror(x14)
    return O


def solve_6e19193c(I):
    x1 = leastcolor(I)
    x2 = objects(I, T, F, T)
    x3 = rbind(toobject, I)
    x4 = compose(first, delta)
    x5 = rbind(colorcount, x1)
    x6 = matcher(x5, TWO)
    x7 = chain(x6, x3, dneighbors)
    x8 = rbind(sfilter, x7)
    x9 = chain(first, x8, toindices)
    x10 = fork(subtract, x4, x9)
    x11 = fork(shoot, x4, x10)
    x12 = mapply(x11, x2)
    x13 = fill(I, x1, x12)
    x14 = mapply(delta, x2)
    O = fill(x13, ZERO, x14)
    return O


def solve_ef135b50(I):
    x1 = ofcolor(I, TWO)
    x2 = ofcolor(I, ZERO)
    x3 = product(x1, x1)
    x4 = power(first, TWO)
    x5 = compose(first, last)
    x6 = fork(equality, x4, x5)
    x7 = sfilter(x3, x6)
    x8 = fork(connect, first, last)
    x9 = mapply(x8, x7)
    x10 = intersection(x9, x2)
    x11 = fill(I, NINE, x10)
    x12 = trim(x11)
    x13 = asobject(x12)
    x14 = shift(x13, UNITY)
    O = paint(I, x14)
    return O


def solve_cbded52d(I):
    x1 = objects(I, T, F, T)
    x2 = sizefilter(x1, ONE)
    x3 = product(x2, x2)
    x4 = fork(vmatching, first, last)
    x5 = fork(hmatching, first, last)
    x6 = fork(either, x4, x5)
    x7 = sfilter(x3, x6)
    x8 = compose(center, first)
    x9 = compose(center, last)
    x10 = fork(connect, x8, x9)
    x11 = chain(initset, center, x10)
    x12 = compose(color, first)
    x13 = fork(recolor, x12, x11)
    x14 = mapply(x13, x7)
    O = paint(I, x14)
    return O


def solve_8a004b2b(I):
    x1 = objects(I, F, T, T)
    x2 = ofcolor(I, FOUR)
    x3 = subgrid(x2, I)
    x4 = argmax(x1, lowermost)
    x5 = normalize(x4)
    x6 = replace(x3, FOUR, ZERO)
    x7 = objects(x6, T, F, T)
    x8 = merge(x7)
    x9 = width(x8)
    x10 = ulcorner(x8)
    x11 = width(x4)
    x12 = divide(x9, x11)
    x13 = upscale(x5, x12)
    x14 = shift(x13, x10)
    O = paint(x3, x14)
    return O


def solve_e26a3af2(I):
    x1 = rot90(I)
    x2 = apply(mostcommon, I)
    x3 = apply(mostcommon, x1)
    x4 = repeat(x2, ONE)
    x5 = repeat(x3, ONE)
    x6 = compose(size, dedupe)
    x7 = x6(x2)
    x8 = x6(x3)
    x9 = greater(x8, x7)
    x10 = branch(x9, height, width)
    x11 = x10(I)
    x12 = rot90(x4)
    x13 = branch(x9, x5, x12)
    x14 = branch(x9, vupscale, hupscale)
    O = x14(x13, x11)
    return O


def solve_6cf79266(I):
    x1 = ofcolor(I, ZERO)
    x2 = astuple(ZERO, ORIGIN)
    x3 = initset(x2)
    x4 = upscale(x3, THREE)
    x5 = toindices(x4)
    x6 = lbind(shift, x5)
    x7 = rbind(difference, x1)
    x8 = chain(size, x7, x6)
    x9 = matcher(x8, ZERO)
    x10 = lbind(add, NEG_UNITY)
    x11 = chain(flip, x9, x10)
    x12 = fork(both, x9, x11)
    x13 = sfilter(x1, x12)
    x14 = mapply(x6, x13)
    O = fill(I, ONE, x14)
    return O


def solve_a87f7484(I):
    x1 = palette(I)
    x2 = dmirror(I)
    x3 = portrait(I)
    x4 = branch(x3, x2, I)
    x5 = size(x1)
    x6 = decrement(x5)
    x7 = hsplit(x4, x6)
    x8 = rbind(ofcolor, ZERO)
    x9 = apply(x8, x7)
    x10 = mostcommon(x9)
    x11 = matcher(x8, x10)
    x12 = compose(flip, x11)
    x13 = extract(x7, x12)
    x14 = dmirror(x13)
    O = branch(x3, x14, x13)
    return O


def solve_4093f84a(I):
    x1 = leastcolor(I)
    x2 = replace(I, x1, FIVE)
    x3 = rot270(x2)
    x4 = ofcolor(I, FIVE)
    x5 = portrait(x4)
    x6 = branch(x5, x2, x3)
    x7 = lefthalf(x6)
    x8 = righthalf(x6)
    x9 = rbind(order, identity)
    x10 = rbind(order, invert)
    x11 = apply(x9, x7)
    x12 = apply(x10, x8)
    x13 = hconcat(x11, x12)
    x14 = rot90(x13)
    O = branch(x5, x13, x14)
    return O


def solve_ba26e723(I):
    x1 = width(I)
    x2 = hsplit(I, x1)
    x3 = interval(ZERO, x1, ONE)
    x4 = rbind(divide, THREE)
    x5 = rbind(multiply, THREE)
    x6 = compose(x5, x4)
    x7 = fork(equality, identity, x6)
    x8 = apply(x7, x3)
    x9 = rbind(ofcolor, FOUR)
    x10 = apply(x9, x2)
    x11 = apply(tojvec, x3)
    x12 = papply(shift, x10, x11)
    x13 = pair(x8, x12)
    x14 = sfilter(x13, first)
    x15 = mapply(last, x14)
    O = fill(I, SIX, x15)
    return O


def solve_4612dd53(I):
    x1 = ofcolor(I, ONE)
    x2 = box(x1)
    x3 = fill(I, TWO, x2)
    x4 = subgrid(x1, x3)
    x5 = ofcolor(x4, ONE)
    x6 = mapply(vfrontier, x5)
    x7 = mapply(hfrontier, x5)
    x8 = size(x6)
    x9 = size(x7)
    x10 = greater(x8, x9)
    x11 = branch(x10, x7, x6)
    x12 = fill(x4, TWO, x11)
    x13 = ofcolor(x12, TWO)
    x14 = ulcorner(x1)
    x15 = shift(x13, x14)
    O = underfill(I, TWO, x15)
    return O


def solve_29c11459(I):
    x1 = lefthalf(I)
    x2 = righthalf(I)
    x3 = objects(x2, T, F, T)
    x4 = objects(x1, T, F, T)
    x5 = compose(hfrontier, center)
    x6 = fork(recolor, color, x5)
    x7 = mapply(x6, x4)
    x8 = paint(x1, x7)
    x9 = mapply(x6, x3)
    x10 = paint(I, x9)
    x11 = objects(x8, T, F, T)
    x12 = apply(urcorner, x11)
    x13 = shift(x12, RIGHT)
    x14 = merge(x11)
    x15 = paint(x10, x14)
    O = fill(x15, FIVE, x13)
    return O


def solve_963e52fc(I):
    x1 = width(I)
    x2 = asobject(I)
    x3 = hperiod(x2)
    x4 = height(x2)
    x5 = astuple(x4, x3)
    x6 = ulcorner(x2)
    x7 = crop(I, x6, x5)
    x8 = rot90(x7)
    x9 = double(x1)
    x10 = divide(x9, x3)
    x11 = increment(x10)
    x12 = repeat(x8, x11)
    x13 = merge(x12)
    x14 = rot270(x13)
    x15 = astuple(x4, x9)
    O = crop(x14, ORIGIN, x15)
    return O


def solve_ae3edfdc(I):
    x1 = objects(I, T, F, T)
    x2 = replace(I, THREE, ZERO)
    x3 = replace(x2, SEVEN, ZERO)
    x4 = lbind(colorfilter, x1)
    x5 = lbind(rbind, gravitate)
    x6 = chain(x5, first, x4)
    x7 = x6(TWO)
    x8 = x6(ONE)
    x9 = x4(THREE)
    x10 = x4(SEVEN)
    x11 = fork(shift, identity, x7)
    x12 = fork(shift, identity, x8)
    x13 = mapply(x11, x9)
    x14 = mapply(x12, x10)
    x15 = paint(x3, x13)
    O = paint(x15, x14)
    return O


def solve_1f0c79e5(I):
    x1 = ofcolor(I, TWO)
    x2 = replace(I, TWO, ZERO)
    x3 = leastcolor(x2)
    x4 = ofcolor(x2, x3)
    x5 = combine(x1, x4)
    x6 = recolor(x3, x5)
    x7 = compose(decrement, double)
    x8 = ulcorner(x5)
    x9 = invert(x8)
    x10 = shift(x1, x9)
    x11 = apply(x7, x10)
    x12 = interval(ZERO, NINE, ONE)
    x13 = prapply(multiply, x11, x12)
    x14 = lbind(shift, x6)
    x15 = mapply(x14, x13)
    O = paint(I, x15)
    return O


def solve_56dc2b01(I):
    x1 = objects(I, T, F, T)
    x2 = colorfilter(x1, THREE)
    x3 = first(x2)
    x4 = ofcolor(I, TWO)
    x5 = gravitate(x3, x4)
    x6 = first(x5)
    x7 = equality(x6, ZERO)
    x8 = branch(x7, width, height)
    x9 = x8(x3)
    x10 = gravitate(x4, x3)
    x11 = sign(x10)
    x12 = multiply(x11, x9)
    x13 = crement(x12)
    x14 = recolor(EIGHT, x4)
    x15 = shift(x14, x13)
    x16 = paint(I, x15)
    O = move(x16, x3, x5)
    return O


def solve_e48d4e1a(I):
    x1 = shape(I)
    x2 = ofcolor(I, FIVE)
    x3 = fill(I, ZERO, x2)
    x4 = leastcolor(x3)
    x5 = size(x2)
    x6 = ofcolor(I, x4)
    x7 = rbind(toobject, I)
    x8 = rbind(colorcount, x4)
    x9 = chain(x8, x7, dneighbors)
    x10 = matcher(x9, FOUR)
    x11 = extract(x6, x10)
    x12 = multiply(DOWN_LEFT, x5)
    x13 = add(x12, x11)
    x14 = canvas(ZERO, x1)
    x15 = fork(combine, vfrontier, hfrontier)
    x16 = x15(x13)
    O = fill(x14, x4, x16)
    return O


def solve_6773b310(I):
    x1 = compress(I)
    x2 = neighbors(ORIGIN)
    x3 = insert(ORIGIN, x2)
    x4 = rbind(multiply, THREE)
    x5 = apply(x4, x3)
    x6 = astuple(FOUR, FOUR)
    x7 = shift(x5, x6)
    x8 = fork(insert, identity, neighbors)
    x9 = apply(x8, x7)
    x10 = rbind(toobject, x1)
    x11 = apply(x10, x9)
    x12 = rbind(colorcount, SIX)
    x13 = matcher(x12, TWO)
    x14 = mfilter(x11, x13)
    x15 = fill(x1, ONE, x14)
    x16 = replace(x15, SIX, ZERO)
    O = downscale(x16, THREE)
    return O


def solve_780d0b14(I):
    x1 = asindices(I)
    x2 = objects(I, T, T, T)
    x3 = rbind(greater, TWO)
    x4 = compose(x3, size)
    x5 = sfilter(x2, x4)
    x6 = totuple(x5)
    x7 = apply(color, x6)
    x8 = apply(center, x6)
    x9 = pair(x7, x8)
    x10 = fill(I, ZERO, x1)
    x11 = paint(x10, x9)
    x12 = rbind(greater, ONE)
    x13 = compose(dedupe, totuple)
    x14 = chain(x12, size, x13)
    x15 = sfilter(x11, x14)
    x16 = rot90(x15)
    x17 = sfilter(x16, x14)
    O = rot270(x17)
    return O


def solve_2204b7a8(I):
    x1 = objects(I, T, F, T)
    x2 = lbind(sfilter, x1)
    x3 = compose(size, x2)
    x4 = x3(vline)
    x5 = x3(hline)
    x6 = greater(x4, x5)
    x7 = branch(x6, lefthalf, tophalf)
    x8 = branch(x6, righthalf, bottomhalf)
    x9 = branch(x6, hconcat, vconcat)
    x10 = x7(I)
    x11 = x8(I)
    x12 = index(x10, ORIGIN)
    x13 = shape(x11)
    x14 = decrement(x13)
    x15 = index(x11, x14)
    x16 = replace(x10, THREE, x12)
    x17 = replace(x11, THREE, x15)
    O = x9(x16, x17)
    return O


def solve_d9f24cd1(I):
    x1 = ofcolor(I, TWO)
    x2 = ofcolor(I, FIVE)
    x3 = prapply(connect, x1, x2)
    x4 = mfilter(x3, vline)
    x5 = underfill(I, TWO, x4)
    x6 = matcher(numcolors, TWO)
    x7 = objects(x5, F, F, T)
    x8 = sfilter(x7, x6)
    x9 = difference(x7, x8)
    x10 = colorfilter(x9, TWO)
    x11 = mapply(toindices, x10)
    x12 = apply(urcorner, x8)
    x13 = shift(x12, UNITY)
    x14 = rbind(shoot, UP)
    x15 = mapply(x14, x13)
    x16 = fill(x5, TWO, x15)
    x17 = mapply(vfrontier, x11)
    O = fill(x16, TWO, x17)
    return O


def solve_b782dc8a(I):
    x1 = leastcolor(I)
    x2 = objects(I, T, F, F)
    x3 = ofcolor(I, x1)
    x4 = first(x3)
    x5 = dneighbors(x4)
    x6 = toobject(x5, I)
    x7 = mostcolor(x6)
    x8 = ofcolor(I, x7)
    x9 = colorfilter(x2, ZERO)
    x10 = rbind(adjacent, x8)
    x11 = mfilter(x9, x10)
    x12 = toindices(x11)
    x13 = rbind(manhattan, x3)
    x14 = chain(even, x13, initset)
    x15 = sfilter(x12, x14)
    x16 = difference(x12, x15)
    x17 = fill(I, x1, x15)
    O = fill(x17, x7, x16)
    return O


def solve_673ef223(I):
    x1 = objects(I, T, F, T)
    x2 = ofcolor(I, EIGHT)
    x3 = replace(I, EIGHT, FOUR)
    x4 = colorfilter(x1, TWO)
    x5 = argmin(x1, uppermost)
    x6 = apply(uppermost, x4)
    x7 = fork(subtract, maximum, minimum)
    x8 = x7(x6)
    x9 = toivec(x8)
    x10 = leftmost(x5)
    x11 = equality(x10, ZERO)
    x12 = branch(x11, LEFT, RIGHT)
    x13 = rbind(shoot, x12)
    x14 = mapply(x13, x2)
    x15 = underfill(x3, EIGHT, x14)
    x16 = shift(x2, x9)
    x17 = mapply(hfrontier, x16)
    O = underfill(x15, EIGHT, x17)
    return O


def solve_f5b8619d(I):
    x1 = leastcolor(I)
    x2 = width(I)
    x3 = height(I)
    x4 = righthalf(I)
    x5 = halve(x2)
    x6 = even(x2)
    x7 = branch(x6, identity, increment)
    x8 = x7(x5)
    x9 = astuple(x3, x8)
    x10 = crop(I, ORIGIN, x9)
    x11 = vconcat(x10, x10)
    x12 = vconcat(x4, x4)
    x13 = hconcat(x12, x11)
    x14 = hconcat(x11, x13)
    x15 = hconcat(x14, x12)
    x16 = ofcolor(x15, x1)
    x17 = mapply(vfrontier, x16)
    O = underfill(x15, EIGHT, x17)
    return O


def solve_f8c80d96(I):
    x1 = leastcolor(I)
    x2 = objects(I, T, F, F)
    x3 = colorfilter(x2, x1)
    x4 = argmax(x3, size)
    x5 = argmin(x2, width)
    x6 = size(x5)
    x7 = equality(x6, ONE)
    x8 = branch(x7, identity, outbox)
    x9 = chain(outbox, outbox, x8)
    x10 = power(x9, TWO)
    x11 = power(x9, THREE)
    x12 = x9(x4)
    x13 = x10(x4)
    x14 = x11(x4)
    x15 = fill(I, x1, x12)
    x16 = fill(x15, x1, x13)
    x17 = fill(x16, x1, x14)
    O = replace(x17, ZERO, FIVE)
    return O


def solve_ecdecbb3(I):
    x1 = objects(I, T, F, T)
    x2 = colorfilter(x1, TWO)
    x3 = colorfilter(x1, EIGHT)
    x4 = product(x2, x3)
    x5 = fork(gravitate, first, last)
    x6 = compose(crement, x5)
    x7 = compose(center, first)
    x8 = fork(add, x7, x6)
    x9 = fork(connect, x7, x8)
    x10 = apply(x9, x4)
    x11 = lbind(greater, EIGHT)
    x12 = compose(x11, size)
    x13 = mfilter(x10, x12)
    x14 = fill(I, TWO, x13)
    x15 = apply(x8, x4)
    x16 = intersection(x13, x15)
    x17 = mapply(neighbors, x16)
    O = fill(x14, EIGHT, x17)
    return O


def solve_e5062a87(I):
    x1 = ofcolor(I, TWO)
    x2 = recolor(ZERO, x1)
    x3 = normalize(x2)
    x4 = occurrences(I, x2)
    x5 = lbind(shift, x3)
    x6 = apply(x5, x4)
    x7 = astuple(ONE, THREE)
    x8 = astuple(FIVE, ONE)
    x9 = astuple(TWO, SIX)
    x10 = initset(x7)
    x11 = insert(x8, x10)
    x12 = insert(x9, x11)
    x13 = rbind(contained, x12)
    x14 = chain(flip, x13, ulcorner)
    x15 = sfilter(x6, x14)
    x16 = merge(x15)
    x17 = recolor(TWO, x16)
    O = paint(I, x17)
    return O


def solve_a8d7556c(I):
    x1 = initset(ORIGIN)
    x2 = recolor(ZERO, x1)
    x3 = upscale(x2, TWO)
    x4 = occurrences(I, x3)
    x5 = lbind(shift, x3)
    x6 = mapply(x5, x4)
    x7 = fill(I, TWO, x6)
    x8 = add(SIX, SIX)
    x9 = astuple(EIGHT, x8)
    x10 = index(x7, x9)
    x11 = equality(x10, TWO)
    x12 = initset(x9)
    x13 = add(x9, DOWN)
    x14 = insert(x13, x12)
    x15 = toobject(x14, x7)
    x16 = toobject(x14, I)
    x17 = branch(x11, x16, x15)
    O = paint(x7, x17)
    return O


def solve_4938f0c2(I):
    x1 = objects(I, T, T, T)
    x2 = ofcolor(I, TWO)
    x3 = vmirror(x2)
    x4 = height(x2)
    x5 = width(x2)
    x6 = toivec(x4)
    x7 = tojvec(x5)
    x8 = add(x7, ZERO_BY_TWO)
    x9 = add(x6, TWO_BY_ZERO)
    x10 = shift(x3, x8)
    x11 = fill(I, TWO, x10)
    x12 = ofcolor(x11, TWO)
    x13 = hmirror(x12)
    x14 = shift(x13, x9)
    x15 = fill(x11, TWO, x14)
    x16 = size(x1)
    x17 = greater(x16, FOUR)
    O = branch(x17, I, x15)
    return O


def solve_834ec97d(I):
    x1 = asindices(I)
    x2 = objects(I, T, F, T)
    x3 = first(x2)
    x4 = shift(x3, DOWN)
    x5 = fill(I, ZERO, x3)
    x6 = paint(x5, x4)
    x7 = uppermost(x4)
    x8 = leftmost(x4)
    x9 = subtract(x8, TEN)
    x10 = add(x8, TEN)
    x11 = interval(x9, x10, TWO)
    x12 = lbind(greater, x7)
    x13 = compose(x12, first)
    x14 = rbind(contained, x11)
    x15 = compose(x14, last)
    x16 = sfilter(x1, x13)
    x17 = sfilter(x16, x15)
    O = fill(x6, FOUR, x17)
    return O


def solve_846bdb03(I):
    x1 = objects(I, F, F, T)
    x2 = rbind(colorcount, FOUR)
    x3 = matcher(x2, ZERO)
    x4 = extract(x1, x3)
    x5 = remove(x4, x1)
    x6 = merge(x5)
    x7 = subgrid(x6, I)
    x8 = index(x7, DOWN)
    x9 = subgrid(x4, I)
    x10 = lefthalf(x9)
    x11 = palette(x10)
    x12 = other(x11, ZERO)
    x13 = equality(x8, x12)
    x14 = branch(x13, identity, vmirror)
    x15 = x14(x4)
    x16 = normalize(x15)
    x17 = shift(x16, UNITY)
    O = paint(x7, x17)
    return O


def solve_90f3ed37(I):
    x1 = objects(I, T, T, T)
    x2 = order(x1, uppermost)
    x3 = first(x2)
    x4 = remove(x3, x2)
    x5 = normalize(x3)
    x6 = lbind(shift, x5)
    x7 = compose(x6, ulcorner)
    x8 = interval(TWO, NEG_ONE, NEG_ONE)
    x9 = apply(tojvec, x8)
    x10 = rbind(apply, x9)
    x11 = lbind(compose, size)
    x12 = lbind(lbind, intersection)
    x13 = compose(x11, x12)
    x14 = lbind(lbind, shift)
    x15 = chain(x10, x14, x7)
    x16 = fork(argmax, x15, x13)
    x17 = mapply(x16, x4)
    O = underfill(I, ONE, x17)
    return O


def solve_8403a5d5(I):
    x1 = asindices(I)
    x2 = objects(I, T, F, T)
    x3 = first(x2)
    x4 = color(x3)
    x5 = leftmost(x3)
    x6 = interval(x5, TEN, TWO)
    x7 = rbind(contained, x6)
    x8 = compose(x7, last)
    x9 = sfilter(x1, x8)
    x10 = increment(x5)
    x11 = add(x5, THREE)
    x12 = interval(x10, TEN, FOUR)
    x13 = interval(x11, TEN, FOUR)
    x14 = lbind(astuple, NINE)
    x15 = apply(tojvec, x12)
    x16 = apply(x14, x13)
    x17 = fill(I, x4, x9)
    x18 = fill(x17, FIVE, x15)
    O = fill(x18, FIVE, x16)
    return O


def solve_91413438(I):
    x1 = colorcount(I, ZERO)
    x2 = subtract(NINE, x1)
    x3 = multiply(x1, THREE)
    x4 = multiply(x3, x1)
    x5 = subtract(x4, THREE)
    x6 = astuple(THREE, x5)
    x7 = canvas(ZERO, x6)
    x8 = hconcat(I, x7)
    x9 = objects(x8, T, T, T)
    x10 = first(x9)
    x11 = lbind(shift, x10)
    x12 = compose(x11, tojvec)
    x13 = interval(ZERO, x2, ONE)
    x14 = rbind(multiply, THREE)
    x15 = apply(x14, x13)
    x16 = mapply(x12, x15)
    x17 = paint(x8, x16)
    x18 = hsplit(x17, x1)
    O = merge(x18)
    return O


def solve_539a4f51(I):
    x1 = shape(I)
    x2 = index(I, ORIGIN)
    x3 = colorcount(I, ZERO)
    x4 = decrement(x1)
    x5 = positive(x3)
    x6 = branch(x5, x4, x1)
    x7 = crop(I, ORIGIN, x6)
    x8 = width(x7)
    x9 = astuple(ONE, x8)
    x10 = crop(x7, ORIGIN, x9)
    x11 = vupscale(x10, x8)
    x12 = dmirror(x11)
    x13 = hconcat(x7, x11)
    x14 = hconcat(x12, x7)
    x15 = vconcat(x13, x14)
    x16 = asobject(x15)
    x17 = multiply(UNITY, TEN)
    x18 = canvas(x2, x17)
    O = paint(x18, x16)
    return O


def solve_5daaa586(I):
    x1 = objects(I, T, F, F)
    x2 = colorfilter(x1, ZERO)
    x3 = rbind(bordering, I)
    x4 = compose(flip, x3)
    x5 = extract(x2, x4)
    x6 = outbox(x5)
    x7 = subgrid(x6, I)
    x8 = fgpartition(x7)
    x9 = argmax(x8, size)
    x10 = color(x9)
    x11 = toindices(x9)
    x12 = prapply(connect, x11, x11)
    x13 = mfilter(x12, vline)
    x14 = mfilter(x12, hline)
    x15 = size(x13)
    x16 = size(x14)
    x17 = greater(x15, x16)
    x18 = branch(x17, x13, x14)
    O = fill(x7, x10, x18)
    return O


def solve_3bdb4ada(I):
    x1 = objects(I, T, F, T)
    x2 = totuple(x1)
    x3 = compose(increment, ulcorner)
    x4 = compose(decrement, lrcorner)
    x5 = apply(x3, x2)
    x6 = apply(x4, x2)
    x7 = papply(connect, x5, x6)
    x8 = apply(last, x5)
    x9 = compose(last, first)
    x10 = power(last, TWO)
    x11 = fork(subtract, x9, x10)
    x12 = compose(even, x11)
    x13 = lbind(rbind, astuple)
    x14 = lbind(compose, x12)
    x15 = compose(x14, x13)
    x16 = fork(sfilter, first, x15)
    x17 = pair(x7, x8)
    x18 = mapply(x16, x17)
    O = fill(I, ZERO, x18)
    return O


def solve_ec883f72(I):
    x1 = palette(I)
    x2 = objects(I, T, T, T)
    x3 = fork(multiply, height, width)
    x4 = argmax(x2, x3)
    x5 = color(x4)
    x6 = remove(ZERO, x1)
    x7 = other(x6, x5)
    x8 = lrcorner(x4)
    x9 = llcorner(x4)
    x10 = urcorner(x4)
    x11 = ulcorner(x4)
    x12 = shoot(x8, UNITY)
    x13 = shoot(x9, DOWN_LEFT)
    x14 = shoot(x10, UP_RIGHT)
    x15 = shoot(x11, NEG_UNITY)
    x16 = combine(x12, x13)
    x17 = combine(x14, x15)
    x18 = combine(x16, x17)
    O = underfill(I, x7, x18)
    return O


def solve_2bee17df(I):
    x1 = height(I)
    x2 = rot90(I)
    x3 = subtract(x1, TWO)
    x4 = interval(ZERO, x1, ONE)
    x5 = rbind(colorcount, ZERO)
    x6 = matcher(x5, x3)
    x7 = rbind(vsplit, x1)
    x8 = lbind(apply, x6)
    x9 = compose(x8, x7)
    x10 = x9(I)
    x11 = pair(x4, x10)
    x12 = sfilter(x11, last)
    x13 = mapply(hfrontier, x12)
    x14 = x9(x2)
    x15 = pair(x14, x4)
    x16 = sfilter(x15, first)
    x17 = mapply(vfrontier, x16)
    x18 = astuple(x13, x17)
    x19 = merge(x18)
    O = underfill(I, THREE, x19)
    return O


def solve_e8dc4411(I):
    x1 = leastcolor(I)
    x2 = ofcolor(I, ZERO)
    x3 = ofcolor(I, x1)
    x4 = position(x2, x3)
    x5 = fork(connect, ulcorner, lrcorner)
    x6 = x5(x2)
    x7 = intersection(x2, x6)
    x8 = equality(x6, x7)
    x9 = fork(subtract, identity, crement)
    x10 = fork(add, identity, x9)
    x11 = branch(x8, identity, x10)
    x12 = shape(x2)
    x13 = multiply(x12, x4)
    x14 = apply(x11, x13)
    x15 = interval(ONE, FIVE, ONE)
    x16 = lbind(multiply, x14)
    x17 = apply(x16, x15)
    x18 = lbind(shift, x2)
    x19 = mapply(x18, x17)
    O = fill(I, x1, x19)
    return O


def solve_e40b9e2f(I):
    x1 = objects(I, F, T, T)
    x2 = neighbors(ORIGIN)
    x3 = mapply(neighbors, x2)
    x4 = first(x1)
    x5 = lbind(intersection, x4)
    x6 = compose(hmirror, vmirror)
    x7 = x6(x4)
    x8 = lbind(shift, x7)
    x9 = apply(x8, x3)
    x10 = argmax(x9, x5)
    x11 = paint(I, x10)
    x12 = objects(x11, F, T, T)
    x13 = first(x12)
    x14 = compose(size, x5)
    x15 = compose(vmirror, dmirror)
    x16 = x15(x13)
    x17 = lbind(shift, x16)
    x18 = apply(x17, x3)
    x19 = argmax(x18, x14)
    O = paint(x11, x19)
    return O


def solve_29623171(I):
    x1 = leastcolor(I)
    x2 = interval(ZERO, NINE, FOUR)
    x3 = product(x2, x2)
    x4 = rbind(add, THREE)
    x5 = rbind(interval, ONE)
    x6 = fork(x5, identity, x4)
    x7 = compose(x6, first)
    x8 = compose(x6, last)
    x9 = fork(product, x7, x8)
    x10 = rbind(colorcount, x1)
    x11 = rbind(toobject, I)
    x12 = compose(x10, x11)
    x13 = apply(x9, x3)
    x14 = valmax(x13, x12)
    x15 = matcher(x12, x14)
    x16 = compose(flip, x15)
    x17 = mfilter(x13, x15)
    x18 = mfilter(x13, x16)
    x19 = fill(I, x1, x17)
    O = fill(x19, ZERO, x18)
    return O


def solve_a2fd1cf0(I):
    x1 = ofcolor(I, TWO)
    x2 = ofcolor(I, THREE)
    x3 = uppermost(x1)
    x4 = leftmost(x1)
    x5 = uppermost(x2)
    x6 = leftmost(x2)
    x7 = astuple(x3, x5)
    x8 = minimum(x7)
    x9 = maximum(x7)
    x10 = astuple(x8, x6)
    x11 = astuple(x9, x6)
    x12 = connect(x10, x11)
    x13 = astuple(x4, x6)
    x14 = minimum(x13)
    x15 = maximum(x13)
    x16 = astuple(x3, x14)
    x17 = astuple(x3, x15)
    x18 = connect(x16, x17)
    x19 = combine(x12, x18)
    O = underfill(I, EIGHT, x19)
    return O


def solve_b0c4d837(I):
    x1 = ofcolor(I, FIVE)
    x2 = ofcolor(I, EIGHT)
    x3 = height(x1)
    x4 = decrement(x3)
    x5 = height(x2)
    x6 = subtract(x4, x5)
    x7 = astuple(ONE, x6)
    x8 = canvas(EIGHT, x7)
    x9 = subtract(SIX, x6)
    x10 = astuple(ONE, x9)
    x11 = canvas(ZERO, x10)
    x12 = hconcat(x8, x11)
    x13 = hsplit(x12, TWO)
    x14 = first(x13)
    x15 = last(x13)
    x16 = vmirror(x15)
    x17 = vconcat(x14, x16)
    x18 = astuple(ONE, THREE)
    x19 = canvas(ZERO, x18)
    O = vconcat(x17, x19)
    return O


def solve_8731374e(I):
    x1 = objects(I, T, F, F)
    x2 = argmax(x1, size)
    x3 = subgrid(x2, I)
    x4 = height(x3)
    x5 = width(x3)
    x6 = vsplit(x3, x4)
    x7 = lbind(greater, FOUR)
    x8 = compose(x7, numcolors)
    x9 = sfilter(x6, x8)
    x10 = merge(x9)
    x11 = rot90(x10)
    x12 = vsplit(x11, x5)
    x13 = sfilter(x12, x8)
    x14 = merge(x13)
    x15 = rot270(x14)
    x16 = leastcolor(x15)
    x17 = ofcolor(x15, x16)
    x18 = fork(combine, vfrontier, hfrontier)
    x19 = mapply(x18, x17)
    O = fill(x15, x16, x19)
    return O


def solve_272f95fa(I):
    x1 = objects(I, T, F, F)
    x2 = colorfilter(x1, ZERO)
    x3 = apply(toindices, x2)
    x4 = rbind(bordering, I)
    x5 = compose(flip, x4)
    x6 = extract(x3, x5)
    x7 = remove(x6, x3)
    x8 = lbind(vmatching, x6)
    x9 = lbind(hmatching, x6)
    x10 = sfilter(x7, x8)
    x11 = sfilter(x7, x9)
    x12 = argmin(x10, uppermost)
    x13 = argmax(x10, uppermost)
    x14 = argmin(x11, leftmost)
    x15 = argmax(x11, leftmost)
    x16 = fill(I, SIX, x6)
    x17 = fill(x16, TWO, x12)
    x18 = fill(x17, ONE, x13)
    x19 = fill(x18, FOUR, x14)
    O = fill(x19, THREE, x15)
    return O


def solve_db93a21d(I):
    x1 = objects(I, T, T, T)
    x2 = ofcolor(I, NINE)
    x3 = colorfilter(x1, NINE)
    x4 = rbind(shoot, DOWN)
    x5 = mapply(x4, x2)
    x6 = underfill(I, ONE, x5)
    x7 = compose(halve, width)
    x8 = rbind(greater, ONE)
    x9 = compose(x8, x7)
    x10 = matcher(x7, THREE)
    x11 = power(outbox, TWO)
    x12 = power(outbox, THREE)
    x13 = mapply(outbox, x3)
    x14 = sfilter(x3, x9)
    x15 = sfilter(x3, x10)
    x16 = mapply(x11, x14)
    x17 = mapply(x12, x15)
    x18 = fill(x6, THREE, x13)
    x19 = fill(x18, THREE, x16)
    O = fill(x19, THREE, x17)
    return O


def solve_53b68214(I):
    x1 = width(I)
    x2 = objects(I, T, T, T)
    x3 = first(x2)
    x4 = vperiod(x3)
    x5 = toivec(x4)
    x6 = interval(ZERO, NINE, ONE)
    x7 = lbind(multiply, x5)
    x8 = apply(x7, x6)
    x9 = lbind(shift, x3)
    x10 = mapply(x9, x8)
    x11 = astuple(x1, x1)
    x12 = portrait(x3)
    x13 = shape(x3)
    x14 = add(DOWN, x13)
    x15 = decrement(x14)
    x16 = shift(x3, x15)
    x17 = branch(x12, x10, x16)
    x18 = canvas(ZERO, x11)
    x19 = paint(x18, x3)
    O = paint(x19, x17)
    return O


def solve_d6ad076f(I):
    x1 = objects(I, T, F, T)
    x2 = argmin(x1, size)
    x3 = argmax(x1, size)
    x4 = vmatching(x2, x3)
    x5 = branch(x4, DOWN, RIGHT)
    x6 = branch(x4, uppermost, leftmost)
    x7 = valmax(x1, x6)
    x8 = x6(x2)
    x9 = equality(x7, x8)
    x10 = branch(x9, NEG_ONE, ONE)
    x11 = multiply(x5, x10)
    x12 = inbox(x2)
    x13 = rbind(shoot, x11)
    x14 = mapply(x13, x12)
    x15 = underfill(I, EIGHT, x14)
    x16 = objects(x15, T, F, T)
    x17 = colorfilter(x16, EIGHT)
    x18 = rbind(bordering, I)
    x19 = mfilter(x17, x18)
    O = cover(x15, x19)
    return O


def solve_6cdd2623(I):
    x1 = leastcolor(I)
    x2 = height(I)
    x3 = width(I)
    x4 = objects(I, T, F, T)
    x5 = merge(x4)
    x6 = cover(I, x5)
    x7 = ofcolor(I, x1)
    x8 = prapply(connect, x7, x7)
    x9 = merge(x8)
    x10 = decrement(x2)
    x11 = decrement(x3)
    x12 = lbind(greater, x10)
    x13 = lbind(greater, x11)
    x14 = fork(both, positive, x12)
    x15 = compose(x14, first)
    x16 = fork(both, positive, x13)
    x17 = compose(x16, last)
    x18 = fork(both, x15, x17)
    x19 = sfilter(x9, x18)
    x20 = fill(x6, x1, x19)
    O = fill(x20, x1, x7)
    return O


def solve_a3df8b1e(I):
    x1 = shape(I)
    x2 = ofcolor(I, ONE)
    x3 = first(x2)
    x4 = shoot(x3, UP_RIGHT)
    x5 = fill(I, ONE, x4)
    x6 = ofcolor(x5, ONE)
    x7 = urcorner(x6)
    x8 = shoot(x7, NEG_UNITY)
    x9 = fill(x5, ONE, x8)
    x10 = objects(x9, T, T, T)
    x11 = first(x10)
    x12 = subgrid(x11, x9)
    x13 = shape(x12)
    x14 = subtract(x13, DOWN)
    x15 = crop(x12, DOWN, x14)
    x16 = vconcat(x15, x15)
    x17 = vconcat(x16, x16)
    x18 = vconcat(x17, x17)
    x19 = hmirror(x18)
    x20 = crop(x19, ORIGIN, x1)
    O = hmirror(x20)
    return O


def solve_8d510a79(I):
    x1 = height(I)
    x2 = halve(x1)
    x3 = ofcolor(I, ONE)
    x4 = ofcolor(I, TWO)
    x5 = ofcolor(I, FIVE)
    x6 = rbind(gravitate, x5)
    x7 = compose(x6, initset)
    x8 = fork(add, identity, x7)
    x9 = fork(connect, identity, x8)
    x10 = mapply(x9, x4)
    x11 = fill(I, TWO, x10)
    x12 = rbind(greater, x2)
    x13 = compose(x12, first)
    x14 = sfilter(x3, x13)
    x15 = difference(x3, x14)
    x16 = rbind(shoot, UP)
    x17 = rbind(shoot, DOWN)
    x18 = mapply(x16, x15)
    x19 = mapply(x17, x14)
    x20 = combine(x18, x19)
    O = fill(x11, ONE, x20)
    return O


def solve_cdecee7f(I):
    x1 = objects(I, T, F, T)
    x2 = astuple(ONE, THREE)
    x3 = size(x1)
    x4 = order(x1, leftmost)
    x5 = apply(color, x4)
    x6 = rbind(canvas, UNITY)
    x7 = apply(x6, x5)
    x8 = merge(x7)
    x9 = dmirror(x8)
    x10 = subtract(NINE, x3)
    x11 = astuple(ONE, x10)
    x12 = canvas(ZERO, x11)
    x13 = hconcat(x9, x12)
    x14 = hsplit(x13, THREE)
    x15 = merge(x14)
    x16 = crop(x15, ORIGIN, x2)
    x17 = crop(x15, DOWN, x2)
    x18 = crop(x15, TWO_BY_ZERO, x2)
    x19 = vmirror(x17)
    x20 = vconcat(x16, x19)
    O = vconcat(x20, x18)
    return O


def solve_3345333e(I):
    x1 = mostcolor(I)
    x2 = asindices(I)
    x3 = objects(I, F, T, T)
    x4 = first(x3)
    x5 = mostcolor(x4)
    x6 = matcher(first, x5)
    x7 = sfilter(x4, x6)
    x8 = toindices(x7)
    x9 = ulcorner(x7)
    x10 = difference(x2, x8)
    x11 = fill(I, x1, x10)
    x12 = subgrid(x7, x11)
    x13 = vmirror(x12)
    x14 = ofcolor(x13, x5)
    x15 = last(x9)
    x16 = even(x15)
    x17 = invert(x16)
    x18 = tojvec(x17)
    x19 = add(x9, x18)
    x20 = shift(x14, x19)
    O = fill(x11, x5, x20)
    return O


def solve_b190f7f5(I):
    x1 = dmirror(I)
    x2 = portrait(I)
    x3 = branch(x2, x1, I)
    x4 = lefthalf(x3)
    x5 = righthalf(x3)
    x6 = palette(x4)
    x7 = contained(EIGHT, x6)
    x8 = branch(x7, x4, x5)
    x9 = branch(x7, x5, x4)
    x10 = width(x9)
    x11 = upscale(x9, x10)
    x12 = repeat(x8, x10)
    x13 = merge(x12)
    x14 = dmirror(x13)
    x15 = repeat(x14, x10)
    x16 = merge(x15)
    x17 = dmirror(x16)
    x18 = ofcolor(x17, ZERO)
    x19 = fill(x11, ZERO, x18)
    x20 = dmirror(x19)
    O = branch(x2, x20, x19)
    return O


def solve_caa06a1f(I):
    x1 = asobject(I)
    x2 = shape(I)
    x3 = decrement(x2)
    x4 = index(I, x3)
    x5 = double(x2)
    x6 = canvas(x4, x5)
    x7 = paint(x6, x1)
    x8 = objects(x7, F, F, T)
    x9 = first(x8)
    x10 = shift(x9, LEFT)
    x11 = vperiod(x10)
    x12 = hperiod(x10)
    x13 = neighbors(ORIGIN)
    x14 = lbind(mapply, neighbors)
    x15 = power(x14, TWO)
    x16 = x15(x13)
    x17 = astuple(x11, x12)
    x18 = lbind(multiply, x17)
    x19 = apply(x18, x16)
    x20 = lbind(shift, x10)
    x21 = mapply(x20, x19)
    O = paint(I, x21)
    return O


def solve_e21d9049(I):
    x1 = asindices(I)
    x2 = leastcolor(I)
    x3 = objects(I, T, F, T)
    x4 = ofcolor(I, x2)
    x5 = merge(x3)
    x6 = shape(x5)
    x7 = neighbors(ORIGIN)
    x8 = lbind(mapply, neighbors)
    x9 = power(x8, TWO)
    x10 = x9(x7)
    x11 = lbind(multiply, x6)
    x12 = lbind(shift, x5)
    x13 = apply(x11, x10)
    x14 = mapply(x12, x13)
    x15 = lbind(hmatching, x4)
    x16 = lbind(vmatching, x4)
    x17 = fork(either, x15, x16)
    x18 = compose(x17, initset)
    x19 = paint(I, x14)
    x20 = sfilter(x1, x18)
    x21 = difference(x1, x20)
    O = cover(x19, x21)
    return O


def solve_d89b689b(I):
    x1 = asindices(I)
    x2 = ofcolor(I, EIGHT)
    x3 = replace(I, EIGHT, ZERO)
    x4 = vsplit(x3, TWO)
    x5 = rbind(order, leftmost)
    x6 = lbind(apply, color)
    x7 = rbind(repeat, ONE)
    x8 = chain(x7, x6, x5)
    x9 = matcher(first, ZERO)
    x10 = compose(flip, x9)
    x11 = rbind(sfilter, x10)
    x12 = lbind(apply, initset)
    x13 = chain(x12, x11, asobject)
    x14 = compose(x8, x13)
    x15 = apply(x14, x4)
    x16 = merge(x15)
    x17 = ulcorner(x2)
    x18 = asindices(x16)
    x19 = toobject(x18, x16)
    x20 = shift(x19, x17)
    x21 = cover(I, x1)
    O = paint(x21, x20)
    return O


def solve_746b3537(I):
    x1 = rot90(I)
    x2 = objects(I, T, F, F)
    x3 = sfilter(x2, vline)
    x4 = compose(positive, size)
    x5 = x4(x3)
    x6 = branch(x5, x1, I)
    x7 = height(x6)
    x8 = astuple(x7, ONE)
    x9 = crop(x6, ORIGIN, x8)
    x10 = objects(x9, T, F, F)
    x11 = asindices(x9)
    x12 = apply(center, x10)
    x13 = difference(x11, x12)
    x14 = fill(x9, ZERO, x13)
    x15 = vsplit(x14, x7)
    x16 = canvas(ZERO, UNITY)
    x17 = rbind(equality, x16)
    x18 = compose(flip, x17)
    x19 = sfilter(x15, x18)
    x20 = merge(x19)
    x21 = dmirror(x20)
    O = branch(x5, x21, x20)
    return O


def solve_63613498(I):
    x1 = objects(I, F, F, T)
    x2 = crop(I, ORIGIN, THREE_BY_THREE)
    x3 = partition(x2)
    x4 = colorfilter(x3, ZERO)
    x5 = difference(x3, x4)
    x6 = first(x5)
    x7 = toindices(x6)
    x8 = ulcorner(x7)
    x9 = invert(x8)
    x10 = shift(x7, x9)
    x11 = totuple(x1)
    x12 = apply(toindices, x11)
    x13 = apply(normalize, x12)
    x14 = pair(x11, x13)
    x15 = matcher(last, x10)
    x16 = sfilter(x14, x15)
    x17 = matcher(first, x6)
    x18 = compose(flip, x17)
    x19 = extract(x16, x18)
    x20 = first(x19)
    x21 = recolor(FIVE, x20)
    O = paint(I, x21)
    return O


def solve_06df4c85(I):
    x1 = partition(I)
    x2 = mostcolor(I)
    x3 = ofcolor(I, x2)
    x4 = colorfilter(x1, ZERO)
    x5 = argmax(x1, size)
    x6 = difference(x1, x4)
    x7 = remove(x5, x6)
    x8 = merge(x7)
    x9 = product(x8, x8)
    x10 = power(first, TWO)
    x11 = compose(first, last)
    x12 = fork(equality, x10, x11)
    x13 = sfilter(x9, x12)
    x14 = compose(last, first)
    x15 = power(last, TWO)
    x16 = fork(connect, x14, x15)
    x17 = fork(recolor, color, x16)
    x18 = apply(x17, x13)
    x19 = fork(either, vline, hline)
    x20 = mfilter(x18, x19)
    x21 = paint(I, x20)
    O = fill(x21, x2, x3)
    return O


def solve_f9012d9b(I):
    x1 = objects(I, T, F, F)
    x2 = ofcolor(I, ZERO)
    x3 = lbind(contained, ZERO)
    x4 = chain(flip, x3, palette)
    x5 = mfilter(x1, x4)
    x6 = vsplit(I, TWO)
    x7 = hsplit(I, TWO)
    x8 = extract(x6, x4)
    x9 = extract(x7, x4)
    x10 = asobject(x8)
    x11 = asobject(x9)
    x12 = vperiod(x10)
    x13 = hperiod(x11)
    x14 = neighbors(ORIGIN)
    x15 = mapply(neighbors, x14)
    x16 = astuple(x12, x13)
    x17 = rbind(multiply, x16)
    x18 = apply(x17, x15)
    x19 = lbind(shift, x5)
    x20 = mapply(x19, x18)
    x21 = paint(I, x20)
    O = subgrid(x2, x21)
    return O


def solve_4522001f(I):
    x1 = objects(I, F, F, T)
    x2 = first(x1)
    x3 = toindices(x2)
    x4 = contained(ZERO_BY_TWO, x3)
    x5 = contained(TWO_BY_TWO, x3)
    x6 = contained(TWO_BY_ZERO, x3)
    x7 = astuple(NINE, NINE)
    x8 = canvas(ZERO, x7)
    x9 = astuple(THREE, ORIGIN)
    x10 = initset(x9)
    x11 = upscale(x10, TWO)
    x12 = upscale(x11, TWO)
    x13 = shape(x12)
    x14 = shift(x12, x13)
    x15 = combine(x12, x14)
    x16 = paint(x8, x15)
    x17 = rot90(x16)
    x18 = rot180(x16)
    x19 = rot270(x16)
    x20 = branch(x4, x17, x16)
    x21 = branch(x5, x18, x20)
    O = branch(x6, x19, x21)
    return O


def solve_a48eeaf7(I):
    x1 = objects(I, T, F, T)
    x2 = ofcolor(I, TWO)
    x3 = colorfilter(x1, FIVE)
    x4 = rbind(gravitate, x2)
    x5 = fork(shift, identity, x4)
    x6 = mapply(x5, x3)
    x7 = paint(I, x6)
    x8 = rbind(vmatching, x2)
    x9 = rbind(hmatching, x2)
    x10 = fork(either, x8, x9)
    x11 = sfilter(x3, x10)
    x12 = merge(x3)
    x13 = cover(x7, x12)
    x14 = difference(x3, x11)
    x15 = rbind(position, x2)
    x16 = rbind(manhattan, x2)
    x17 = compose(halve, x16)
    x18 = fork(multiply, x17, x15)
    x19 = fork(subtract, x18, x15)
    x20 = fork(shift, identity, x19)
    x21 = mapply(x20, x14)
    O = paint(x13, x21)
    return O


def solve_eb5a1d5d(I):
    x1 = objects(I, T, F, F)
    x2 = argmin(x1, size)
    x3 = color(x2)
    x4 = compose(invert, width)
    x5 = order(x1, x4)
    x6 = apply(color, x5)
    x7 = size(x5)
    x8 = double(x7)
    x9 = decrement(x8)
    x10 = interval(ZERO, x7, ONE)
    x11 = pair(x10, x10)
    x12 = decrement(x9)
    x13 = interval(x12, ZERO, NEG_TWO)
    x14 = papply(add, x13, x10)
    x15 = order(x14, invert)
    x16 = pair(x15, x15)
    x17 = pair(x11, x16)
    x18 = apply(box, x17)
    x19 = mpapply(recolor, x6, x18)
    x20 = astuple(x9, x9)
    x21 = canvas(ZERO, x20)
    x22 = paint(x21, x19)
    O = replace(x22, ZERO, x3)
    return O


def solve_e179c5f4(I):
    x1 = height(I)
    x2 = ofcolor(I, ONE)
    x3 = first(x2)
    x4 = shoot(x3, UP_RIGHT)
    x5 = fill(I, ONE, x4)
    x6 = ofcolor(x5, ONE)
    x7 = urcorner(x6)
    x8 = shoot(x7, NEG_UNITY)
    x9 = fill(x5, ONE, x8)
    x10 = ofcolor(x9, ONE)
    x11 = subgrid(x10, x9)
    x12 = height(x11)
    x13 = width(x11)
    x14 = decrement(x12)
    x15 = astuple(x14, x13)
    x16 = ulcorner(x10)
    x17 = crop(x9, x16, x15)
    x18 = repeat(x17, NINE)
    x19 = merge(x18)
    x20 = astuple(x1, x13)
    x21 = crop(x19, ORIGIN, x20)
    x22 = hmirror(x21)
    O = replace(x22, ZERO, EIGHT)
    return O


def solve_228f6490(I):
    x1 = objects(I, T, F, F)
    x2 = colorfilter(x1, ZERO)
    x3 = rbind(bordering, I)
    x4 = compose(flip, x3)
    x5 = sfilter(x2, x4)
    x6 = first(x5)
    x7 = last(x5)
    x8 = difference(x1, x2)
    x9 = compose(normalize, toindices)
    x10 = x9(x6)
    x11 = x9(x7)
    x12 = matcher(x9, x10)
    x13 = matcher(x9, x11)
    x14 = extract(x8, x12)
    x15 = extract(x8, x13)
    x16 = ulcorner(x6)
    x17 = ulcorner(x7)
    x18 = ulcorner(x14)
    x19 = ulcorner(x15)
    x20 = subtract(x16, x18)
    x21 = subtract(x17, x19)
    x22 = move(I, x14, x20)
    O = move(x22, x15, x21)
    return O


def solve_995c5fa3(I):
    x1 = hsplit(I, THREE)
    x2 = astuple(TWO, ONE)
    x3 = rbind(ofcolor, ZERO)
    x4 = compose(ulcorner, x3)
    x5 = compose(size, x3)
    x6 = matcher(x5, ZERO)
    x7 = matcher(x4, UNITY)
    x8 = matcher(x4, DOWN)
    x9 = matcher(x4, x2)
    x10 = rbind(multiply, THREE)
    x11 = power(double, TWO)
    x12 = compose(double, x6)
    x13 = chain(x11, double, x7)
    x14 = compose(x10, x8)
    x15 = compose(x11, x9)
    x16 = fork(add, x12, x13)
    x17 = fork(add, x14, x15)
    x18 = fork(add, x16, x17)
    x19 = rbind(canvas, UNITY)
    x20 = compose(x19, x18)
    x21 = apply(x20, x1)
    x22 = merge(x21)
    O = hupscale(x22, THREE)
    return O


def solve_d06dbe63(I):
    x1 = ofcolor(I, EIGHT)
    x2 = center(x1)
    x3 = connect(ORIGIN, DOWN)
    x4 = connect(ORIGIN, ZERO_BY_TWO)
    x5 = combine(x3, x4)
    x6 = subtract(x2, TWO_BY_ZERO)
    x7 = shift(x5, x6)
    x8 = astuple(NEG_TWO, TWO)
    x9 = interval(ZERO, FIVE, ONE)
    x10 = lbind(multiply, x8)
    x11 = apply(x10, x9)
    x12 = lbind(shift, x7)
    x13 = mapply(x12, x11)
    x14 = fill(I, FIVE, x13)
    x15 = rot180(x14)
    x16 = ofcolor(x15, EIGHT)
    x17 = center(x16)
    x18 = subtract(x17, x6)
    x19 = shift(x13, x18)
    x20 = toivec(NEG_TWO)
    x21 = shift(x19, x20)
    x22 = fill(x15, FIVE, x21)
    O = rot180(x22)
    return O


def solve_36fdfd69(I):
    x1 = upscale(I, TWO)
    x2 = ofcolor(x1, TWO)
    x3 = mapply(neighbors, x2)
    x4 = difference(x3, x2)
    x5 = fill(x1, FOUR, x4)
    x6 = objects(x5, T, T, T)
    x7 = colorfilter(x6, FOUR)
    x8 = totuple(x7)
    x9 = rbind(subgrid, x5)
    x10 = apply(x9, x8)
    x11 = apply(ulcorner, x8)
    x12 = rbind(ofcolor, TWO)
    x13 = apply(x12, x10)
    x14 = papply(subgrid, x13, x10)
    x15 = rbind(downscale, TWO)
    x16 = apply(x15, x14)
    x17 = apply(asindices, x16)
    x18 = apply(increment, x11)
    x19 = apply(halve, x18)
    x20 = mpapply(shift, x17, x19)
    x21 = fill(I, FOUR, x20)
    x22 = ofcolor(I, TWO)
    O = fill(x21, TWO, x22)
    return O


def solve_0a938d79(I):
    x1 = portrait(I)
    x2 = branch(x1, dmirror, identity)
    x3 = x2(I)
    x4 = objects(x3, T, F, T)
    x5 = argmin(x4, leftmost)
    x6 = argmax(x4, leftmost)
    x7 = color(x5)
    x8 = color(x6)
    x9 = leftmost(x5)
    x10 = leftmost(x6)
    x11 = subtract(x10, x9)
    x12 = double(x11)
    x13 = multiply(THREE, TEN)
    x14 = interval(x9, x13, x12)
    x15 = interval(x10, x13, x12)
    x16 = compose(vfrontier, tojvec)
    x17 = mapply(x16, x14)
    x18 = mapply(x16, x15)
    x19 = recolor(x7, x17)
    x20 = recolor(x8, x18)
    x21 = combine(x19, x20)
    x22 = paint(x3, x21)
    O = x2(x22)
    return O


def solve_045e512c(I):
    x1 = objects(I, T, T, T)
    x2 = apply(size, x1)
    x3 = maximum(x2)
    x4 = lbind(greater, x3)
    x5 = compose(x4, size)
    x6 = sfilter(x1, x5)
    x7 = difference(x1, x6)
    x8 = first(x7)
    x9 = interval(ONE, FOUR, ONE)
    x10 = lbind(multiply, FOUR)
    x11 = apply(x10, x9)
    x12 = product(x6, x11)
    x13 = totuple(x12)
    x14 = apply(first, x13)
    x15 = apply(last, x13)
    x16 = apply(color, x14)
    x17 = lbind(position, x8)
    x18 = apply(x17, x14)
    x19 = papply(multiply, x18, x15)
    x20 = rbind(recolor, x8)
    x21 = apply(x20, x16)
    x22 = mpapply(shift, x21, x19)
    x23 = paint(I, x22)
    O = paint(x23, x8)
    return O


def solve_82819916(I):
    x1 = objects(I, F, F, T)
    x2 = index(I, DOWN)
    x3 = argmax(x1, size)
    x4 = remove(x3, x1)
    x5 = matcher(first, x2)
    x6 = sfilter(x3, x5)
    x7 = difference(x3, x6)
    x8 = lbind(shift, x6)
    x9 = lbind(shift, x7)
    x10 = totuple(x4)
    x11 = apply(ulcorner, x10)
    x12 = apply(urcorner, x10)
    x13 = apply(first, x11)
    x14 = apply(decrement, x13)
    x15 = apply(toivec, x14)
    x16 = lbind(index, I)
    x17 = apply(x16, x11)
    x18 = apply(x16, x12)
    x19 = apply(x8, x15)
    x20 = apply(x9, x15)
    x21 = mpapply(recolor, x17, x19)
    x22 = mpapply(recolor, x18, x20)
    x23 = paint(I, x21)
    O = paint(x23, x22)
    return O


def solve_99fa7670(I):
    x1 = shape(I)
    x2 = objects(I, T, F, T)
    x3 = rbind(shoot, RIGHT)
    x4 = compose(x3, center)
    x5 = fork(recolor, color, x4)
    x6 = mapply(x5, x2)
    x7 = paint(I, x6)
    x8 = add(x1, DOWN_LEFT)
    x9 = initset(x8)
    x10 = recolor(ZERO, x9)
    x11 = objects(x7, T, F, T)
    x12 = insert(x10, x11)
    x13 = order(x12, uppermost)
    x14 = first(x13)
    x15 = remove(x10, x13)
    x16 = remove(x14, x13)
    x17 = compose(lrcorner, first)
    x18 = compose(lrcorner, last)
    x19 = fork(connect, x17, x18)
    x20 = compose(color, first)
    x21 = fork(recolor, x20, x19)
    x22 = pair(x15, x16)
    x23 = mapply(x21, x22)
    O = underpaint(x7, x23)
    return O


def solve_72322fa7(I):
    x1 = objects(I, F, T, T)
    x2 = matcher(numcolors, ONE)
    x3 = sfilter(x1, x2)
    x4 = difference(x1, x3)
    x5 = lbind(matcher, first)
    x6 = compose(x5, mostcolor)
    x7 = fork(sfilter, identity, x6)
    x8 = fork(difference, identity, x7)
    x9 = lbind(occurrences, I)
    x10 = compose(x9, x7)
    x11 = compose(x9, x8)
    x12 = compose(ulcorner, x8)
    x13 = fork(subtract, ulcorner, x12)
    x14 = lbind(rbind, add)
    x15 = compose(x14, x13)
    x16 = fork(apply, x15, x11)
    x17 = lbind(lbind, shift)
    x18 = compose(x17, normalize)
    x19 = fork(mapply, x18, x10)
    x20 = fork(mapply, x18, x16)
    x21 = mapply(x19, x4)
    x22 = mapply(x20, x4)
    x23 = paint(I, x21)
    O = paint(x23, x22)
    return O


def solve_855e0971(I):
    x1 = rot90(I)
    x2 = objects(I, T, F, F)
    x3 = sizefilter(x2, ONE)
    x4 = difference(x2, x3)
    x5 = first(x4)
    x6 = portrait(x5)
    x7 = branch(x6, x1, I)
    x8 = objects(x7, T, F, F)
    x9 = sizefilter(x8, ONE)
    x10 = difference(x8, x9)
    x11 = rbind(subgrid, x7)
    x12 = rbind(ofcolor, ZERO)
    x13 = lbind(mapply, vfrontier)
    x14 = chain(x13, x12, x11)
    x15 = lbind(recolor, ZERO)
    x16 = compose(x15, x14)
    x17 = fork(paint, x11, x16)
    x18 = fork(toobject, asindices, identity)
    x19 = compose(x18, x17)
    x20 = fork(shift, x19, ulcorner)
    x21 = mapply(x20, x10)
    x22 = paint(x7, x21)
    x23 = rot270(x22)
    O = branch(x6, x23, x22)
    return O


def solve_a78176bb(I):
    x1 = palette(I)
    x2 = objects(I, T, F, T)
    x3 = remove(ZERO, x1)
    x4 = other(x3, FIVE)
    x5 = colorfilter(x2, FIVE)
    x6 = lbind(index, I)
    x7 = compose(x6, urcorner)
    x8 = matcher(x7, FIVE)
    x9 = sfilter(x5, x8)
    x10 = difference(x5, x9)
    x11 = apply(urcorner, x9)
    x12 = apply(llcorner, x10)
    x13 = rbind(add, UP_RIGHT)
    x14 = rbind(add, DOWN_LEFT)
    x15 = apply(x13, x11)
    x16 = apply(x14, x12)
    x17 = rbind(shoot, UNITY)
    x18 = rbind(shoot, NEG_UNITY)
    x19 = fork(combine, x17, x18)
    x20 = mapply(x19, x15)
    x21 = mapply(x19, x16)
    x22 = combine(x20, x21)
    x23 = fill(I, x4, x22)
    O = replace(x23, FIVE, ZERO)
    return O


def solve_952a094c(I):
    x1 = objects(I, F, F, T)
    x2 = first(x1)
    x3 = inbox(x2)
    x4 = toobject(x3, I)
    x5 = dmirror(x4)
    x6 = cmirror(x5)
    x7 = paint(I, x6)
    x8 = lbind(index, x7)
    x9 = fork(astuple, x8, identity)
    x10 = compose(initset, x9)
    x11 = double(NEG_UNITY)
    x12 = astuple(NEG_TWO, TWO)
    x13 = astuple(TWO, NEG_TWO)
    x14 = ulcorner(x3)
    x15 = urcorner(x3)
    x16 = llcorner(x3)
    x17 = lrcorner(x3)
    x18 = x10(x14)
    x19 = move(x7, x18, x11)
    x20 = x10(x15)
    x21 = move(x19, x20, x12)
    x22 = x10(x16)
    x23 = move(x21, x22, x13)
    x24 = x10(x17)
    O = move(x23, x24, TWO_BY_TWO)
    return O


def solve_6d58a25d(I):
    x1 = objects(I, T, T, T)
    x2 = argmax(x1, size)
    x3 = remove(x2, x1)
    x4 = first(x3)
    x5 = color(x4)
    x6 = leftmost(x2)
    x7 = rightmost(x2)
    x8 = center(x2)
    x9 = first(x8)
    x10 = rbind(greater, x6)
    x11 = compose(x10, leftmost)
    x12 = lbind(greater, x7)
    x13 = compose(x12, rightmost)
    x14 = rbind(greater, x9)
    x15 = compose(x14, lowermost)
    x16 = fork(both, x11, x13)
    x17 = fork(both, x16, x15)
    x18 = sfilter(x3, x17)
    x19 = mapply(toindices, x18)
    x20 = apply(last, x19)
    x21 = lbind(astuple, x9)
    x22 = apply(x21, x20)
    x23 = rbind(shoot, DOWN)
    x24 = mapply(x23, x22)
    O = underfill(I, x5, x24)
    return O


def solve_6aa20dc0(I):
    x1 = objects(I, F, T, T)
    x2 = argmax(x1, numcolors)
    x3 = normalize(x2)
    x4 = lbind(matcher, first)
    x5 = compose(x4, mostcolor)
    x6 = fork(sfilter, identity, x5)
    x7 = fork(difference, identity, x6)
    x8 = lbind(rbind, upscale)
    x9 = interval(ONE, FOUR, ONE)
    x10 = apply(x8, x9)
    x11 = initset(identity)
    x12 = insert(vmirror, x11)
    x13 = insert(hmirror, x12)
    x14 = insert(cmirror, x13)
    x15 = insert(dmirror, x14)
    x16 = fork(compose, first, last)
    x17 = lbind(occurrences, I)
    x18 = lbind(lbind, shift)
    x19 = compose(x17, x7)
    x20 = product(x15, x10)
    x21 = apply(x16, x20)
    x22 = rapply(x21, x3)
    x23 = fork(mapply, x18, x19)
    x24 = mapply(x23, x22)
    O = paint(I, x24)
    return O


def solve_e6721834(I):
    x1 = portrait(I)
    x2 = branch(x1, vsplit, hsplit)
    x3 = x2(I, TWO)
    x4 = order(x3, numcolors)
    x5 = first(x4)
    x6 = last(x4)
    x7 = objects(x6, F, F, T)
    x8 = merge(x7)
    x9 = mostcolor(x8)
    x10 = matcher(first, x9)
    x11 = compose(flip, x10)
    x12 = rbind(sfilter, x11)
    x13 = lbind(occurrences, x5)
    x14 = compose(x13, x12)
    x15 = chain(positive, size, x14)
    x16 = sfilter(x7, x15)
    x17 = chain(first, x13, x12)
    x18 = compose(ulcorner, x12)
    x19 = fork(subtract, x17, x18)
    x20 = fork(shift, identity, x19)
    x21 = apply(x20, x16)
    x22 = compose(decrement, width)
    x23 = chain(positive, decrement, x22)
    x24 = mfilter(x21, x23)
    O = paint(x5, x24)
    return O


def solve_447fd412(I):
    x1 = objects(I, F, T, T)
    x2 = argmax(x1, numcolors)
    x3 = normalize(x2)
    x4 = lbind(matcher, first)
    x5 = compose(x4, mostcolor)
    x6 = fork(sfilter, identity, x5)
    x7 = fork(difference, identity, x6)
    x8 = lbind(rbind, upscale)
    x9 = interval(ONE, FOUR, ONE)
    x10 = apply(x8, x9)
    x11 = lbind(recolor, ZERO)
    x12 = compose(x11, outbox)
    x13 = fork(combine, identity, x12)
    x14 = lbind(occurrences, I)
    x15 = lbind(rbind, subtract)
    x16 = lbind(apply, increment)
    x17 = lbind(lbind, shift)
    x18 = chain(x15, ulcorner, x7)
    x19 = chain(x14, x13, x7)
    x20 = fork(apply, x18, x19)
    x21 = compose(x16, x20)
    x22 = fork(mapply, x17, x21)
    x23 = rapply(x10, x3)
    x24 = mapply(x22, x23)
    O = paint(I, x24)
    return O


def solve_2bcee788(I):
    x1 = mostcolor(I)
    x2 = objects(I, T, F, T)
    x3 = replace(I, x1, THREE)
    x4 = argmax(x2, size)
    x5 = argmin(x2, size)
    x6 = position(x4, x5)
    x7 = first(x6)
    x8 = last(x6)
    x9 = subgrid(x4, x3)
    x10 = hline(x5)
    x11 = hmirror(x9)
    x12 = vmirror(x9)
    x13 = branch(x10, x11, x12)
    x14 = branch(x10, x7, ZERO)
    x15 = branch(x10, ZERO, x8)
    x16 = asobject(x13)
    x17 = matcher(first, THREE)
    x18 = compose(flip, x17)
    x19 = sfilter(x16, x18)
    x20 = ulcorner(x4)
    x21 = shape(x4)
    x22 = astuple(x14, x15)
    x23 = multiply(x21, x22)
    x24 = add(x20, x23)
    x25 = shift(x19, x24)
    O = paint(x3, x25)
    return O


def solve_776ffc46(I):
    x1 = objects(I, T, F, T)
    x2 = colorfilter(x1, FIVE)
    x3 = totuple(x2)
    x4 = rbind(subgrid, I)
    x5 = apply(x4, x3)
    x6 = multiply(FOUR, SIX)
    x7 = rbind(ofcolor, FIVE)
    x8 = compose(size, x7)
    x9 = matcher(x8, x6)
    x10 = extract(x5, x9)
    x11 = astuple(FIVE, FIVE)
    x12 = crop(x10, UNITY, x11)
    x13 = objects(x12, T, F, T)
    x14 = first(x13)
    x15 = color(x14)
    x16 = normalize(x14)
    x17 = toindices(x16)
    x18 = totuple(x1)
    x19 = apply(normalize, x18)
    x20 = apply(toindices, x19)
    x21 = pair(x18, x20)
    x22 = matcher(last, x17)
    x23 = sfilter(x21, x22)
    x24 = mapply(first, x23)
    x25 = recolor(x15, x24)
    O = paint(I, x25)
    return O


def solve_f35d900a(I):
    x1 = objects(I, T, F, T)
    x2 = merge(x1)
    x3 = hmirror(x2)
    x4 = compose(neighbors, last)
    x5 = fork(recolor, first, x4)
    x6 = mapply(x5, x3)
    x7 = paint(I, x6)
    x8 = outbox(x2)
    x9 = ulcorner(x8)
    x10 = subgrid(x8, x7)
    x11 = dneighbors(ORIGIN)
    x12 = rbind(multiply, FOUR)
    x13 = apply(double, x11)
    x14 = apply(x12, x11)
    x15 = apply(increment, x13)
    x16 = apply(increment, x14)
    x17 = combine(x15, x16)
    x18 = underfill(x10, FIVE, x17)
    x19 = vmirror(x18)
    x20 = underfill(x19, FIVE, x17)
    x21 = ofcolor(x20, FIVE)
    x22 = hmirror(x20)
    x23 = underfill(x22, FIVE, x21)
    x24 = ofcolor(x23, FIVE)
    x25 = shift(x24, x9)
    O = fill(x7, FIVE, x25)
    return O


def solve_0dfd9992(I):
    x1 = height(I)
    x2 = width(I)
    x3 = partition(I)
    x4 = colorfilter(x3, ZERO)
    x5 = difference(x3, x4)
    x6 = merge(x5)
    x7 = astuple(x1, ONE)
    x8 = astuple(ONE, x2)
    x9 = decrement(x1)
    x10 = decrement(x2)
    x11 = toivec(x10)
    x12 = tojvec(x9)
    x13 = crop(I, x11, x8)
    x14 = crop(I, x12, x7)
    x15 = asobject(x14)
    x16 = asobject(x13)
    x17 = vperiod(x15)
    x18 = hperiod(x16)
    x19 = astuple(x17, x18)
    x20 = lbind(multiply, x19)
    x21 = neighbors(ORIGIN)
    x22 = mapply(neighbors, x21)
    x23 = apply(x20, x22)
    x24 = lbind(shift, x6)
    x25 = mapply(x24, x23)
    O = paint(I, x25)
    return O


def solve_29ec7d0e(I):
    x1 = height(I)
    x2 = width(I)
    x3 = partition(I)
    x4 = colorfilter(x3, ZERO)
    x5 = difference(x3, x4)
    x6 = merge(x5)
    x7 = astuple(x1, ONE)
    x8 = astuple(ONE, x2)
    x9 = decrement(x1)
    x10 = decrement(x2)
    x11 = toivec(x10)
    x12 = tojvec(x9)
    x13 = crop(I, x11, x8)
    x14 = crop(I, x12, x7)
    x15 = asobject(x14)
    x16 = asobject(x13)
    x17 = vperiod(x15)
    x18 = hperiod(x16)
    x19 = astuple(x17, x18)
    x20 = lbind(multiply, x19)
    x21 = neighbors(ORIGIN)
    x22 = mapply(neighbors, x21)
    x23 = apply(x20, x22)
    x24 = lbind(shift, x6)
    x25 = mapply(x24, x23)
    O = paint(I, x25)
    return O


def solve_36d67576(I):
    x1 = objects(I, F, F, T)
    x2 = argmax(x1, numcolors)
    x3 = astuple(TWO, FOUR)
    x4 = rbind(contained, x3)
    x5 = compose(x4, first)
    x6 = rbind(sfilter, x5)
    x7 = lbind(rbind, subtract)
    x8 = lbind(occurrences, I)
    x9 = lbind(lbind, shift)
    x10 = compose(x7, ulcorner)
    x11 = chain(x10, x6, normalize)
    x12 = chain(x8, x6, normalize)
    x13 = fork(apply, x11, x12)
    x14 = compose(x9, normalize)
    x15 = fork(mapply, x14, x13)
    x16 = astuple(cmirror, dmirror)
    x17 = astuple(hmirror, vmirror)
    x18 = combine(x16, x17)
    x19 = product(x18, x18)
    x20 = fork(compose, first, last)
    x21 = apply(x20, x19)
    x22 = totuple(x21)
    x23 = combine(x18, x22)
    x24 = rapply(x23, x2)
    x25 = mapply(x15, x24)
    O = paint(I, x25)
    return O


def solve_98cf29f8(I):
    x1 = objects(I, T, F, T)
    x2 = fork(multiply, height, width)
    x3 = fork(equality, size, x2)
    x4 = extract(x1, x3)
    x5 = other(x1, x4)
    x6 = toindices(x5)
    x7 = rbind(add, LEFT)
    x8 = rbind(add, RIGHT)
    x9 = rbind(add, UP)
    x10 = rbind(add, DOWN)
    x11 = initset(ZERO)
    x12 = compose(initset, x8)
    x13 = compose(initset, x10)
    x14 = fork(insert, x7, x12)
    x15 = fork(insert, x9, x13)
    x16 = matcher(palette, x11)
    x17 = rbind(toobject, I)
    x18 = chain(x16, x17, x14)
    x19 = chain(x16, x17, x15)
    x20 = fork(either, x18, x19)
    x21 = sfilter(x6, x20)
    x22 = cover(I, x21)
    x23 = difference(x6, x21)
    x24 = gravitate(x23, x4)
    x25 = toobject(x23, x22)
    O = move(x22, x25, x24)
    return O


def solve_469497ad(I):
    x1 = numcolors(I)
    x2 = decrement(x1)
    x3 = upscale(I, x2)
    x4 = objects(x3, F, F, T)
    x5 = argmin(x4, size)
    x6 = ulcorner(x5)
    x7 = llcorner(x5)
    x8 = shoot(x6, NEG_UNITY)
    x9 = shoot(x6, UNITY)
    x10 = shoot(x7, DOWN_LEFT)
    x11 = shoot(x7, UP_RIGHT)
    x12 = combine(x8, x9)
    x13 = combine(x10, x11)
    x14 = combine(x12, x13)
    x15 = underfill(x3, TWO, x14)
    x16 = objects(x15, T, F, T)
    x17 = argmax(x16, lrcorner)
    x18 = urcorner(x17)
    x19 = tojvec(NEG_TWO)
    x20 = add(x18, x19)
    x21 = connect(x18, x20)
    x22 = toobject(x21, x15)
    x23 = shift(x22, UP)
    x24 = color(x23)
    x25 = equality(x24, SIX)
    x26 = branch(x25, x23, x17)
    O = paint(x15, x26)
    return O


def solve_39e1d7f9(I):
    x1 = fgpartition(I)
    x2 = objects(I, T, F, T)
    x3 = order(x1, height)
    x4 = last(x3)
    x5 = remove(x4, x3)
    x6 = last(x5)
    x7 = color(x6)
    x8 = colorfilter(x2, x7)
    x9 = power(outbox, TWO)
    x10 = rbind(toobject, I)
    x11 = chain(numcolors, x10, x9)
    x12 = argmax(x8, x11)
    x13 = ulcorner(x12)
    x14 = shape(x12)
    x15 = subtract(x13, x14)
    x16 = decrement(x15)
    x17 = multiply(x14, THREE)
    x18 = add(x17, TWO_BY_TWO)
    x19 = crop(I, x16, x18)
    x20 = asobject(x19)
    x21 = apply(ulcorner, x8)
    x22 = increment(x14)
    x23 = rbind(subtract, x22)
    x24 = apply(x23, x21)
    x25 = lbind(shift, x20)
    x26 = mapply(x25, x24)
    O = paint(I, x26)
    return O


def solve_484b58aa(I):
    x1 = height(I)
    x2 = width(I)
    x3 = partition(I)
    x4 = colorfilter(x3, ZERO)
    x5 = difference(x3, x4)
    x6 = merge(x5)
    x7 = astuple(x1, TWO)
    x8 = astuple(TWO, x2)
    x9 = power(decrement, TWO)
    x10 = x9(x1)
    x11 = x9(x2)
    x12 = toivec(x11)
    x13 = tojvec(x10)
    x14 = crop(I, x12, x8)
    x15 = crop(I, x13, x7)
    x16 = asobject(x15)
    x17 = asobject(x14)
    x18 = vperiod(x16)
    x19 = hperiod(x17)
    x20 = astuple(x18, x19)
    x21 = lbind(multiply, x20)
    x22 = neighbors(ORIGIN)
    x23 = mapply(neighbors, x22)
    x24 = apply(x21, x23)
    x25 = lbind(shift, x6)
    x26 = mapply(x25, x24)
    O = paint(I, x26)
    return O


def solve_3befdf3e(I):
    x1 = objects(I, F, F, T)
    x2 = totuple(x1)
    x3 = apply(mostcolor, x2)
    x4 = apply(leastcolor, x2)
    x5 = apply(width, x2)
    x6 = rbind(subtract, TWO)
    x7 = apply(x6, x5)
    x8 = apply(invert, x7)
    x9 = papply(recolor, x4, x2)
    x10 = apply(toivec, x7)
    x11 = mpapply(shift, x9, x10)
    x12 = paint(I, x11)
    x13 = apply(toivec, x8)
    x14 = mpapply(shift, x9, x13)
    x15 = paint(x12, x14)
    x16 = apply(tojvec, x7)
    x17 = mpapply(shift, x9, x16)
    x18 = paint(x15, x17)
    x19 = apply(tojvec, x8)
    x20 = mpapply(shift, x9, x19)
    x21 = paint(x18, x20)
    x22 = merge(x2)
    x23 = paint(x21, x22)
    x24 = first(x4)
    x25 = first(x3)
    x26 = replace(x23, x24, NEG_ONE)
    x27 = replace(x26, x25, x24)
    O = replace(x27, NEG_ONE, x25)
    return O


def solve_9aec4887(I):
    x1 = objects(I, F, T, T)
    x2 = colorfilter(x1, EIGHT)
    x3 = first(x2)
    x4 = other(x1, x3)
    x5 = subgrid(x4, I)
    x6 = normalize(x3)
    x7 = shift(x6, UNITY)
    x8 = paint(x5, x7)
    x9 = palette(x8)
    x10 = remove(ZERO, x9)
    x11 = remove(EIGHT, x10)
    x12 = lbind(ofcolor, x8)
    x13 = rbind(remove, x11)
    x14 = lbind(mapply, x12)
    x15 = lbind(rbind, manhattan)
    x16 = chain(x15, x14, x13)
    x17 = rbind(compose, initset)
    x18 = lbind(lbind, manhattan)
    x19 = chain(x17, x18, x12)
    x20 = compose(x17, x16)
    x21 = lbind(fork, greater)
    x22 = fork(x21, x20, x19)
    x23 = ofcolor(x8, EIGHT)
    x24 = lbind(sfilter, x23)
    x25 = compose(x24, x22)
    x26 = fork(recolor, identity, x25)
    x27 = mapply(x26, x11)
    O = paint(x8, x27)
    return O


def solve_49d1d64f(I):
    x1 = upscale(I, TWO)
    x2 = asindices(x1)
    x3 = corners(x2)
    x4 = fill(x1, ZERO, x3)
    x5 = height(x4)
    x6 = width(x4)
    x7 = equality(x5, SIX)
    x8 = equality(x6, SIX)
    x9 = compose(decrement, halve)
    x10 = x9(x5)
    x11 = x9(x6)
    x12 = astuple(x10, x6)
    x13 = crop(x4, ORIGIN, x12)
    x14 = increment(x10)
    x15 = toivec(x14)
    x16 = astuple(x14, x6)
    x17 = crop(x4, x15, x16)
    x18 = vconcat(x13, x17)
    x19 = branch(x7, x18, x4)
    x20 = decrement(x5)
    x21 = branch(x7, x20, x5)
    x22 = astuple(x21, x11)
    x23 = crop(x19, ORIGIN, x22)
    x24 = increment(x11)
    x25 = tojvec(x24)
    x26 = astuple(x21, x24)
    x27 = crop(x19, x25, x26)
    x28 = hconcat(x23, x27)
    O = branch(x8, x28, x19)
    return O


def solve_57aa92db(I):
    x1 = objects(I, F, T, T)
    x2 = objects(I, T, F, T)
    x3 = lbind(lbind, colorcount)
    x4 = fork(apply, x3, palette)
    x5 = compose(maximum, x4)
    x6 = compose(minimum, x4)
    x7 = fork(subtract, x5, x6)
    x8 = argmax(x1, x7)
    x9 = leastcolor(x8)
    x10 = normalize(x8)
    x11 = matcher(first, x9)
    x12 = sfilter(x10, x11)
    x13 = ulcorner(x12)
    x14 = colorfilter(x2, x9)
    x15 = rbind(toobject, I)
    x16 = lbind(remove, ZERO)
    x17 = chain(first, x16, palette)
    x18 = chain(x17, x15, outbox)
    x19 = lbind(multiply, x13)
    x20 = compose(x19, width)
    x21 = fork(subtract, ulcorner, x20)
    x22 = lbind(shift, x10)
    x23 = compose(x22, x21)
    x24 = fork(upscale, x23, width)
    x25 = fork(recolor, x18, x24)
    x26 = mapply(x25, x14)
    x27 = paint(I, x26)
    x28 = merge(x2)
    O = paint(x27, x28)
    return O


def solve_aba27056(I):
    x1 = objects(I, T, F, T)
    x2 = mapply(toindices, x1)
    x3 = box(x2)
    x4 = difference(x3, x2)
    x5 = delta(x2)
    x6 = position(x5, x4)
    x7 = interval(ZERO, NINE, ONE)
    x8 = lbind(multiply, x6)
    x9 = apply(x8, x7)
    x10 = lbind(shift, x4)
    x11 = mapply(x10, x9)
    x12 = fill(I, FOUR, x5)
    x13 = fill(x12, FOUR, x11)
    x14 = corners(x4)
    x15 = ofcolor(x13, ZERO)
    x16 = rbind(toobject, x13)
    x17 = rbind(colorcount, ZERO)
    x18 = chain(x17, x16, dneighbors)
    x19 = matcher(x18, TWO)
    x20 = rbind(adjacent, x2)
    x21 = rbind(adjacent, x11)
    x22 = fork(both, x20, x21)
    x23 = compose(x22, initset)
    x24 = sfilter(x15, x19)
    x25 = sfilter(x24, x23)
    x26 = product(x14, x25)
    x27 = fork(subtract, last, first)
    x28 = fork(shoot, first, x27)
    x29 = mapply(x28, x26)
    O = fill(x13, FOUR, x29)
    return O


def solve_f1cefba8(I):
    x1 = palette(I)
    x2 = objects(I, F, F, T)
    x3 = ofcolor(I, ZERO)
    x4 = first(x2)
    x5 = ulcorner(x4)
    x6 = subgrid(x4, I)
    x7 = power(trim, TWO)
    x8 = x7(x6)
    x9 = asindices(x8)
    x10 = shift(x9, TWO_BY_TWO)
    x11 = fill(x6, ZERO, x10)
    x12 = leastcolor(x11)
    x13 = remove(ZERO, x1)
    x14 = other(x13, x12)
    x15 = ofcolor(x11, x12)
    x16 = shift(x15, x5)
    x17 = ofcolor(I, x12)
    x18 = uppermost(x17)
    x19 = lowermost(x17)
    x20 = matcher(first, x18)
    x21 = matcher(first, x19)
    x22 = fork(either, x20, x21)
    x23 = sfilter(x16, x22)
    x24 = difference(x16, x23)
    x25 = mapply(vfrontier, x23)
    x26 = mapply(hfrontier, x24)
    x27 = combine(x25, x26)
    x28 = intersection(x3, x27)
    x29 = fill(I, x14, x27)
    O = fill(x29, x12, x28)
    return O


def solve_1e32b0e9(I):
    x1 = height(I)
    x2 = mostcolor(I)
    x3 = asobject(I)
    x4 = subtract(x1, TWO)
    x5 = divide(x4, THREE)
    x6 = astuple(x5, x5)
    x7 = crop(I, ORIGIN, x6)
    x8 = partition(x7)
    x9 = matcher(color, ZERO)
    x10 = compose(flip, x9)
    x11 = extract(x8, x10)
    x12 = initset(x2)
    x13 = palette(x3)
    x14 = palette(x11)
    x15 = difference(x13, x14)
    x16 = difference(x15, x12)
    x17 = first(x16)
    x18 = interval(ZERO, THREE, ONE)
    x19 = product(x18, x18)
    x20 = totuple(x19)
    x21 = apply(first, x20)
    x22 = apply(last, x20)
    x23 = lbind(multiply, x5)
    x24 = apply(x23, x21)
    x25 = apply(x23, x22)
    x26 = papply(add, x24, x21)
    x27 = papply(add, x25, x22)
    x28 = papply(astuple, x26, x27)
    x29 = lbind(shift, x11)
    x30 = mapply(x29, x28)
    O = underfill(I, x17, x30)
    return O


def solve_28e73c20(I):
    x1 = width(I)
    x2 = astuple(ONE, TWO)
    x3 = astuple(TWO, TWO)
    x4 = astuple(TWO, ONE)
    x5 = astuple(THREE, ONE)
    x6 = canvas(THREE, UNITY)
    x7 = upscale(x6, FOUR)
    x8 = initset(DOWN)
    x9 = insert(UNITY, x8)
    x10 = insert(x2, x9)
    x11 = insert(x3, x10)
    x12 = fill(x7, ZERO, x11)
    x13 = vupscale(x6, FIVE)
    x14 = hupscale(x13, THREE)
    x15 = insert(x4, x9)
    x16 = insert(x5, x15)
    x17 = fill(x14, ZERO, x16)
    x18 = even(x1)
    x19 = branch(x18, x12, x17)
    x20 = canvas(ZERO, UNITY)
    x21 = lbind(hupscale, x20)
    x22 = chain(x21, decrement, height)
    x23 = rbind(hconcat, x6)
    x24 = compose(x23, x22)
    x25 = lbind(hupscale, x6)
    x26 = compose(x25, height)
    x27 = fork(vconcat, x24, rot90)
    x28 = fork(vconcat, x26, x27)
    x29 = subtract(x1, FOUR)
    x30 = power(x28, x29)
    O = x30(x19)
    return O


def solve_4c5c2cf0(I):
    x1 = objects(I, T, T, T)
    x2 = objects(I, F, T, T)
    x3 = first(x2)
    x4 = rbind(subgrid, I)
    x5 = fork(equality, identity, rot90)
    x6 = compose(x5, x4)
    x7 = extract(x1, x6)
    x8 = center(x7)
    x9 = subgrid(x3, I)
    x10 = hmirror(x9)
    x11 = objects(x10, F, T, T)
    x12 = first(x11)
    x13 = objects(x10, T, T, T)
    x14 = extract(x13, x6)
    x15 = center(x14)
    x16 = subtract(x8, x15)
    x17 = shift(x12, x16)
    x18 = paint(I, x17)
    x19 = objects(x18, F, T, T)
    x20 = first(x19)
    x21 = subgrid(x20, x18)
    x22 = vmirror(x21)
    x23 = objects(x22, F, T, T)
    x24 = first(x23)
    x25 = objects(x22, T, T, T)
    x26 = color(x7)
    x27 = matcher(color, x26)
    x28 = extract(x25, x27)
    x29 = center(x28)
    x30 = subtract(x8, x29)
    x31 = shift(x24, x30)
    O = paint(x18, x31)
    return O


def solve_508bd3b6(I):
    x1 = width(I)
    x2 = objects(I, T, T, T)
    x3 = argmin(x2, size)
    x4 = argmax(x2, size)
    x5 = ulcorner(x3)
    x6 = urcorner(x3)
    x7 = index(I, x5)
    x8 = equality(x7, EIGHT)
    x9 = branch(x8, x5, x6)
    x10 = branch(x8, UNITY, DOWN_LEFT)
    x11 = multiply(x10, x1)
    x12 = double(x11)
    x13 = add(x9, x12)
    x14 = subtract(x9, x12)
    x15 = connect(x13, x14)
    x16 = fill(I, THREE, x15)
    x17 = paint(x16, x4)
    x18 = objects(x17, T, F, T)
    x19 = rbind(adjacent, x4)
    x20 = extract(x18, x19)
    x21 = first(x20)
    x22 = last(x21)
    x23 = flip(x8)
    x24 = branch(x23, UNITY, DOWN_LEFT)
    x25 = multiply(x24, x1)
    x26 = double(x25)
    x27 = add(x22, x26)
    x28 = subtract(x22, x26)
    x29 = connect(x27, x28)
    x30 = fill(x17, THREE, x29)
    x31 = paint(x30, x3)
    O = paint(x31, x4)
    return O


def solve_6d0160f0(I):
    x1 = ofcolor(I, FOUR)
    x2 = first(x1)
    x3 = first(x2)
    x4 = last(x2)
    x5 = greater(x3, THREE)
    x6 = greater(x3, SEVEN)
    x7 = greater(x4, THREE)
    x8 = greater(x4, SEVEN)
    x9 = branch(x5, FOUR, ZERO)
    x10 = branch(x6, EIGHT, x9)
    x11 = branch(x7, FOUR, ZERO)
    x12 = branch(x8, EIGHT, x11)
    x13 = astuple(x10, x12)
    x14 = initset(ZERO)
    x15 = insert(FOUR, x14)
    x16 = insert(EIGHT, x15)
    x17 = product(x16, x16)
    x18 = crop(I, ORIGIN, THREE_BY_THREE)
    x19 = asindices(x18)
    x20 = recolor(ZERO, x19)
    x21 = lbind(shift, x20)
    x22 = mapply(x21, x17)
    x23 = paint(I, x22)
    x24 = crop(I, x13, THREE_BY_THREE)
    x25 = replace(x24, FIVE, ZERO)
    x26 = ofcolor(x25, FOUR)
    x27 = first(x26)
    x28 = asindices(x25)
    x29 = toobject(x28, x25)
    x30 = multiply(x27, FOUR)
    x31 = shift(x29, x30)
    O = paint(x23, x31)
    return O


def solve_f8a8fe49(I):
    x1 = objects(I, T, F, T)
    x2 = replace(I, FIVE, ZERO)
    x3 = colorfilter(x1, TWO)
    x4 = first(x3)
    x5 = portrait(x4)
    x6 = branch(x5, hsplit, vsplit)
    x7 = branch(x5, vmirror, hmirror)
    x8 = ofcolor(I, TWO)
    x9 = subgrid(x8, I)
    x10 = trim(x9)
    x11 = x7(x10)
    x12 = x6(x11, TWO)
    x13 = compose(normalize, asobject)
    x14 = apply(x13, x12)
    x15 = last(x14)
    x16 = first(x14)
    x17 = ulcorner(x8)
    x18 = increment(x17)
    x19 = shift(x15, x18)
    x20 = shift(x16, x18)
    x21 = branch(x5, width, height)
    x22 = branch(x5, tojvec, toivec)
    x23 = x21(x15)
    x24 = double(x23)
    x25 = compose(x22, increment)
    x26 = x25(x23)
    x27 = invert(x26)
    x28 = x25(x24)
    x29 = shift(x19, x27)
    x30 = shift(x20, x28)
    x31 = paint(x2, x29)
    O = paint(x31, x30)
    return O


def solve_d07ae81c(I):
    x1 = objects(I, T, F, F)
    x2 = sizefilter(x1, ONE)
    x3 = apply(color, x2)
    x4 = difference(x1, x2)
    x5 = apply(color, x4)
    x6 = first(x5)
    x7 = last(x5)
    x8 = ofcolor(I, x6)
    x9 = ofcolor(I, x7)
    x10 = rbind(shoot, UNITY)
    x11 = rbind(shoot, NEG_UNITY)
    x12 = rbind(shoot, DOWN_LEFT)
    x13 = rbind(shoot, UP_RIGHT)
    x14 = fork(combine, x10, x11)
    x15 = fork(combine, x12, x13)
    x16 = fork(combine, x14, x15)
    x17 = compose(x16, center)
    x18 = mapply(x17, x2)
    x19 = intersection(x8, x18)
    x20 = intersection(x9, x18)
    x21 = first(x2)
    x22 = color(x21)
    x23 = center(x21)
    x24 = neighbors(x23)
    x25 = toobject(x24, I)
    x26 = mostcolor(x25)
    x27 = other(x3, x22)
    x28 = equality(x26, x6)
    x29 = branch(x28, x22, x27)
    x30 = branch(x28, x27, x22)
    x31 = fill(I, x29, x19)
    O = fill(x31, x30, x20)
    return O


def solve_6a1e5592(I):
    x1 = width(I)
    x2 = objects(I, T, F, T)
    x3 = astuple(FIVE, x1)
    x4 = crop(I, ORIGIN, x3)
    x5 = colorfilter(x2, FIVE)
    x6 = merge(x5)
    x7 = cover(I, x6)
    x8 = compose(toindices, normalize)
    x9 = apply(x8, x5)
    x10 = asindices(x4)
    x11 = ofcolor(x4, ZERO)
    x12 = ofcolor(x4, TWO)
    x13 = rbind(multiply, TEN)
    x14 = rbind(multiply, EIGHT)
    x15 = rbind(intersection, x12)
    x16 = rbind(intersection, x11)
    x17 = rbind(intersection, x10)
    x18 = chain(x13, size, x15)
    x19 = chain(size, x16, delta)
    x20 = compose(x14, uppermost)
    x21 = chain(size, x16, outbox)
    x22 = chain(x13, size, x17)
    x23 = compose(invert, x18)
    x24 = fork(add, x22, x23)
    x25 = fork(subtract, x24, x21)
    x26 = fork(subtract, x25, x20)
    x27 = fork(subtract, x26, x19)
    x28 = rbind(apply, x10)
    x29 = lbind(lbind, shift)
    x30 = rbind(argmax, x27)
    x31 = chain(x30, x28, x29)
    x32 = mapply(x31, x9)
    O = fill(x7, ONE, x32)
    return O


def solve_0e206a2e(I):
    x1 = palette(I)
    x2 = objects(I, F, F, T)
    x3 = rbind(greater, ONE)
    x4 = compose(x3, numcolors)
    x5 = sfilter(x2, x4)
    x6 = remove(ZERO, x1)
    x7 = lbind(colorcount, I)
    x8 = argmax(x6, x7)
    x9 = remove(x8, x6)
    x10 = rbind(contained, x9)
    x11 = compose(x10, first)
    x12 = rbind(sfilter, x11)
    x13 = lbind(rbind, subtract)
    x14 = lbind(occurrences, I)
    x15 = lbind(lbind, shift)
    x16 = compose(x13, ulcorner)
    x17 = chain(x16, x12, normalize)
    x18 = chain(x14, x12, normalize)
    x19 = fork(apply, x17, x18)
    x20 = compose(x15, normalize)
    x21 = fork(mapply, x20, x19)
    x22 = astuple(cmirror, dmirror)
    x23 = astuple(hmirror, vmirror)
    x24 = combine(x22, x23)
    x25 = product(x24, x24)
    x26 = fork(compose, first, last)
    x27 = apply(x26, x25)
    x28 = totuple(x27)
    x29 = combine(x24, x28)
    x30 = lbind(rapply, x29)
    x31 = mapply(x30, x5)
    x32 = mapply(x21, x31)
    x33 = paint(I, x32)
    x34 = merge(x5)
    O = cover(x33, x34)
    return O


def solve_d22278a0(I):
    x1 = asindices(I)
    x2 = objects(I, T, F, T)
    x3 = fork(multiply, sign, identity)
    x4 = lbind(apply, x3)
    x5 = chain(even, maximum, x4)
    x6 = lbind(sfilter, x1)
    x7 = fork(add, first, last)
    x8 = rbind(remove, x2)
    x9 = compose(center, last)
    x10 = fork(subtract, first, x9)
    x11 = compose(x5, x10)
    x12 = lbind(rbind, equality)
    x13 = lbind(argmin, x2)
    x14 = chain(x7, x4, x10)
    x15 = lbind(lbind, astuple)
    x16 = lbind(rbind, astuple)
    x17 = lbind(compose, x11)
    x18 = lbind(compose, x14)
    x19 = compose(x18, x15)
    x20 = compose(x18, x16)
    x21 = compose(x13, x19)
    x22 = rbind(compose, x21)
    x23 = lbind(lbind, valmin)
    x24 = rbind(compose, x19)
    x25 = chain(x24, x23, x8)
    x26 = lbind(fork, greater)
    x27 = fork(x26, x25, x20)
    x28 = chain(x6, x17, x16)
    x29 = chain(x6, x22, x12)
    x30 = fork(intersection, x28, x29)
    x31 = compose(x6, x27)
    x32 = fork(intersection, x30, x31)
    x33 = fork(recolor, color, x32)
    x34 = mapply(x33, x2)
    O = paint(I, x34)
    return O


def solve_4290ef0e(I):
    x1 = mostcolor(I)
    x2 = fgpartition(I)
    x3 = objects(I, T, F, T)
    x4 = rbind(valmax, width)
    x5 = lbind(colorfilter, x3)
    x6 = chain(x4, x5, color)
    x7 = compose(maximum, shape)
    x8 = fork(add, x7, x6)
    x9 = compose(invert, x8)
    x10 = order(x2, x9)
    x11 = rbind(argmin, centerofmass)
    x12 = compose(initset, vmirror)
    x13 = fork(insert, dmirror, x12)
    x14 = fork(insert, cmirror, x13)
    x15 = fork(insert, hmirror, x14)
    x16 = compose(x11, x15)
    x17 = apply(x16, x10)
    x18 = size(x2)
    x19 = apply(size, x2)
    x20 = contained(ONE, x19)
    x21 = increment(x18)
    x22 = branch(x20, x18, x21)
    x23 = double(x22)
    x24 = decrement(x23)
    x25 = apply(normalize, x17)
    x26 = interval(ZERO, x22, ONE)
    x27 = pair(x26, x26)
    x28 = mpapply(shift, x25, x27)
    x29 = astuple(x24, x24)
    x30 = canvas(x1, x29)
    x31 = paint(x30, x28)
    x32 = rot90(x31)
    x33 = paint(x32, x28)
    x34 = rot90(x33)
    x35 = paint(x34, x28)
    x36 = rot90(x35)
    O = paint(x36, x28)
    return O


def solve_50846271(I):
    x1 = ofcolor(I, TWO)
    x2 = prapply(connect, x1, x1)
    x3 = lbind(greater, SIX)
    x4 = compose(x3, size)
    x5 = fork(either, vline, hline)
    x6 = fork(both, x4, x5)
    x7 = mfilter(x2, x6)
    x8 = fill(I, TWO, x7)
    x9 = objects(x8, T, F, F)
    x10 = colorfilter(x9, TWO)
    x11 = valmax(x10, width)
    x12 = halve(x11)
    x13 = toivec(x12)
    x14 = tojvec(x12)
    x15 = rbind(add, ZERO_BY_TWO)
    x16 = rbind(add, TWO_BY_ZERO)
    x17 = rbind(subtract, ZERO_BY_TWO)
    x18 = rbind(subtract, TWO_BY_ZERO)
    x19 = rbind(colorcount, TWO)
    x20 = rbind(toobject, x8)
    x21 = compose(initset, x15)
    x22 = fork(insert, x16, x21)
    x23 = fork(insert, x17, x22)
    x24 = fork(insert, x18, x23)
    x25 = fork(combine, dneighbors, x24)
    x26 = chain(x19, x20, x25)
    x27 = rbind(argmax, x26)
    x28 = compose(x27, toindices)
    x29 = apply(x28, x10)
    x30 = rbind(add, x13)
    x31 = rbind(subtract, x13)
    x32 = rbind(add, x14)
    x33 = rbind(subtract, x14)
    x34 = fork(connect, x30, x31)
    x35 = fork(connect, x32, x33)
    x36 = fork(combine, x34, x35)
    x37 = mapply(x36, x29)
    x38 = fill(x8, EIGHT, x37)
    O = fill(x38, TWO, x1)
    return O


def solve_b527c5c6(I):
    x1 = objects(I, F, F, T)
    x2 = matcher(first, TWO)
    x3 = rbind(sfilter, x2)
    x4 = compose(lowermost, x3)
    x5 = compose(rightmost, x3)
    x6 = compose(uppermost, x3)
    x7 = compose(leftmost, x3)
    x8 = fork(equality, x4, lowermost)
    x9 = fork(equality, x5, rightmost)
    x10 = fork(equality, x6, uppermost)
    x11 = fork(equality, x7, leftmost)
    x12 = compose(invert, x10)
    x13 = compose(invert, x11)
    x14 = fork(add, x12, x8)
    x15 = fork(add, x13, x9)
    x16 = fork(astuple, x14, x15)
    x17 = compose(center, x3)
    x18 = fork(shoot, x17, x16)
    x19 = mapply(x18, x1)
    x20 = fill(I, TWO, x19)
    x21 = compose(vline, x18)
    x22 = sfilter(x1, x21)
    x23 = difference(x1, x22)
    x24 = chain(decrement, minimum, shape)
    x25 = compose(increment, x24)
    x26 = compose(invert, x24)
    x27 = rbind(interval, ONE)
    x28 = fork(x27, x26, x25)
    x29 = lbind(apply, toivec)
    x30 = lbind(apply, tojvec)
    x31 = lbind(lbind, shift)
    x32 = compose(x31, x18)
    x33 = compose(x29, x28)
    x34 = compose(x30, x28)
    x35 = fork(mapply, x32, x33)
    x36 = fork(mapply, x32, x34)
    x37 = mapply(x35, x23)
    x38 = mapply(x36, x22)
    x39 = combine(x37, x38)
    O = underfill(x20, THREE, x39)
    return O


def solve_150deff5(I):
    x1 = canvas(FIVE, TWO_BY_TWO)
    x2 = asobject(x1)
    x3 = occurrences(I, x2)
    x4 = lbind(shift, x2)
    x5 = mapply(x4, x3)
    x6 = fill(I, EIGHT, x5)
    x7 = canvas(FIVE, UNITY)
    x8 = astuple(TWO, ONE)
    x9 = canvas(EIGHT, x8)
    x10 = vconcat(x9, x7)
    x11 = asobject(x10)
    x12 = occurrences(x6, x11)
    x13 = lbind(shift, x11)
    x14 = mapply(x13, x12)
    x15 = fill(x6, TWO, x14)
    x16 = astuple(ONE, THREE)
    x17 = canvas(FIVE, x16)
    x18 = asobject(x17)
    x19 = occurrences(x15, x18)
    x20 = lbind(shift, x18)
    x21 = mapply(x20, x19)
    x22 = fill(x15, TWO, x21)
    x23 = hmirror(x10)
    x24 = asobject(x23)
    x25 = occurrences(x22, x24)
    x26 = lbind(shift, x24)
    x27 = mapply(x26, x25)
    x28 = fill(x22, TWO, x27)
    x29 = dmirror(x10)
    x30 = asobject(x29)
    x31 = occurrences(x28, x30)
    x32 = lbind(shift, x30)
    x33 = mapply(x32, x31)
    x34 = fill(x28, TWO, x33)
    x35 = vmirror(x29)
    x36 = asobject(x35)
    x37 = occurrences(x34, x36)
    x38 = lbind(shift, x36)
    x39 = mapply(x38, x37)
    O = fill(x34, TWO, x39)
    return O


def solve_b7249182(I):
    x1 = objects(I, T, F, T)
    x2 = merge(x1)
    x3 = portrait(x2)
    x4 = branch(x3, identity, dmirror)
    x5 = x4(I)
    x6 = objects(x5, T, F, T)
    x7 = order(x6, uppermost)
    x8 = first(x7)
    x9 = last(x7)
    x10 = color(x8)
    x11 = color(x9)
    x12 = compose(first, toindices)
    x13 = x12(x8)
    x14 = x12(x9)
    x15 = connect(x13, x14)
    x16 = centerofmass(x15)
    x17 = connect(x13, x16)
    x18 = fill(x5, x11, x15)
    x19 = fill(x18, x10, x17)
    x20 = add(x16, DOWN)
    x21 = initset(x16)
    x22 = insert(x20, x21)
    x23 = toobject(x22, x19)
    x24 = astuple(ZERO, NEG_TWO)
    x25 = shift(x23, ZERO_BY_TWO)
    x26 = shift(x23, x24)
    x27 = combine(x25, x26)
    x28 = ulcorner(x27)
    x29 = urcorner(x27)
    x30 = connect(x28, x29)
    x31 = shift(x30, UP)
    x32 = llcorner(x27)
    x33 = lrcorner(x27)
    x34 = connect(x32, x33)
    x35 = shift(x34, DOWN)
    x36 = paint(x19, x27)
    x37 = fill(x36, x10, x31)
    x38 = fill(x37, x11, x35)
    x39 = cover(x38, x22)
    O = x4(x39)
    return O


def solve_9d9215db(I):
    x1 = rot90(I)
    x2 = rot180(I)
    x3 = rot270(I)
    x4 = initset(I)
    x5 = chain(numcolors, lefthalf, tophalf)
    x6 = insert(x1, x4)
    x7 = insert(x2, x6)
    x8 = insert(x3, x7)
    x9 = argmax(x8, x5)
    x10 = vmirror(x9)
    x11 = papply(pair, x9, x10)
    x12 = lbind(apply, maximum)
    x13 = apply(x12, x11)
    x14 = partition(x13)
    x15 = sizefilter(x14, FOUR)
    x16 = apply(llcorner, x15)
    x17 = apply(lrcorner, x15)
    x18 = combine(x16, x17)
    x19 = cover(x13, x18)
    x20 = tojvec(NEG_TWO)
    x21 = rbind(add, ZERO_BY_TWO)
    x22 = rbind(add, x20)
    x23 = compose(x21, ulcorner)
    x24 = compose(x22, urcorner)
    x25 = fork(connect, x23, x24)
    x26 = compose(even, last)
    x27 = rbind(sfilter, x26)
    x28 = chain(normalize, x27, x25)
    x29 = fork(shift, x28, x23)
    x30 = fork(recolor, color, x29)
    x31 = mapply(x30, x15)
    x32 = paint(x19, x31)
    x33 = rot90(x32)
    x34 = rot180(x32)
    x35 = rot270(x32)
    x36 = papply(pair, x32, x33)
    x37 = apply(x12, x36)
    x38 = papply(pair, x37, x34)
    x39 = apply(x12, x38)
    x40 = papply(pair, x39, x35)
    O = apply(x12, x40)
    return O


def solve_6855a6e4(I):
    x1 = fgpartition(I)
    x2 = rot90(I)
    x3 = colorfilter(x1, TWO)
    x4 = first(x3)
    x5 = portrait(x4)
    x6 = branch(x5, I, x2)
    x7 = objects(x6, T, F, T)
    x8 = colorfilter(x7, FIVE)
    x9 = apply(center, x8)
    x10 = valmin(x9, first)
    x11 = compose(first, center)
    x12 = matcher(x11, x10)
    x13 = compose(flip, x12)
    x14 = extract(x8, x12)
    x15 = extract(x8, x13)
    x16 = ulcorner(x14)
    x17 = ulcorner(x15)
    x18 = subgrid(x14, x6)
    x19 = subgrid(x15, x6)
    x20 = hmirror(x18)
    x21 = hmirror(x19)
    x22 = ofcolor(x20, FIVE)
    x23 = recolor(FIVE, x22)
    x24 = ofcolor(x21, FIVE)
    x25 = recolor(FIVE, x24)
    x26 = height(x23)
    x27 = height(x25)
    x28 = add(THREE, x26)
    x29 = add(THREE, x27)
    x30 = toivec(x28)
    x31 = toivec(x29)
    x32 = add(x16, x30)
    x33 = subtract(x17, x31)
    x34 = shift(x23, x32)
    x35 = shift(x25, x33)
    x36 = merge(x8)
    x37 = cover(x6, x36)
    x38 = paint(x37, x34)
    x39 = paint(x38, x35)
    x40 = rot270(x39)
    O = branch(x5, x39, x40)
    return O


def solve_264363fd(I):
    x1 = objects(I, F, F, T)
    x2 = argmin(x1, size)
    x3 = normalize(x2)
    x4 = height(x2)
    x5 = width(x2)
    x6 = equality(x4, FIVE)
    x7 = equality(x5, FIVE)
    x8 = astuple(x6, x7)
    x9 = add(UNITY, x8)
    x10 = invert(x9)
    x11 = center(x2)
    x12 = index(I, x11)
    x13 = branch(x6, UP, RIGHT)
    x14 = add(x13, x11)
    x15 = index(I, x14)
    x16 = astuple(x12, ORIGIN)
    x17 = initset(x16)
    x18 = cover(I, x2)
    x19 = mostcolor(x18)
    x20 = ofcolor(x18, x19)
    x21 = occurrences(x18, x17)
    x22 = objects(x18, F, F, T)
    x23 = rbind(occurrences, x17)
    x24 = rbind(subgrid, x18)
    x25 = compose(x23, x24)
    x26 = lbind(mapply, vfrontier)
    x27 = lbind(mapply, hfrontier)
    x28 = compose(x26, x25)
    x29 = compose(x27, x25)
    x30 = branch(x6, x28, x29)
    x31 = branch(x7, x29, x28)
    x32 = fork(combine, x30, x31)
    x33 = lbind(recolor, x15)
    x34 = compose(x33, x32)
    x35 = fork(paint, x24, x34)
    x36 = compose(asobject, x35)
    x37 = fork(shift, x36, ulcorner)
    x38 = mapply(x37, x22)
    x39 = paint(x18, x38)
    x40 = shift(x3, x10)
    x41 = lbind(shift, x40)
    x42 = mapply(x41, x21)
    x43 = paint(x39, x42)
    O = fill(x43, x19, x20)
    return O


def solve_7df24a62(I):
    x1 = height(I)
    x2 = width(I)
    x3 = ofcolor(I, ONE)
    x4 = ofcolor(I, FOUR)
    x5 = ulcorner(x3)
    x6 = subgrid(x3, I)
    x7 = rot90(x6)
    x8 = rot180(x6)
    x9 = rot270(x6)
    x10 = matcher(size, ZERO)
    x11 = rbind(ofcolor, ONE)
    x12 = compose(normalize, x11)
    x13 = rbind(ofcolor, FOUR)
    x14 = rbind(shift, x5)
    x15 = compose(x14, x13)
    x16 = lbind(subtract, x1)
    x17 = chain(increment, x16, height)
    x18 = lbind(subtract, x2)
    x19 = chain(increment, x18, width)
    x20 = rbind(interval, ONE)
    x21 = lbind(x20, ZERO)
    x22 = compose(x21, x17)
    x23 = compose(x21, x19)
    x24 = fork(product, x22, x23)
    x25 = rbind(shift, NEG_UNITY)
    x26 = lbind(lbind, shift)
    x27 = chain(x26, x25, x12)
    x28 = astuple(x6, x7)
    x29 = astuple(x8, x9)
    x30 = combine(x28, x29)
    x31 = apply(x15, x30)
    x32 = lbind(difference, x4)
    x33 = apply(x32, x31)
    x34 = apply(normalize, x31)
    x35 = apply(x24, x34)
    x36 = lbind(rbind, difference)
    x37 = apply(x26, x34)
    x38 = apply(x36, x33)
    x39 = papply(compose, x38, x37)
    x40 = lbind(compose, x10)
    x41 = apply(x40, x39)
    x42 = papply(sfilter, x35, x41)
    x43 = apply(x27, x30)
    x44 = mpapply(mapply, x43, x42)
    O = fill(I, ONE, x44)
    return O


def solve_f15e1fac(I):
    x1 = ofcolor(I, TWO)
    x2 = portrait(x1)
    x3 = branch(x2, identity, dmirror)
    x4 = x3(I)
    x5 = leftmost(x1)
    x6 = equality(x5, ZERO)
    x7 = branch(x6, identity, vmirror)
    x8 = x7(x4)
    x9 = ofcolor(x8, EIGHT)
    x10 = uppermost(x9)
    x11 = equality(x10, ZERO)
    x12 = branch(x11, identity, hmirror)
    x13 = x12(x8)
    x14 = ofcolor(x13, EIGHT)
    x15 = ofcolor(x13, TWO)
    x16 = rbind(shoot, DOWN)
    x17 = mapply(x16, x14)
    x18 = height(x13)
    x19 = apply(first, x15)
    x20 = insert(ZERO, x19)
    x21 = insert(x18, x19)
    x22 = apply(decrement, x21)
    x23 = order(x20, identity)
    x24 = order(x22, identity)
    x25 = size(x15)
    x26 = increment(x25)
    x27 = interval(ZERO, x26, ONE)
    x28 = apply(tojvec, x27)
    x29 = pair(x23, x24)
    x30 = lbind(sfilter, x17)
    x31 = compose(first, last)
    x32 = chain(decrement, first, first)
    x33 = fork(greater, x31, x32)
    x34 = chain(increment, last, first)
    x35 = fork(greater, x34, x31)
    x36 = fork(both, x33, x35)
    x37 = lbind(lbind, astuple)
    x38 = lbind(compose, x36)
    x39 = chain(x30, x38, x37)
    x40 = apply(x39, x29)
    x41 = papply(shift, x40, x28)
    x42 = merge(x41)
    x43 = fill(x13, EIGHT, x42)
    x44 = chain(x3, x7, x12)
    O = x44(x43)
    return O


def solve_234bbc79(I):
    x1 = rbind(objects, T)
    x2 = rbind(x1, F)
    x3 = rbind(x2, F)
    x4 = rbind(argmin, leftmost)
    x5 = compose(x4, x3)
    x6 = fork(remove, x5, x3)
    x7 = compose(x4, x6)
    x8 = compose(last, last)
    x9 = matcher(first, FIVE)
    x10 = rbind(sfilter, x9)
    x11 = fork(difference, identity, x10)
    x12 = rbind(argmin, x8)
    x13 = compose(x12, x10)
    x14 = compose(last, x13)
    x15 = rbind(add, RIGHT)
    x16 = compose(x14, x7)
    x17 = compose(x14, x5)
    x18 = fork(subtract, x16, x17)
    x19 = compose(invert, x18)
    x20 = compose(x15, x19)
    x21 = compose(mostcolor, x11)
    x22 = fork(astuple, x21, x14)
    x23 = fork(remove, x13, identity)
    x24 = fork(insert, x22, x23)
    x25 = compose(x24, x7)
    x26 = fork(cover, identity, x25)
    x27 = fork(shift, x25, x20)
    x28 = fork(paint, x26, x27)
    x29 = rbind(argmax, x8)
    x30 = chain(first, x29, x11)
    x31 = fork(recolor, x30, x10)
    x32 = fork(combine, x11, x31)
    x33 = compose(x32, x5)
    x34 = fork(paint, x28, x33)
    x35 = x34(I)
    x36 = x34(x35)
    x37 = palette(x36)
    x38 = contained(FIVE, x37)
    x39 = branch(x38, x34, identity)
    x40 = x39(x36)
    x41 = x3(x40)
    x42 = merge(x41)
    x43 = width(x42)
    x44 = astuple(THREE, x43)
    O = crop(x40, ORIGIN, x44)
    return O


def solve_22233c11(I):
    x1 = objects(I, T, F, T)
    x2 = objects(I, T, T, T)
    x3 = first(x1)
    x4 = recolor(EIGHT, x3)
    x5 = normalize(x4)
    x6 = totuple(x2)
    x7 = apply(width, x6)
    x8 = lbind(index, I)
    x9 = rbind(equality, ZERO)
    x10 = chain(flip, x9, x8)
    x11 = apply(urcorner, x6)
    x12 = apply(x10, x11)
    x13 = first(x7)
    x14 = halve(x13)
    x15 = pair(x6, x12)
    x16 = sfilter(x15, last)
    x17 = apply(first, x16)
    x18 = apply(flip, x12)
    x19 = pair(x6, x18)
    x20 = sfilter(x19, last)
    x21 = apply(first, x20)
    x22 = apply(urcorner, x21)
    x23 = invert(x14)
    x24 = astuple(x23, ONE)
    x25 = lbind(add, x24)
    x26 = apply(x25, x22)
    x27 = lbind(shift, x5)
    x28 = mapply(x27, x26)
    x29 = apply(llcorner, x21)
    x30 = astuple(ONE, x23)
    x31 = lbind(add, x30)
    x32 = apply(x31, x29)
    x33 = mapply(x27, x32)
    x34 = apply(ulcorner, x17)
    x35 = astuple(x23, x23)
    x36 = lbind(add, x35)
    x37 = apply(x36, x34)
    x38 = mapply(x27, x37)
    x39 = apply(lrcorner, x17)
    x40 = lbind(add, UNITY)
    x41 = apply(x40, x39)
    x42 = mapply(x27, x41)
    x43 = paint(I, x28)
    x44 = paint(x43, x33)
    x45 = paint(x44, x38)
    O = paint(x45, x42)
    return O


def solve_2dd70a9a(I):
    x1 = ofcolor(I, TWO)
    x2 = ofcolor(I, THREE)
    x3 = vline(x1)
    x4 = vline(x2)
    x5 = center(x1)
    x6 = branch(x4, uppermost, rightmost)
    x7 = x6(x1)
    x8 = x6(x2)
    x9 = greater(x7, x8)
    x10 = both(x4, x9)
    x11 = branch(x10, lowermost, uppermost)
    x12 = x11(x2)
    x13 = branch(x4, leftmost, rightmost)
    x14 = x13(x2)
    x15 = astuple(x12, x14)
    x16 = other(x2, x15)
    x17 = subtract(x15, x16)
    x18 = shoot(x15, x17)
    x19 = underfill(I, ONE, x18)
    x20 = objects(x19, T, F, F)
    x21 = colorfilter(x20, ONE)
    x22 = rbind(adjacent, x2)
    x23 = sfilter(x21, x22)
    x24 = difference(x21, x23)
    x25 = merge(x24)
    x26 = cover(x19, x25)
    x27 = shoot(x5, DOWN)
    x28 = shoot(x5, UP)
    x29 = shoot(x5, LEFT)
    x30 = shoot(x5, RIGHT)
    x31 = combine(x27, x28)
    x32 = combine(x29, x30)
    x33 = branch(x3, x31, x32)
    x34 = ofcolor(x26, ONE)
    x35 = initset(x15)
    x36 = rbind(manhattan, x35)
    x37 = compose(x36, initset)
    x38 = argmax(x34, x37)
    x39 = initset(x38)
    x40 = gravitate(x39, x33)
    x41 = crement(x40)
    x42 = add(x38, x41)
    x43 = connect(x38, x42)
    x44 = fill(x26, ONE, x43)
    x45 = connect(x42, x5)
    x46 = underfill(x44, ONE, x45)
    O = replace(x46, ONE, THREE)
    return O


def solve_a64e4611(I):
    x1 = asindices(I)
    x2 = fork(product, identity, identity)
    x3 = lbind(canvas, ZERO)
    x4 = compose(asobject, x3)
    x5 = fork(multiply, first, last)
    x6 = compose(positive, size)
    x7 = lbind(lbind, shift)
    x8 = rbind(fork, x5)
    x9 = lbind(x8, multiply)
    x10 = lbind(chain, x6)
    x11 = rbind(x10, x4)
    x12 = lbind(lbind, occurrences)
    x13 = chain(x9, x11, x12)
    x14 = compose(x2, first)
    x15 = compose(x13, last)
    x16 = fork(argmax, x14, x15)
    x17 = chain(x7, x4, x16)
    x18 = compose(x4, x16)
    x19 = fork(occurrences, last, x18)
    x20 = fork(mapply, x17, x19)
    x21 = multiply(TWO, SIX)
    x22 = interval(THREE, x21, ONE)
    x23 = astuple(x22, I)
    x24 = x20(x23)
    x25 = fill(I, THREE, x24)
    x26 = interval(THREE, TEN, ONE)
    x27 = astuple(x26, x25)
    x28 = x20(x27)
    x29 = fill(x25, THREE, x28)
    x30 = astuple(x26, x29)
    x31 = x20(x30)
    x32 = fill(x29, THREE, x31)
    x33 = rbind(toobject, x32)
    x34 = rbind(colorcount, THREE)
    x35 = chain(x34, x33, neighbors)
    x36 = matcher(x35, EIGHT)
    x37 = sfilter(x1, x36)
    x38 = fill(I, THREE, x37)
    x39 = ofcolor(x38, ZERO)
    x40 = rbind(bordering, x38)
    x41 = compose(x40, initset)
    x42 = lbind(contained, THREE)
    x43 = rbind(toobject, x38)
    x44 = chain(x42, palette, x43)
    x45 = compose(x44, dneighbors)
    x46 = fork(both, x45, x41)
    x47 = sfilter(x39, x46)
    O = fill(x38, THREE, x47)
    return O


def solve_7837ac64(I):
    x1 = compress(I)
    x2 = lbind(colorcount, x1)
    x3 = palette(x1)
    x4 = order(x3, x2)
    x5 = remove(ZERO, x4)
    x6 = last(x5)
    x7 = replace(x1, x6, ZERO)
    x8 = objects(x7, T, F, T)
    x9 = merge(x8)
    x10 = subgrid(x9, x7)
    x11 = index(x10, ORIGIN)
    x12 = vmirror(x10)
    x13 = index(x12, ORIGIN)
    x14 = hmirror(x10)
    x15 = index(x14, ORIGIN)
    x16 = vmirror(x14)
    x17 = index(x16, ORIGIN)
    x18 = width(x10)
    x19 = subtract(x18, FOUR)
    x20 = divide(x19, THREE)
    x21 = increment(x20)
    x22 = tojvec(x21)
    x23 = toivec(x21)
    x24 = index(x10, x22)
    x25 = index(x12, x22)
    x26 = index(x14, x22)
    x27 = index(x16, x22)
    x28 = index(x10, x23)
    x29 = index(x14, x23)
    x30 = index(x12, x23)
    x31 = equality(x24, x25)
    x32 = equality(x26, x27)
    x33 = equality(x28, x29)
    x34 = equality(x29, x30)
    x35 = branch(x31, x24, ZERO)
    x36 = branch(x32, x26, ZERO)
    x37 = branch(x33, x28, ZERO)
    x38 = branch(x34, x29, ZERO)
    x39 = astuple(x11, x35)
    x40 = repeat(x13, ONE)
    x41 = combine(x39, x40)
    x42 = astuple(x37, ZERO)
    x43 = repeat(x38, ONE)
    x44 = combine(x42, x43)
    x45 = astuple(x15, x36)
    x46 = repeat(x17, ONE)
    x47 = combine(x45, x46)
    x48 = astuple(x41, x44)
    x49 = repeat(x47, ONE)
    O = vconcat(x48, x49)
    return O


def solve_a8c38be5(I):
    x1 = objects(I, T, F, T)
    x2 = colorfilter(x1, FIVE)
    x3 = argmax(x2, size)
    x4 = subgrid(x3, I)
    x5 = difference(x1, x2)
    x6 = fork(equality, height, width)
    x7 = fork(greater, width, height)
    x8 = sfilter(x5, x6)
    x9 = sfilter(x5, portrait)
    x10 = sfilter(x5, x7)
    x11 = rbind(subgrid, I)
    x12 = chain(center, delta, normalize)
    x13 = order(x8, x12)
    x14 = apply(x11, x13)
    x15 = order(x9, x12)
    x16 = apply(x11, x15)
    x17 = order(x10, x12)
    x18 = apply(x11, x17)
    x19 = first(x14)
    x20 = remove(x19, x14)
    x21 = first(x20)
    x22 = remove(x21, x20)
    x23 = first(x22)
    x24 = last(x14)
    x25 = last(x16)
    x26 = first(x16)
    x27 = last(x18)
    x28 = first(x18)
    x29 = astuple(ONE, TWO)
    x30 = astuple(ONE, THREE)
    x31 = astuple(NINE, ONE)
    x32 = canvas(FIVE, x29)
    x33 = canvas(FIVE, x30)
    x34 = canvas(FIVE, x31)
    x35 = vconcat(x24, x32)
    x36 = vconcat(x35, x25)
    x37 = vconcat(x36, x32)
    x38 = vconcat(x37, x21)
    x39 = vconcat(x27, x33)
    x40 = vconcat(x39, x4)
    x41 = vconcat(x40, x33)
    x42 = vconcat(x41, x28)
    x43 = vconcat(x23, x32)
    x44 = vconcat(x43, x26)
    x45 = vconcat(x44, x32)
    x46 = vconcat(x45, x19)
    x47 = hconcat(x38, x34)
    x48 = hconcat(x47, x42)
    x49 = hconcat(x48, x34)
    O = hconcat(x49, x46)
    return O


def solve_b775ac94(I):
    x1 = objects(I, F, T, T)
    x2 = lbind(rbind, equality)
    x3 = rbind(compose, first)
    x4 = chain(x3, x2, mostcolor)
    x5 = fork(sfilter, identity, x4)
    x6 = fork(difference, identity, x5)
    x7 = lbind(rbind, adjacent)
    x8 = rbind(compose, initset)
    x9 = chain(x8, x7, x6)
    x10 = fork(extract, x5, x9)
    x11 = fork(insert, x10, x6)
    x12 = lbind(recolor, ZERO)
    x13 = chain(x12, delta, x11)
    x14 = fork(combine, x11, x13)
    x15 = fork(position, x5, x6)
    x16 = chain(toivec, first, x15)
    x17 = chain(tojvec, last, x15)
    x18 = fork(multiply, shape, x16)
    x19 = fork(multiply, shape, x17)
    x20 = fork(multiply, shape, x15)
    x21 = fork(shift, hmirror, x18)
    x22 = fork(shift, vmirror, x19)
    x23 = compose(hmirror, vmirror)
    x24 = fork(shift, x23, x20)
    x25 = lbind(compose, x5)
    x26 = x25(x21)
    x27 = x25(x22)
    x28 = x25(x24)
    x29 = compose(crement, invert)
    x30 = lbind(compose, x29)
    x31 = x30(x16)
    x32 = x30(x17)
    x33 = x30(x15)
    x34 = fork(shift, x26, x31)
    x35 = fork(shift, x27, x32)
    x36 = fork(shift, x28, x33)
    x37 = lbind(index, I)
    x38 = lbind(compose, toindices)
    x39 = x38(x14)
    x40 = x38(x34)
    x41 = x38(x35)
    x42 = x38(x36)
    x43 = fork(intersection, x39, x40)
    x44 = fork(intersection, x39, x41)
    x45 = fork(intersection, x39, x42)
    x46 = chain(x37, first, x43)
    x47 = chain(x37, first, x44)
    x48 = chain(x37, first, x45)
    x49 = fork(recolor, x46, x34)
    x50 = fork(recolor, x47, x35)
    x51 = fork(recolor, x48, x36)
    x52 = mapply(x49, x1)
    x53 = mapply(x50, x1)
    x54 = mapply(x51, x1)
    x55 = paint(I, x52)
    x56 = paint(x55, x53)
    O = paint(x56, x54)
    return O


def solve_97a05b5b(I):
    x1 = objects(I, F, T, T)
    x2 = argmax(x1, size)
    x3 = subgrid(x2, I)
    x4 = rbind(greater, ONE)
    x5 = compose(x4, numcolors)
    x6 = sfilter(x1, x5)
    x7 = lbind(rbind, subtract)
    x8 = switch(x3, TWO, ZERO)
    x9 = lbind(occurrences, x8)
    x10 = lbind(lbind, shift)
    x11 = compose(x7, ulcorner)
    x12 = matcher(first, TWO)
    x13 = compose(flip, x12)
    x14 = rbind(sfilter, x12)
    x15 = rbind(sfilter, x13)
    x16 = lbind(recolor, ZERO)
    x17 = compose(x16, x15)
    x18 = fork(combine, x17, x14)
    x19 = chain(x11, x18, normalize)
    x20 = objects(x8, T, T, T)
    x21 = apply(toindices, x20)
    x22 = chain(x9, x18, normalize)
    x23 = rbind(colorcount, TWO)
    x24 = lbind(sfilter, x21)
    x25 = chain(size, first, x24)
    x26 = compose(positive, size)
    x27 = lbind(lbind, contained)
    x28 = chain(x26, x24, x27)
    x29 = compose(x25, x27)
    x30 = rbind(sfilter, x28)
    x31 = compose(x30, x22)
    x32 = lbind(rbind, equality)
    x33 = rbind(compose, x29)
    x34 = chain(x33, x32, x23)
    x35 = fork(sfilter, x31, x34)
    x36 = fork(apply, x19, x35)
    x37 = compose(x10, normalize)
    x38 = fork(mapply, x37, x36)
    x39 = astuple(cmirror, dmirror)
    x40 = astuple(hmirror, vmirror)
    x41 = combine(x39, x40)
    x42 = product(x41, x41)
    x43 = fork(compose, first, last)
    x44 = apply(x43, x42)
    x45 = lbind(rapply, x44)
    x46 = mapply(x45, x6)
    x47 = mapply(x38, x46)
    x48 = paint(x3, x47)
    x49 = palette(x47)
    x50 = lbind(remove, TWO)
    x51 = x50(x49)
    x52 = chain(first, x50, palette)
    x53 = rbind(contained, x51)
    x54 = chain(flip, x53, x52)
    x55 = sfilter(x6, x54)
    x56 = fork(apply, x19, x22)
    x57 = fork(mapply, x37, x56)
    x58 = mapply(x45, x55)
    x59 = mapply(x57, x58)
    O = paint(x48, x59)
    return O


def solve_3e980e27(I):
    x1 = objects(I, F, T, T)
    x2 = lbind(contained, TWO)
    x3 = compose(x2, palette)
    x4 = lbind(contained, THREE)
    x5 = compose(x4, palette)
    x6 = sfilter(x1, x3)
    x7 = sfilter(x1, x5)
    x8 = compose(positive, size)
    x9 = x8(x7)
    x10 = x8(x6)
    x11 = both(x9, x10)
    x12 = repeat(ZERO, ZERO)
    x13 = rbind(subgrid, I)
    x14 = chain(asobject, vmirror, x13)
    x15 = matcher(first, ZERO)
    x16 = compose(flip, x15)
    x17 = lbind(matcher, first)
    x18 = lbind(rbind, add)
    x19 = rbind(argmax, size)
    x20 = chain(x18, invert, ulcorner)
    x21 = lbind(lbind, shift)
    x22 = lbind(occurrences, I)
    x23 = rbind(astuple, ORIGIN)
    x24 = chain(x22, initset, x23)
    x25 = branch(x9, x7, x6)
    x26 = x19(x25)
    x27 = branch(x9, identity, x14)
    x28 = branch(x9, THREE, TWO)
    x29 = x27(x26)
    x30 = sfilter(x29, x16)
    x31 = x24(x28)
    x32 = x17(x28)
    x33 = sfilter(x26, x32)
    x34 = center(x33)
    x35 = remove(x34, x31)
    x36 = normalize(x30)
    x37 = sfilter(x36, x32)
    x38 = x20(x37)
    x39 = apply(x38, x35)
    x40 = x21(x36)
    x41 = mapply(x40, x39)
    x42 = paint(I, x41)
    x43 = branch(x10, x6, x7)
    x44 = x19(x43)
    x45 = branch(x9, x14, identity)
    x46 = branch(x10, TWO, THREE)
    x47 = x45(x44)
    x48 = sfilter(x47, x16)
    x49 = x24(x46)
    x50 = x17(x46)
    x51 = sfilter(x44, x50)
    x52 = center(x51)
    x53 = remove(x52, x49)
    x54 = normalize(x48)
    x55 = sfilter(x54, x50)
    x56 = x20(x55)
    x57 = apply(x56, x53)
    x58 = branch(x11, x57, x12)
    x59 = x21(x54)
    x60 = mapply(x59, x58)
    O = paint(x42, x60)
    return O



    def solve_626c0bcc(I):
        x1 = connect(ORIGIN, RIGHT)
        x2 = connect(DOWN, UNITY)
        x3 = insert(UNITY, x1)
        x4 = insert(ORIGIN, x2)
        x5 = insert(RIGHT, x2)
        x6 = astuple(THREE, x3)
        x7 = astuple(FOUR, x4)
        x8 = astuple(TWO, x5)
        x9 = initset(x6)
        x10 = insert(x7, x9)
        x11 = insert(x8, x10)
        x12 = compose(positive, size)
        x13 = lbind(compose, x12)
        x14 = fork(recolor, first, last)
        x15 = lbind(rbind, difference)
        x16 = lbind(rbind, remove)
        x17 = lbind(recolor, EIGHT)
        x18 = rbind(compose, x17)
        x19 = lbind(lbind, occurrences)
        x20 = compose(x18, x19)
        x21 = lbind(lbind, shift)
        x22 = lbind(fork, apply)
        x23 = lbind(x22, x21)
        x24 = lbind(chain, merge)
        x25 = rbind(x24, last)
        x26 = rbind(apply, x11)
        x27 = rbind(x24, merge)
        x28 = lbind(chain, x13)
        x29 = lbind(x28, x15)
        x30 = lbind(fork, sfilter)
        x31 = lbind(x30, identity)
        x32 = lbind(compose, merge)
        x33 = rbind(chain, last)
        x34 = lbind(compose, x14)
        x35 = compose(x23, x20)
        x36 = chain(x16, x26, x25)
        x37 = chain(x29, x27, x36)
        x38 = chain(x32, x31, x37)
        x39 = lbind(fork, astuple)
        x40 = lbind(x39, first)
        x41 = rbind(mapply, x11)
        x42 = chain(x41, x34, x40)
        x43 = compose(x38, x35)
        x44 = fork(x33, x43, x35)
        x45 = compose(x42, x44)
        # so x46 basically does the following: for a grid, fill yellow for each area of matching shape that is light blue 
        # and for which at least one pixel can't be covered by any of the other two shapes (that is red or green)  
        # and do the same for green and red. Repeat process (4 times); at each step light blue decreses. 
        # Then at the end, the remaining light blues are the squares
        x46 = fork(paint, identity, x45)
        x47 = power(x46, FOUR)
        x48 = x47(I)
        O = replace(x48, EIGHT, ONE)
        return O


