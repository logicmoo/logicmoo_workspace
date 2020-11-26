% ===================================================================
% File 'parser_all.pl'
% Purpose: English to KIF conversions from SWI-Prolog
% This implementation is incomplete
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_all.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================

:- module(parser_frames, []).

:- ensure_loaded(nl_pipeline).

% =================================================================
% %%%%%%%%%%%%%%%%%%%%%%% Frames %%%%%%%%%%%%%%%%%%%%%%%
% =================================================================
f_setprops(OO, List):- is_list(List), !, maplist(f_setprops(OO), List).
f_setprops(OO, K=V):- fr_set(OO, K, V).

make_frame(Frame):-
  put_attr(Frame, frame, UDT),
  put_attr(UDT, frame, _),
  f_setprops(Frame,
  [target = subject,
   ftype = frame,
   preconds = true,
   postconds = true,
   lf = postconds,
   subject = _VAR1,
   object = _VAR2,
   oblique = _VAR3,
   event = _VAR4,
   time = now]).

frame:attr_unify_hook(Frame, Value):- % dumpST, !,
  fmt(frame:attr_unify_hook(Frame, Value)), !, fail.

is_frame(Frame):- attvar(Frame), get_attr(Frame, frame, _).
  %fr_get(Frame, ftype, frame).

frame_created(Frame):- \+ is_frame(Frame), ensure_frame(Frame).

% =================================================================
ensure_frame(Frame, L, L):- ensure_frame(Frame).
%
ensure_frame(Frame):- assertion(var(Frame)), is_frame(Frame)->true;make_frame(Frame).

% =================================================================
frame_copy_into(S, T, L, L):- frame_copy_into(S, T).
%
frame_copy_into(S, T):- frame_created(T), !, frame_copy_into(S, T).
frame_copy_into(S, T):- frame_created(S), !, frame_copy_into(S, T).
frame_copy_into(S, T):-
  frame_get(S, postconds, Conds), push_lf(T, postconds, Conds),
  frame_get(S, preconds, PreConds), push_lf(T, preconds, PreConds).

negate_frame(Frame, ~Frame).
% =================================================================
push_lf(Frame, Value, L, L):- push_lf(Frame, Value).
%
push_lf(Frame, Value):- frame_created(Frame), !, push_lf(Frame, Value).
push_lf(Frame, Value):- is_frame(Value), !, frame_copy_into(Value, Frame).
push_lf(Frame, Value):- push_lf(Frame, $lf, Value).

% =================================================================
push_lf(Frame, Place, Value, L, L):- push_lf(Frame, Place, Value).
%
push_lf(Frame, Place, Value):- frame_resolve(f_push1, Frame, [Place, Value]).

f_push1(Frame, $lf  , Value):- !, frame_get(Frame, lf, NewPlace), f_push1(Frame, NewPlace, Value).
f_push1(Frame, Place, Value):- frame_get(Frame, Place, Before), conjoin(Before, Value, New), frame_set(Frame, Place, New).

%resolve_value(_Frame, Value, NewValue):- \+ compound(Value), !, NewValue=Value.
resolve_value(Frame, Value, NewValue):- compound(Value),
   sub_term(Sub, Value), compound(Sub), (Sub= $(Var)),
   fr_get(Frame, Var, LF)-> subst(Value, Sub, LF, M), \+ cyclic_term(M), !,
   resolve_value_exceptfor(Frame, M, NewValue, [Var]).
resolve_value(_Frame, Value, Value).

%resolve_value_exceptfor(_Frame, Value, NewValue, _):- \+ compound(Value), !, NewValue=Value.
resolve_value_exceptfor( Frame, Value, NewValue, Vars):- compound(Value),
   sub_term(Sub, Value), compound(Sub), (Sub= $(Var)),
   \+ member(Var, Vars),
   fr_get(Frame, Var, LF)-> subst(Value, Sub, LF, M), \+ cyclic_term(M), !,
   resolve_value_exceptfor(Frame, M, NewValue, [Var|Vars]).
resolve_value_exceptfor(_, Value, Value, _).

resolve_target(Frame, Value, NewTLF):- !, resolve_value(Frame, Value, NewTLF), !.
resolve_target(Frame, Value, NewTLF):-
   subst(Value, $target, NewT, M), !,
   subst(M, $lf, NewLF, NewTLF), !,
   Value\=@=NewTLF, !,
   fr_get(Frame, target, T),
   fr_get(Frame, lf, LF),
   NewT=T, NewLF=LF.

frame_resolve(Pred, Frame, PlaceValue):- frame_created(Frame), !, frame_resolve(Pred, Frame, PlaceValue).
frame_resolve(Pred, Frame, PlaceValue):- (resolve_target(Frame, PlaceValue, NewPlaceValue) -> PlaceValue\=@=NewPlaceValue), !,
  frame_resolve(Pred, Frame, NewPlaceValue).
frame_resolve(Pred, Frame, PlaceValue):- apply(Pred, [Frame|PlaceValue]).

fr_set(X, Y, Z):- oo_get(X, frame, F), oo_set(F, Y, Z).
fr_get(X, Y, Z):- oo_get(X, frame, F), oo_get(F, Y, Z).

% =================================================================
frame_get(Frame, Place, Value, L, L):- frame_get(Frame, Place, Value).
%
frame_get(Frame, Place, Value):- frame_resolve(fr_get, Frame, [Place, Value]).
% =================================================================
frame_set(Frame, Place, Value, L, L):- frame_set(Frame, Place, Value).
%
frame_set(Frame, Place, Value):- frame_resolve(fr_set, Frame, [Place, Value]).

:- fixup_exports.
